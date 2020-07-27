module BlackJack.Domain

type Undefined = exn

type AppError =
    | ErrorInitializingPlayers
    | ErrorDuringDealerPlay
    | ErrorPlayerPlayingInvalidHandState
    | ErrorDrawCard
    | ErrorDrawCardToHand
    | ErrorAskingForNumberOfPlayers
    | ErrorDealerCanOnlyBeDealtCardsAfterPlayersHaveBeenDealt
    | ErrorPlayerLoopsCanOlyStartAfterDealerHasBeenDealt
    | ErrorDealerTurnCanOnlyStartAfterAllPlayersFinished
    | ErrorWinnerCanOlyBeDeterminedAfterDealerIsFinished

type Suit = Hearts | Spades | Diamonds | Clubs
type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type Card = { Rank: Rank; Suit: Suit }

let allRanks = [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]
let allSuits = [ Diamonds; Hearts; Clubs; Spades ]

type HandCards = HandCards of Card list
type Deck = DeckCards of Card list

type Score = Score of int

type HandStatus = 
    | CardsDealt
    | BlackJack 
    | Busted of Score 
    | Stayed of Score 

type Hand = {
    Cards: HandCards
    Status: HandStatus
}

type Dealer = { Hand: Hand }

type PlayerId = PlayerId of int
type Player = { Hand: Hand; Id: PlayerId }

let getCards hand = match hand.Cards with | HandCards cards -> cards

let deck2cards deck =
    let (DeckCards cards) = deck
    cards
    
let cards2deck (cards: Card list) : Deck =
    cards |> DeckCards 


let fullDeck = [
    for suit in allSuits do
    for rank in allRanks do
    yield { Suit = suit; Rank = rank } ]

let createDeck : Deck =
    let shuffle deck = 
        let random = System.Random()
        deck |> List.sortBy (fun x -> random.Next())
    fullDeck |> shuffle |> DeckCards

let drawCardFromDeck (deck:Deck) : Result<(Card*Deck),AppError> =
    match deck with
    | DeckCards [] -> Error ErrorDrawCard
    | DeckCards (topCard::restOfDeck) -> Ok (topCard, (DeckCards restOfDeck))

let drawCardToHand ((deck:Deck), hand) =
    match drawCardFromDeck deck with
    | Error -> Error ErrorDrawCardToHand
    | Ok (card, modifiedDeck) -> 
        let handCards = card :: (hand |> getCards)
        let newHand = { Cards = HandCards handCards; Status = hand.Status }
        Ok (modifiedDeck, newHand)

type MaybeBuilder() =
    member this.Bind(input, func) =
        match input with
        | Error e -> Error e
        | Ok value -> func value
    
    member this.Return value =
        Ok value

let trySetupPlayer drawCardFcn id deck =
    let maybe = MaybeBuilder ()

    maybe {
        let! firstCard, deck = drawCardFcn deck
        let! secondCard, deck = drawCardFcn deck
        let handCards = HandCards [firstCard; secondCard]
        let hand = { Cards = handCards; Status = CardsDealt }
        return {Hand = hand; Id = id}, deck
    }

let trySetupDealer drawCardFcn deck =
    let maybe = MaybeBuilder ()

    maybe {
        let! firstCard, deck = drawCardFcn deck
        let! secondCard, deck = drawCardFcn deck
        let handCards = HandCards [firstCard; secondCard]
        let hand = { Cards = handCards; Status = CardsDealt }
        return {Hand = hand}, deck
    }

type NumberOfPlayers = NumberOfPlayers of int

// TODO: make private (use try wrapper below)
let initializePlayers numberOfPlayers initialDeck =
    let (NumberOfPlayers n) = numberOfPlayers
    let playerIds = [1..n] |> List.map PlayerId
    List.fold 
        (fun (currentPlayers, currentDeck) playerId ->
            match trySetupPlayer drawCardFromDeck playerId currentDeck with
            | Ok (player, modifiedDeck) -> (currentPlayers@[player], modifiedDeck)
            | Error -> (currentPlayers, currentDeck))
        ([], initialDeck)
        playerIds
    
let tryInitializePlayers numberOfPlayers initialDeck =
    let (initializedPlayers, deckAfterInitializingAllPlayers) = initializePlayers numberOfPlayers initialDeck
    let (NumberOfPlayers requestedNumberOfPlayers) = numberOfPlayers
    let numberOfCardsDealtToPlayer = 2
    let isValid =
        initializedPlayers.Length = requestedNumberOfPlayers
        && (deckAfterInitializingAllPlayers |> deck2cards).Length =
            (initialDeck |> deck2cards).Length - (requestedNumberOfPlayers * numberOfCardsDealtToPlayer)

    if isValid then Ok (initializedPlayers, deckAfterInitializingAllPlayers)
    else Error ErrorInitializingPlayers

let calcScore (handCards: HandCards) =
    let getCardValueSoft card =
        match card.Rank with
        | Two -> 2
        | Three -> 3
        | Four -> 4
        | Five -> 5
        | Six -> 6
        | Seven -> 7
        | Eight -> 8
        | Nine -> 9
        | Ten | Jack | Queen | King -> 10
        | Ace -> 11

    let getCardValueHard card =
        match card.Rank with
        | Ace -> 1
        | _ -> getCardValueSoft card
    
    let getHandValue handCards =
        let (HandCards cards) = handCards
        let softValue = List.fold (fun accumulator element -> accumulator + getCardValueSoft element) 0 cards
        
        if softValue <= 21 then softValue
        else
            List.fold (fun accumulator element ->
                let newValue = accumulator + getCardValueSoft element
                if newValue < 21 then newValue
                else accumulator + getCardValueHard element)
                0
                (List.sort cards)
                
    getHandValue handCards |> Score

let getStatus (hand: Hand) =
    let score = calcScore hand.Cards
    match hand.Status, score with
    | handStatus, score when handStatus = CardsDealt && score = Score 21 -> BlackJack
    | _, score when score <= Score 21 -> Stayed (score)
    | _, score -> Busted (score)

type DealerPlayResult =
    | ErrorDuringPlay
    | DealerBusted of (Score * HandCards * Deck)
    | DealerStayed of (Score * HandCards * Deck)

type DealerPlayState = {
    Cards: HandCards
    Deck: Deck
}

let rec dealerPlays (dealerPlayState:DealerPlayState) =
    let dealerScore = calcScore dealerPlayState.Cards
    match dealerScore with
    | score when score > Score 21 -> DealerBusted (dealerScore, dealerPlayState.Cards, dealerPlayState.Deck)
    | score when score >= Score 17 -> DealerStayed (dealerScore, dealerPlayState.Cards, dealerPlayState.Deck)
    | _ ->
        match drawCardFromDeck dealerPlayState.Deck with
        | Error -> ErrorDuringPlay
        | Ok (card, deck) -> 
            let (HandCards prevCards) = dealerPlayState.Cards 
            let newHandCards = card::prevCards |> HandCards
            dealerPlays { Cards = newHandCards; Deck = deck }

let getPotentialWinningPlayers (players: Player list) =
    match players with
    | [] -> None
    | players ->
        let isNotBusted hand =
            match hand.Status with
            | Busted _ -> false
            | _ -> true
            
        players
        |> List.filter (fun p -> p.Hand |> isNotBusted)
        |> List.groupBy (fun p -> calcScore p.Hand.Cards) // creates a tuple (Score * Player list)
        |> List.sort // sort by score
        |> List.rev // ensure highest score is first
        |> List.head // gets the first element in the list
        |> snd // gets the second part of the tuple (all potential winning players); ("fst" is the score here)
        |> Some

type Winner =
    | Players of Player list
    | Dealer of Dealer
    | Nobody

let determinWinner players (dealer: Dealer) =
    let winningPlayersOpt = players |> getPotentialWinningPlayers 
    match winningPlayersOpt with
    | None ->
        match dealer.Hand.Status with
        | Busted -> Nobody
        | _ -> Dealer dealer
    | Some players ->
        (* 
            All winning players have the same Score. 
            We take the first player (players.Head) for comparison with the dealer 
        *)
        match players.Head.Hand.Status, dealer.Hand.Status with
        | Stayed playerScore, Stayed dealerScore ->
            match playerScore, dealerScore with
            | pScore, dScore when pScore = dScore -> Nobody
            | pScore, dScore when pScore > dScore -> Players players
            | _ -> Dealer dealer
        | Stayed, Busted -> Players players
        | Busted, Stayed -> Dealer dealer
        | _ -> Nobody
