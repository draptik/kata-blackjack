module BlackJack.Domain

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

type Deck = Card list
type Hand = Card list
type Score = Score of int

type HandStatus = 
    | CardsDealt
    | BlackJack 
    | Busted of Score 
    | Stayed of Score 

type Dealer = { Hand: Hand; HandStatus: HandStatus }

type PlayerId = PlayerId of int
type Player = { Hand: Hand; HandStatus: HandStatus; Id: PlayerId }

// extended list comprehension
let fullDeck = [
    for suit in allSuits do
    for rank in allRanks do
    yield { Suit = suit; Rank = rank } ]

let createDeck : Deck =
    let shuffle deck = 
        let random = System.Random()
        deck |> List.sortBy (fun x -> random.Next())
    fullDeck |> shuffle 

let drawCard deck =
    match deck with
    | [] -> Error ErrorDrawCard
    | topCard::restOfDeck -> Ok (topCard, restOfDeck)

let drawCardToHand (deck, hand) =
    match drawCard deck with
    | Error -> Error ErrorDrawCardToHand
    | Ok (card, modifiedDeck) -> Ok (modifiedDeck, card :: hand)

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
        let hand = [firstCard; secondCard]

        return {Hand = hand; Id = id; HandStatus = CardsDealt}, deck
    }

let trySetupDealer drawCardFcn deck =
    let maybe = MaybeBuilder ()

    maybe {
        let! firstCard, deck = drawCardFcn deck
        let! secondCard, deck = drawCardFcn deck
        let hand = [firstCard; secondCard]

        return {Hand = hand; HandStatus = CardsDealt}, deck
    }

type NumberOfPlayers = NumberOfPlayers of int

// TODO: make private (use try wrapper below)
let initializePlayers numberOfPlayers initialDeck =
    let (NumberOfPlayers n) = numberOfPlayers
    let playerIds = [1..n] |> List.map PlayerId
    List.fold 
        (fun (currentPlayers, currentDeck) playerId ->
            match trySetupPlayer drawCard playerId currentDeck with
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
        && deckAfterInitializingAllPlayers.Length = initialDeck.Length - (requestedNumberOfPlayers * numberOfCardsDealtToPlayer)

    if isValid then Ok (initializedPlayers, deckAfterInitializingAllPlayers)
    else Error ErrorInitializingPlayers

let calcScore (hand: Hand) =
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
    
    let getHandValue hand =
        let softValue = List.fold (fun accumulator element -> accumulator + getCardValueSoft element) 0 hand
        
        if softValue <= 21 then softValue
        else
            List.fold (fun accumulator element ->
                let newValue = accumulator + getCardValueSoft element
                if newValue < 21 then newValue
                else accumulator + getCardValueHard element)
                0
                (List.sort hand)
                
    getHandValue hand |> Score

let getStatus (handStatus, hand) =
    let score = calcScore hand
    match handStatus, score with
    | handStatus, score when handStatus = CardsDealt && score = Score 21 -> BlackJack
    | _, score when score <= Score 21 -> Stayed (score)
    | _, score -> Busted (score)

type DealerPlayResult =
    | ErrorDuringPlay
    | DealerBusted of (Score * Hand * Deck)
    | DealerStayed of (Score * Hand * Deck)

type DealerPlayState = {
    Hand: Hand
    Deck: Deck
}

let rec dealerPlays dealerPlayState =
    let dealerScore = calcScore dealerPlayState.Hand
    match dealerScore with
    | score when score > Score 21 -> DealerBusted (dealerScore, dealerPlayState.Hand, dealerPlayState.Deck)
    | score when score >= Score 17 -> DealerStayed (dealerScore, dealerPlayState.Hand, dealerPlayState.Deck)
    | _ ->
        match drawCard dealerPlayState.Deck with
        | Error -> ErrorDuringPlay
        | Ok (card, deck) -> dealerPlays { Hand = card::dealerPlayState.Hand; Deck = deck }

let getPotentialWinningPlayers players =
    match players with
    | [] -> None
    | players ->
        let isNotBusted handStatus =
            match handStatus with
            | Busted _ -> false
            | _ -> true
            
        players
        |> List.filter (fun p -> p.HandStatus |> isNotBusted)
        |> List.groupBy (fun p -> calcScore p.Hand) // creates a tuple (Score * Player list)
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
        match dealer.HandStatus with
        | Busted -> Nobody
        | _ -> Dealer dealer
    | Some players ->
        (* 
            All winning players have the same Score. 
            We take the first player (players.Head) for comparison with the dealer 
        *)
        match players.Head.HandStatus, dealer.HandStatus with
        | Stayed playerScore, Stayed dealerScore ->
            match playerScore, dealerScore with
            | pScore, dScore when pScore = dScore -> Nobody
            | pScore, dScore when pScore > dScore -> Players players
            | _ -> Dealer dealer
        | Stayed, Busted -> Players players
        | Busted, Stayed -> Dealer dealer
        | _ -> Nobody
