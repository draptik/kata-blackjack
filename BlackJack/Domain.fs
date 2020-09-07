module BlackJack.Domain

type Undefined = exn

type AppError =
    | ErrorInitializingPlayers
    | ErrorDuringDealerPlay
    | ErrorPlayerPlayingInvalidHandState
    | ErrorDrawCard
    | ErrorDrawCardToHand
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

type Dealer = { Hand: HandCards }

type DealerType =
    | House of Dealer
    | Busted of Dealer
    
type PlayerId = PlayerId of int
type Player = { Hand: HandCards; Id: PlayerId }

type InitializedPlayer = Player
type BustedPlayer = Player
type StayedPlayer = Player
type BlackJackedPlayer = Player

type PlayerType =
    | InitializedPlayer of InitializedPlayer
    | BustedPlayer of BustedPlayer
    | StayedPlayer of StayedPlayer
    | BlackJackedPlayer of BlackJackedPlayer

let getPlayersCards playerType : HandCards =
    match playerType with
    | InitializedPlayer x -> x.Hand
    | BustedPlayer x -> x.Hand
    | StayedPlayer x -> x.Hand
    | BlackJackedPlayer x -> x.Hand

type NonBustedPlayer = StayedPlayer of Player | BlackJackedPlayer of Player
    
let getPlayerId (playerType: PlayerType) : PlayerId =
    match playerType with
    | InitializedPlayer x -> x.Id
    | BustedPlayer x -> x.Id
    | PlayerType.StayedPlayer x -> x.Id
    | PlayerType.BlackJackedPlayer x -> x.Id

let getWinningPlayerId (winningPlayers: NonBustedPlayer) : PlayerId =
    match winningPlayers with
    | StayedPlayer x -> x.Id
    | BlackJackedPlayer x -> x.Id

let areEqualPlayerIds playerType1 playerType2 =
    let id1 = playerType1 |> getPlayerId
    let id2 = playerType2 |> getPlayerId
    id1 = id2

let areNotEqualPlayerIds playerType1 playerType2 =
    (areEqualPlayerIds playerType1 playerType2)
    |> not

let getCards hand = match hand with | HandCards cards -> cards

let deck2cards (DeckCards cards) = cards

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
    | Error _ -> Error ErrorDrawCardToHand
    | Ok (card, modifiedDeck) -> 
        let handCards = card :: (hand |> getCards)
        Ok (modifiedDeck, HandCards handCards)

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
        return InitializedPlayer {Hand = handCards; Id = id}, deck
    }

let trySetupDealer drawCardFcn deck =
    let maybe = MaybeBuilder ()

    maybe {
        let! firstCard, deck = drawCardFcn deck
        let! secondCard, deck = drawCardFcn deck
        let handCards = HandCards [firstCard; secondCard]
        return {Hand = handCards}, deck
    }

type NumberOfPlayers = NumberOfPlayers of int

// TODO: make private (use try wrapper below)
let initializePlayers (NumberOfPlayers numberOfPlayers) initialDeck =
    let playerIds = [1..numberOfPlayers] |> List.map PlayerId
    List.fold 
        (fun (currentPlayers, currentDeck) playerId ->
            match trySetupPlayer drawCardFromDeck playerId currentDeck with
            | Ok (player, modifiedDeck) ->
                (currentPlayers@[player], modifiedDeck)
            | Error e ->
                (currentPlayers, currentDeck))
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
    
    let getHandValue (HandCards cards) =
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

let setHandForPlayerType (playerType: PlayerType) hand =
    match playerType with
    | PlayerType.BlackJackedPlayer p -> PlayerType.BlackJackedPlayer p
    | PlayerType.BustedPlayer p -> PlayerType.BustedPlayer p
    | PlayerType.InitializedPlayer p -> PlayerType.InitializedPlayer { p with Hand = hand } 
    | PlayerType.StayedPlayer p -> PlayerType.StayedPlayer { p with Hand = hand }
    
let getStatus (playerType: PlayerType) : PlayerType =
    match playerType with
    | PlayerType.BustedPlayer p -> PlayerType.BustedPlayer p
    | PlayerType.BlackJackedPlayer p -> PlayerType.BlackJackedPlayer p
    | PlayerType.InitializedPlayer p ->
        if calcScore p.Hand = Score 21 then
            PlayerType.BlackJackedPlayer p
        elif calcScore p.Hand > Score 21 then
            PlayerType.BustedPlayer p
        else
            PlayerType.StayedPlayer p
    | PlayerType.StayedPlayer p ->
        if calcScore p.Hand > Score 21 then
            PlayerType.BustedPlayer p
        else
            PlayerType.StayedPlayer p    
    
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
        | Error e -> ErrorDuringPlay
        | Ok (card, deck) -> 
            let (HandCards prevCards) = dealerPlayState.Cards 
            let newHandCards = card::prevCards |> HandCards
            dealerPlays { Cards = newHandCards; Deck = deck }


let getPotentialWinningPlayers (players: NonBustedPlayer list) =
    match players with
    | [] -> None
    | players ->
        players
        |> List.sort // sort by score
        |> List.rev // ensure highest score is first
        |> Some

type Winner =
    | Players of NonBustedPlayer list
    | Dealer of Dealer
    | Nobody


let determineWinners players (dealer: Dealer) =
    let winningPlayersOpt = players |> getPotentialWinningPlayers 
    match winningPlayersOpt with
    | None ->
        if calcScore dealer.Hand <= Score 21 then
            Dealer dealer
        else
            Nobody
    | Some winningPlayers ->
        
        let isDealerBusted (dealer: Dealer) = calcScore dealer.Hand > Score 21
        let dealerBusted = isDealerBusted dealer
        
        (* 
            All winning players have the same Score. 
            We take the first player (players.Head) for comparison with the dealer 
        *)
        match winningPlayers.Head, dealerBusted with
        | StayedPlayer player, false ->
            match (calcScore player.Hand, calcScore dealer.Hand) with
            | pScore, dScore when pScore = dScore -> Nobody
            | pScore, dScore when pScore > dScore -> winningPlayers |> Players
            | _ -> Dealer dealer
        | StayedPlayer _, true ->
            winningPlayers |> Players
        | _ ->
            Nobody

type CurrentPlayerAction = Hit | Stand