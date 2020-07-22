module BlackJack.Domain

type AppError =
    | ErrorInitializingPlayers
    | ErrorDuringDealerPlay
    | ErrorPlayerPlayingInvalidHandState
    | ErrorDrawCard
    | ErrorDrawCardToHand
    | ErrorAskingForNumberOfPlayers

type Suit = Hearts | Spades | Diamonds | Clubs
type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type Card = { Rank: Rank; Suit: Suit }

let allRanks = [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]
let allSuits = [Diamonds; Hearts; Clubs; Spades]

let showCard (card: Card) =
    // https://en.wikipedia.org/wiki/Playing_cards_in_Unicode#Block
    match card with
    | { Suit = Spades; Rank = Ace } -> sprintf "\U0001F0A1"
    | { Suit = Spades; Rank = Two } -> sprintf "\U0001F0A2"
    | { Suit = Spades; Rank = Three } -> sprintf "\U0001F0A3"
    | { Suit = Spades; Rank = Four } -> sprintf "\U0001F0A4"
    | { Suit = Spades; Rank = Five } -> sprintf "\U0001F0A5"
    | { Suit = Spades; Rank = Six } -> sprintf "\U0001F0A6"
    | { Suit = Spades; Rank = Seven } -> sprintf "\U0001F0A7"
    | { Suit = Spades; Rank = Eight } -> sprintf "\U0001F0A8"
    | { Suit = Spades; Rank = Nine } -> sprintf "\U0001F0A9"
    | { Suit = Spades; Rank = Ten } -> sprintf "\U0001F0AA"
    | { Suit = Spades; Rank = Jack } -> sprintf "\U0001F0AB"
    | { Suit = Spades; Rank = Queen } -> sprintf "\U0001F0AD"
    | { Suit = Spades; Rank = King } -> sprintf "\U0001F0AE"

    | { Suit = Hearts; Rank = Ace } -> sprintf "\U0001F0B1"
    | { Suit = Hearts; Rank = Two } -> sprintf "\U0001F0B2"
    | { Suit = Hearts; Rank = Three } -> sprintf "\U0001F0B3"
    | { Suit = Hearts; Rank = Four } -> sprintf "\U0001F0B4"
    | { Suit = Hearts; Rank = Five } -> sprintf "\U0001F0B5"
    | { Suit = Hearts; Rank = Six } -> sprintf "\U0001F0B6"
    | { Suit = Hearts; Rank = Seven } -> sprintf "\U0001F0B7"
    | { Suit = Hearts; Rank = Eight } -> sprintf "\U0001F0B8"
    | { Suit = Hearts; Rank = Nine } -> sprintf "\U0001F0B9"
    | { Suit = Hearts; Rank = Ten } -> sprintf "\U0001F0BA"
    | { Suit = Hearts; Rank = Jack } -> sprintf "\U0001F0BB"
    | { Suit = Hearts; Rank = Queen } -> sprintf "\U0001F0BD"
    | { Suit = Hearts; Rank = King } -> sprintf "\U0001F0BE"

    | { Suit = Diamonds; Rank = Ace } -> sprintf "\U0001F0C1"
    | { Suit = Diamonds; Rank = Two } -> sprintf "\U0001F0C2"
    | { Suit = Diamonds; Rank = Three } -> sprintf "\U0001F0C3"
    | { Suit = Diamonds; Rank = Four } -> sprintf "\U0001F0C4"
    | { Suit = Diamonds; Rank = Five } -> sprintf "\U0001F0C5"
    | { Suit = Diamonds; Rank = Six } -> sprintf "\U0001F0C6"
    | { Suit = Diamonds; Rank = Seven } -> sprintf "\U0001F0C7"
    | { Suit = Diamonds; Rank = Eight } -> sprintf "\U0001F0C8"
    | { Suit = Diamonds; Rank = Nine } -> sprintf "\U0001F0C9"
    | { Suit = Diamonds; Rank = Ten } -> sprintf "\U0001F0CA"
    | { Suit = Diamonds; Rank = Jack } -> sprintf "\U0001F0CB"
    | { Suit = Diamonds; Rank = Queen } -> sprintf "\U0001F0CD"
    | { Suit = Diamonds; Rank = King } -> sprintf "\U0001F0CE"

    | { Suit = Clubs; Rank = Ace } -> sprintf "\U0001F0D1"
    | { Suit = Clubs; Rank = Two } -> sprintf "\U0001F0D2"
    | { Suit = Clubs; Rank = Three } -> sprintf "\U0001F0D3"
    | { Suit = Clubs; Rank = Four } -> sprintf "\U0001F0D4"
    | { Suit = Clubs; Rank = Five } -> sprintf "\U0001F0D5"
    | { Suit = Clubs; Rank = Six } -> sprintf "\U0001F0D6"
    | { Suit = Clubs; Rank = Seven } -> sprintf "\U0001F0D7"
    | { Suit = Clubs; Rank = Eight } -> sprintf "\U0001F0D8"
    | { Suit = Clubs; Rank = Nine } -> sprintf "\U0001F0D9"
    | { Suit = Clubs; Rank = Ten } -> sprintf "\U0001F0DA"
    | { Suit = Clubs; Rank = Jack } -> sprintf "\U0001F0DB"
    | { Suit = Clubs; Rank = Queen } -> sprintf "\U0001F0DD"
    | { Suit = Clubs; Rank = King } -> sprintf "\U0001F0DE"

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

type DrawCardFcn = Deck -> Result<(Card * Deck),AppError>
let drawCard : DrawCardFcn =
    fun deck ->
        match deck with
        | [] -> Error ErrorDrawCard
        | topCard::restOfDeck -> Ok (topCard, restOfDeck)

type DrawCardToHandFcn = (Deck * Hand) -> Result<(Deck * Hand),AppError>
let drawCardToHand : DrawCardToHandFcn =
    fun (deck, hand) ->
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

type TrySetupPlayerFcn = DrawCardFcn -> PlayerId -> Deck -> Result<(Player * Deck), AppError>         
let trySetupPlayer : TrySetupPlayerFcn =
    fun drawCard id deck ->
        let maybe = MaybeBuilder ()

        maybe {
            let! firstCard, deck = drawCard deck
            let! secondCard, deck = drawCard deck
            let hand = [firstCard; secondCard]

            return {Hand = hand; Id = id; HandStatus = CardsDealt}, deck
        }

type TrySetupDealerFcn = DrawCardFcn -> Deck -> Result<(Dealer * Deck), AppError>         
let trySetupDealer : TrySetupDealerFcn =
    fun drawCard deck ->
        let maybe = MaybeBuilder ()

        maybe {
            let! firstCard, deck = drawCard deck
            let! secondCard, deck = drawCard deck
            let hand = [firstCard; secondCard]

            return {Hand = hand; HandStatus = CardsDealt}, deck
        }

type NumberOfPlayers = NumberOfPlayers of int

// returns (Player list * Deck)
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
    
// returns Result<(Player list * Deck),AppError>
let tryInitializePlayers numberOfPlayers initialDeck =
    let (initializedPlayers, deckAfterInitializingAllPlayers) = initializePlayers numberOfPlayers initialDeck
    let (NumberOfPlayers requestedNumberOfPlayers) = numberOfPlayers
    let numberOfCardsDealtToPlayer = 2
    let isValid = 
        initializedPlayers.Length = requestedNumberOfPlayers
        && deckAfterInitializingAllPlayers.Length = initialDeck.Length - (requestedNumberOfPlayers * numberOfCardsDealtToPlayer)

    if isValid then Ok (initializedPlayers, deckAfterInitializingAllPlayers)
    else Error ErrorInitializingPlayers

type CalcScore = Hand -> Score
let calcScore : CalcScore =
    fun hand ->
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

type GetStatus = (HandStatus * Hand) -> HandStatus
let getStatus : GetStatus =
    fun (handStatus, hand) ->
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

let showHand (hand: Hand) =
    hand |> List.map showCard |> String.concat " " |> sprintf "%A %A" (calcScore hand)


// create 2 lists of players: tied for max score and the rest
let splitPlayers (players: Player list) : (Player list * Player list) =
    match players with
    | [] -> ([], [])
    | players ->
        let groupedPlayers = 
            players 
            |> List.groupBy (fun p -> calcScore p.Hand) 
            |> List.sort // sort by score
            |> List.rev // ensure highest score is first

        // get info via destruct
        let (_, winningPlayers) = groupedPlayers.Head

        // get info via aggregate function 'collect'
        let otherPlayers = groupedPlayers.Tail |> List.collect (fun (_, b) -> b)

        (winningPlayers, otherPlayers)
