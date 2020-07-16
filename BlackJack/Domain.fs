module BlackJack.Domain

type Suit = Hearts | Spades | Diamonds | Clubs
type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type Card = { Rank: Rank; Suit: Suit }

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

let allRanks = [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]
let allSuits = [Diamonds; Hearts; Clubs; Spades]

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

type DrawCardFcn = Deck -> (Card * Deck) option
let drawCard : DrawCardFcn =
    fun deck ->
        match deck with
        | [] -> None
        | topCard::restOfDeck -> Some (topCard, restOfDeck)

type DrawCardToHandFcn = (Deck * Hand) -> (Deck * Hand) option
let drawCardToHand : DrawCardToHandFcn =
    fun (deck, hand) ->
        match drawCard deck with
        | None -> None
        | Some (card, modifiedDeck) -> Some (modifiedDeck, card :: hand)

type MaybeBuilder() =
    member this.Bind(input, func) =
        match input with
        | None -> None
        | Some value -> func value
    
    member this.Return value =
        Some value

type TrySetupPlayerFcn = DrawCardFcn -> PlayerId -> Deck -> (Player * Deck) option         
let trySetupPlayer : TrySetupPlayerFcn =
    fun drawCard id deck ->
        let maybe = MaybeBuilder ()

        maybe {
            let! firstCard, deck = drawCard deck
            let! secondCard, deck = drawCard deck
            let hand = [firstCard; secondCard]

            return {Hand = hand; Id = id; HandStatus = CardsDealt}, deck
        }

type TrySetupDealerFcn = DrawCardFcn -> Deck -> (Dealer * Deck) option         
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
            | Some (player, modifiedDeck) -> (currentPlayers@[player], modifiedDeck)
            | None -> (currentPlayers, currentDeck))
        ([], initialDeck)
        playerIds
    
// returns (Player list * Deck) option
let tryInitializePlayers numberOfPlayers initialDeck =
    let (initializedPlayers, deckAfterInitializingAllPlayers) = initializePlayers numberOfPlayers initialDeck
    let (NumberOfPlayers requestedNumberOfPlayers) = numberOfPlayers
    let numberOfCardsDealtToPlayer = 2
    let isValid = 
        initializedPlayers.Length = requestedNumberOfPlayers
        && deckAfterInitializingAllPlayers.Length = initialDeck.Length - (requestedNumberOfPlayers * numberOfCardsDealtToPlayer)

    if isValid then Some (initializedPlayers, deckAfterInitializingAllPlayers)
    else None

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

type DealerResponse =
    | DealerError of (string * Hand * Deck)
    | DealerBusted of (Score * Hand * Deck)
    | DealerStayed of (Score * Hand * Deck)

type DealerRecord = {
    Hand: Hand
    Deck: Deck
}

let rec dealerAction dealerRecord =
    let score = calcScore dealerRecord.Hand
    match score with
    | x when x > Score 21 -> DealerBusted (score, dealerRecord.Hand, dealerRecord.Deck)
    | x when x >= Score 17 -> DealerStayed (score, dealerRecord.Hand, dealerRecord.Deck)
    | _ ->
        match drawCard dealerRecord.Deck with
        | None -> DealerError ("unable to draw a card", dealerRecord.Hand, dealerRecord.Deck) 
        | Some (card, d) -> dealerAction { Hand = card::dealerRecord.Hand; Deck = d }
