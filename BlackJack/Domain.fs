module BlackJack.Domain

type Suit = Hearts | Spades | Diamonds | Clubs
type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type Card = { Rank: Rank; Suit: Suit }
//type Points = Hard of int | Soft of int * int

type Deck = Card list
type Hand = Card list
type Score = Score of int
type Status = BlackJack | Busted of Score | Stayed of Score | CardsDealt
type Dealer = { Hand: Hand; Status: Status }
type PlayerId = PlayerId of int
type Player = { Hand: Hand; Status: Status; Id: PlayerId }
type Players = Player list

type Actions = Hit | Stay

type Game = { Deck: Deck; Dealer: Dealer; Players: Players }


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

type MaybeBuilder() =
    member this.Bind(input, func) =
        match input with
        | None -> None
        | Some value -> func value
    
    member this.Return value =
        Some value

type SetupPlayerOptFcn = DrawCardFcn -> PlayerId -> Deck -> (Player * Deck) option         
let setupPlayer : SetupPlayerOptFcn =
    fun drawCard id deck ->
        let maybe = MaybeBuilder ()

        maybe {
            let! firstCard, deck = drawCard deck
            let! secondCard, deck = drawCard deck
            let hand = [firstCard; secondCard]

            return {Hand = hand; Id = id; Status = CardsDealt}, deck
        }

type SetupDealerOptFcn = DrawCardFcn -> Deck -> (Dealer * Deck) option         
let setupDealer : SetupDealerOptFcn =
    fun drawCard deck ->
        let maybe = MaybeBuilder ()

        maybe {
            let! firstCard, deck = drawCard deck
            let! secondCard, deck = drawCard deck
            let hand = [firstCard; secondCard]

            return {Hand = hand; Status = CardsDealt}, deck
        }


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

type GetStatus = (Status * Hand) -> Status
let getStatus : GetStatus =
    fun (status, hand) ->
        let score = calcScore hand
        match status, score with
        | status, score when status = CardsDealt && score = Score 21 -> BlackJack
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


// RESOURCES -----------------------------------------------
// https://github.com/todoa2c/blackjack-fsharp
// https://github.com/dudeNumber4/fsharp-blackjack
// https://github.com/defshef/defshef-blackjack/tree/master/fsharp
// https://github.com/leandrosilva/fsharp-learning/blob/master/fsharp-tutorial-jaoo-2009/tutorial/FunctionalTypes/ExerciseSolution.fsx