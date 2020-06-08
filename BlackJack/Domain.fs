module BlackJack.Domain

type Suit = Hearts | Spades | Diamonds | Clubs
type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type Card = { Rank: Rank; Suit: Suit }
type Points = Hard of int | Soft of int * int

type Deck = Card list
type Hand = Card list
type Score = Score of int
type Status = BlackJack | Busted of Score | Stayed of Score | CardsDealt
type Dealer = { Hand: Hand; Status: Status }
type Player = { Hand: Hand; Status: Status; Id: int }
type Players = Player list

type Actions = Hit | Stay

type Game = { Deck: Deck; Dealer: Dealer; Players: Players }


// Face list
let allRanks = [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]
 
// Suit list
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

type SetupPlayerOptFcn = DrawCardFcn -> int -> Deck -> (Player * Deck) option         
let setupPlayer : SetupPlayerOptFcn =
    fun drawCard id deck ->
        let maybe = MaybeBuilder ()

        maybe {
            let! firstCard, deck = drawCard deck
            let! secondCard, deck = drawCard deck
            let hand = [firstCard; secondCard]

            return {Hand = hand; Id = id; Status = CardsDealt}, deck
        }

let calcScore (hand: Hand) : Score =
    let getCardValue card =
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

    let getCardValueAceOne card =
        match card.Rank with
        | Ace -> 1
        | _ -> getCardValue card
    
    let getHandValue hand =
        let simpleValue = List.fold (fun accumulator element -> accumulator + getCardValue element) 0 hand
        
        if simpleValue <= 21 then simpleValue
        else
            List.fold (fun accumulator element ->
                let newValue = accumulator + getCardValue element
                if newValue < 21 then newValue
                else accumulator + getCardValueAceOne element)
                0
                (List.sort hand)
                
    getHandValue hand |> Score

let getStatus hand =
    let score = calcScore hand
    match score with
    | score when score < Score 21 -> Stayed (score)
    | score when score = Score 21 -> BlackJack
    | score -> Busted (score)

// https://github.com/todoa2c/blackjack-fsharp
// https://github.com/dudeNumber4/fsharp-blackjack
// https://github.com/defshef/defshef-blackjack/tree/master/fsharp
// https://github.com/leandrosilva/fsharp-learning/blob/master/fsharp-tutorial-jaoo-2009/tutorial/FunctionalTypes/ExerciseSolution.fsx