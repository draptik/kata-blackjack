module BlackJack.Domain

type Suit = Hearts | Spades | Diamonds | Clubs
type Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type Card = { Face: Face; Suit: Suit }
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
let allFaces = [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace]
 
// Suit list
let allSuits = [Diamonds; Hearts; Clubs; Spades]

// extended list comprehension
let fullDeck = [
    for suit in allSuits do
    for face in allFaces do
    yield { Suit = suit; Face = face } ]

let createDeck : Deck =
    let shuffle deck = 
        let random = System.Random()
        deck |> List.sortBy (fun x -> random.Next())
    fullDeck |> shuffle 

//let drawCardSimple deck =
//    match deck with
//    | [] -> None
//    | topCard::restOfDeck -> Some (topCard, restOfDeck)

type DrawCardFcn = Deck -> (Card * Deck) option
let drawCard : DrawCardFcn =
    fun deck ->
        match deck with
        | [] -> None
        | topCard::restOfDeck -> Some (topCard, restOfDeck)

// 1. iteration
//type SetupPlayer = (Deck -> (Card * Deck) option) -> int -> Deck -> (Player * Deck) 
////let setupPlayerNaive drawCard id deck =
//let setupPlayerNaive : SetupPlayer =
//    fun drawCard id deck ->
//    let firstCard, deck = drawCard deck
//    let secondCard, deck = drawCard deck
//    let hand = [firstCard; secondCard]
//    
//    { Hand = hand; Id = id; Status = CardsDealt }, deck

// 2. iteration
//type SetupPlayerOpt = (Deck -> (Card * Deck) option) -> int -> Deck -> (Player * Deck) option 
//let setupPlayer2 : SetupPlayerOpt =
//    fun drawCard id deck ->
//        match drawCard deck with
//        | None -> None
//        | Some (firstCard, deck) ->
//            match drawCard deck with
//            | None -> None
//            | Some (secondCard, deck) ->
//                let hand = [firstCard; secondCard]
//                Some ({Hand = hand; Id = id; Status = CardsDealt}, deck)

// 3. iteration
type MaybeBuilder() =
    member this.Bind(input, func) =
        match input with
        | None -> None
        | Some value -> func value
    
    member this.Return value =
        Some value

type SetupPlayerOptFcn = DrawCardFcn -> int -> Deck -> (Player * Deck) option         
//let setupPlayer drawCard id deck =
let setupPlayer : SetupPlayerOptFcn =
    fun drawCard id deck ->
        let maybe = MaybeBuilder ()

        maybe {
            let! firstCard, deck = drawCard deck
            let! secondCard, deck = drawCard deck
            let hand = [firstCard; secondCard]

            return {Hand = hand; Id = id; Status = CardsDealt}, deck
        }

let hasPlayerWon player =
    match player.Status with
    | BlackJack -> true
    | _ -> false

let isPlayerBusted player =
    match player.Status with
    | Busted _ -> true
    | _ -> false

    