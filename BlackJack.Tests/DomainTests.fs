module DomainTests

open Xunit
open FsUnit.Xunit
open BlackJack.Domain

let isFalse = Assert.True(false)

[<Fact>]
let ``a deck has 52 cards initially`` () =
    createDeck |> List.length |> should equal 52
    
[<Fact>]
let ``drawing a card from the deck reduces number of cards in deck by one`` () =
    let deck = createDeck
    let opt = drawCard deck
    match opt with
    | Some (_, d) -> d.Length |> should equal (deck.Length - 1)
    | _ -> isFalse

[<Fact>]
let ``setup player has 2 cards and deck has 50 cards`` () =
    let deck = createDeck
    let result = setupPlayer drawCard (PlayerId 1) deck
    match result with
    | None -> isFalse
    | Some (p, d) -> (p.Hand.Length, d.Length) |> should equal (2, 50)

[<Fact>]
let ``setup 2 players: each player has 2 cards and deck has 48 cards`` () =
    let deck = createDeck
    let result1 = setupPlayer drawCard (PlayerId 1) deck
    match result1 with
    | None -> isFalse
    | Some (p1, d1) ->
        let result2 = setupPlayer drawCard (PlayerId 2) d1
        match result2 with
        | None -> isFalse
        | Some (p2, d2) ->
            (p1.Hand.Length, p2.Hand.Length, d2.Length) |> should equal (2, 2, 48)

[<Fact>]
let ``trying to draw a card from an empty deck returns None`` () =
    let deck = []
    let result = drawCard deck
    match result with
    | Some _ -> isFalse
    | None -> Assert.True(true)
    
[<Fact>]
let ``Status and score: below 21`` () =
    (CardsDealt, [
        { Rank = Two; Suit = Spades }; 
        { Rank = King; Suit = Hearts }])
    |> getStatus |> should equal (Stayed (Score 12))
    
[<Fact>]
let ``Status and score: below 21 with Ace as 1`` () =
    (Stayed (Score 0), [
        { Rank = Ace; Suit = Spades }; 
        { Rank = Nine; Suit = Hearts }; 
        { Rank = Five; Suit = Hearts }])
    |> getStatus |> should equal (Stayed (Score 15))
    
[<Fact>]
let ``Status and score: below 21 with two Aces`` () =
    (Stayed (Score 0), [
        { Rank = Ace; Suit = Spades }; 
        { Rank = Nine; Suit = Hearts }; 
        { Rank = Five; Suit = Hearts }; 
        { Rank = Ace; Suit = Clubs }])
    |> getStatus |> should equal (Stayed (Score 16))
    
[<Fact>]
let ``Status and score: 21 (more than 2 cards)`` () =
    (Stayed (Score 0), [
        { Rank = Two; Suit = Spades }; 
        { Rank = King; Suit = Hearts }; 
        { Rank = Nine; Suit = Clubs }])
    |> getStatus |> should equal (Stayed (Score 21))

[<Fact>]
let ``Status and score: 21 freshly dealt: BlackJack`` () =
    (CardsDealt, [
        { Rank = Ace; Suit = Spades }; 
        { Rank = King; Suit = Hearts }])
    |> getStatus |> should equal (BlackJack)
    
[<Fact>]
let ``Status and score: Busted (with correct score)`` () =
    (Stayed (Score 0), [
        { Rank = Queen; Suit = Spades }; 
        { Rank = King; Suit = Hearts }; 
        { Rank = Five; Suit = Hearts }])
    |> getStatus |> should equal (Busted (Score 25))




[<Fact>]
let ``setup dealer has 2 cards and deck has 50 cards`` () =
    let deck = createDeck
    let result = setupDealer drawCard deck
    match result with
    | None -> isFalse
    | Some (p, d) -> (p.Hand.Length, d.Length) |> should equal (2, 50)

[<Fact>]
let ``dealerAction 1`` () =
    let initialDeck = [
        { Rank = Queen; Suit = Spades }; 
        { Rank = Seven; Suit = Hearts };
        { Rank = King; Suit = Hearts }; 
        ]

    let dealerOpt = setupDealer drawCard initialDeck
    match dealerOpt with
    | None -> isFalse
    | Some (dealer, deck) ->
        let dealerResponse = dealerAction { Hand = dealer.Hand; Deck = deck }
        match dealerResponse with
        | DealerStayed (x, _, _) -> x |> should equal (Score 17)
        | _ -> isFalse
