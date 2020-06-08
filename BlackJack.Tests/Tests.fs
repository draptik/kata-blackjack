module Tests

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
    let result = setupPlayer drawCard 1 deck
    match result with
    | None -> isFalse
    | Some (p, d) -> (p.Hand.Length, d.Length) |> should equal (2, 50)

[<Fact>]
let ``setup 2 players: each player has 2 cards and deck has 48 cards`` () =
    let deck = createDeck
    let result1 = setupPlayer drawCard 1 deck
    match result1 with
    | None -> isFalse
    | Some (p1, d1) ->
        let result2 = setupPlayer drawCard 2 d1
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
let ``Hand calculation below 21`` () =
    [{ Rank = Two; Suit = Spades }; { Rank = King; Suit = Hearts }]
    |> getStatus |> should equal (Stayed (Score 12))
    
[<Fact>]
let ``Hand calculation below 21 with Ace as 1`` () =
    [{ Rank = Ace; Suit = Spades }; { Rank = Nine; Suit = Hearts }; { Rank = Five; Suit = Hearts }]
    |> getStatus |> should equal (Stayed (Score 15))
    
[<Fact>]
let ``Hand calculation below 21 with two Aces`` () =
    [{ Rank = Ace; Suit = Spades }; { Rank = Nine; Suit = Hearts }; { Rank = Five; Suit = Hearts }; { Rank = Ace; Suit = Clubs }]
    |> getStatus |> should equal (Stayed (Score 16))
    
[<Fact>]
let ``Hand calculation 21 (more than 2 cards)`` () =
    [{ Rank = Two; Suit = Spades }; { Rank = King; Suit = Hearts }; { Rank = Nine; Suit = Clubs }]
    |> getStatus |> should equal (Stayed (Score 21))

[<Fact>]
let ``Hand calculation 21 with 2 cards: BlackJack`` () =
    [{ Rank = Ace; Suit = Spades }; { Rank = King; Suit = Hearts }]
    |> getStatus |> should equal (BlackJack)
    
[<Fact>]
let ``Hand calculation Busted (with correct score)`` () =
    [{ Rank = Queen; Suit = Spades }; { Rank = King; Suit = Hearts }; { Rank = Five; Suit = Hearts }]
    |> getStatus |> should equal (Busted (Score 25))
