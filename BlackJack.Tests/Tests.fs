module Tests

open System
open Xunit
open BlackJack.Domain
open Xunit

[<Fact>]
let ``a deck has 52 cards initially`` () =
    let deck = createDeck
    Assert.Equal(deck.Length, 52)
    
[<Fact>]
let ``drawing a card from the deck reduces number of cards in deck by one`` () =
    let deck = createDeck
    let opt = drawCard deck
    match opt with
    | Some (_, d) -> Assert.Equal(deck.Length - 1, d.Length)
    | _ -> Assert.True(false)

[<Fact>]
let ``setup player has 2 cards and deck has 50 cards`` () =
    let deck = createDeck
    let result = setupPlayer drawCard 1 deck
    match result with
    | None -> Assert.True(false)
    | Some (p, d) ->
        Assert.Equal((p.Hand.Length, d.Length), (2, 50))

[<Fact>]
let ``setup 2 players: each player has 2 cards and deck has 48 cards`` () =
    let deck = createDeck
    let result1 = setupPlayer drawCard 1 deck
    match result1 with
    | None -> Assert.True(false)
    | Some (p1, d1) ->
        let result2 = setupPlayer drawCard 2 d1
        match result2 with
        | None -> Assert.True(false)
        | Some (p2, d2) ->
            Assert.Equal((p1.Hand.Length, p2.Hand.Length, d2.Length), (2, 2, 48))

[<Fact>]
let ``trying to draw a card from an empty deck returns None`` () =
    let deck = []
    let result = drawCard deck
    match result with
    | Some _ -> Assert.True(false)
    | None -> Assert.True(true)
    
[<Fact>]
let ``Hand calculation below 21`` () =
    let hand = [{ Rank = Two; Suit = Spades }; { Rank = King; Suit = Hearts }]
    let score = getStatus hand
    Assert.Equal(Stayed (Score 12), score)
    
[<Fact>]
let ``Hand calculation below 21 with Ace as 1`` () =
    let hand = [{ Rank = Ace; Suit = Spades }; { Rank = Nine; Suit = Hearts }; { Rank = Five; Suit = Hearts }]
    let score = getStatus hand
    Assert.Equal(Stayed (Score 15), score)
    
[<Fact>]
let ``Hand calculation below 21 with two Aces`` () =
    let hand = [{ Rank = Ace; Suit = Spades }; { Rank = Nine; Suit = Hearts }; { Rank = Five; Suit = Hearts }; { Rank = Ace; Suit = Clubs }]
    let score = getStatus hand
    Assert.Equal(Stayed (Score 16), score)
    
[<Fact>]
let ``Hand calculation blackjack`` () =
    let hand = [{ Rank = Ace; Suit = Spades }; { Rank = King; Suit = Hearts }]
    let score = getStatus hand
    Assert.Equal(BlackJack, score)
    
[<Fact>]
let ``Hand calculation Busted (with correct score)`` () =
    let hand = [{ Rank = Queen; Suit = Spades }; { Rank = King; Suit = Hearts }; { Rank = Five; Suit = Hearts }]
    let score = getStatus hand
    Assert.Equal(Busted (Score 25), score)
