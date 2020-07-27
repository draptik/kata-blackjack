module DomainTests

open Xunit
open FsUnit.Xunit
open BlackJack.Domain

let isFalse = Assert.True(false)
let numberOfCards hand = hand |> getCards |> List.length

[<Fact>]
let ``a deck has 52 cards initially`` () =
    createDeck |> List.length |> should equal 52
    
[<Fact>]
let ``drawing a card from the deck reduces number of cards in deck by one`` () =
    let deck = createDeck
    let maybeCardDeck = drawCardFromDeck deck
    match maybeCardDeck with
    | Ok (_, d) -> d.Length |> should equal (deck.Length - 1)
    | _ -> isFalse

[<Fact>]
let ``setup player has 2 cards and deck has 50 cards`` () =
    let deck = createDeck
    let maybePlayerDeck = trySetupPlayer drawCardFromDeck (PlayerId 1) deck
    match maybePlayerDeck with
    | Error _ -> isFalse
    | Ok (p, d) -> (p.Hand |> numberOfCards, d.Length) |> should equal (2, 50)

[<Fact>]
let ``setup 2 players: each player has 2 cards and deck has 48 cards`` () =
    let deck = createDeck
    let maybePlayerDeck1 = trySetupPlayer drawCardFromDeck (PlayerId 1) deck
    match maybePlayerDeck1 with
    | Error _ -> isFalse
    | Ok (p1, d1) ->
        let maybePlayerDeck2 = trySetupPlayer drawCardFromDeck (PlayerId 2) d1
        match maybePlayerDeck2 with
        | Error _ -> isFalse
        | Ok (p2, d2) ->
            (p1.Hand |> numberOfCards, p2.Hand |> numberOfCards, d2.Length) |> should equal (2, 2, 48)

[<Fact>]
let ``trying to draw a card from an empty deck returns None`` () =
    let deck = []
    let maybeCardDeck = drawCardFromDeck deck
    match maybeCardDeck with
    | Ok _ -> isFalse
    | Error e -> e |> should equal ErrorDrawCard

[<Fact>]
let ``calcScore returns 0 for an empty hand`` () =
    let emptyHand:HandCards = HandCards []
    emptyHand |> calcScore |> should equal (Score 0)

let prepareHand (status, cards) = {Status = status; Cards = HandCards cards}

[<Fact>]
let ``Status and score: below 21`` () =
    (CardsDealt, [
        { Rank = Two; Suit = Spades }; 
        { Rank = King; Suit = Hearts }])
    |> prepareHand |> getStatus |> should equal (Stayed (Score 12))
    
[<Fact>]
let ``Status and score: below 21 with Ace as 1`` () =
    (Stayed (Score 0), [
        { Rank = Ace; Suit = Spades }; 
        { Rank = Nine; Suit = Hearts }; 
        { Rank = Five; Suit = Hearts }])
    |> prepareHand |> getStatus |> should equal (Stayed (Score 15))
    
[<Fact>]
let ``Status and score: below 21 with two Aces`` () =
    (Stayed (Score 0), [
        { Rank = Ace; Suit = Spades }; 
        { Rank = Nine; Suit = Hearts }; 
        { Rank = Five; Suit = Hearts }; 
        { Rank = Ace; Suit = Clubs }])
    |> prepareHand |> getStatus |> should equal (Stayed (Score 16))
    
[<Fact>]
let ``Status and score: 21 (more than 2 cards)`` () =
    (Stayed (Score 0), [
        { Rank = Two; Suit = Spades }; 
        { Rank = King; Suit = Hearts }; 
        { Rank = Nine; Suit = Clubs }])
    |> prepareHand |> getStatus |> should equal (Stayed (Score 21))

[<Fact>]
let ``Status and score: 21 freshly dealt: BlackJack`` () =
    (CardsDealt, [
        { Rank = Ace; Suit = Spades }; 
        { Rank = King; Suit = Hearts }])
    |> prepareHand |> getStatus |> should equal (BlackJack)
    
[<Fact>]
let ``Status and score: Busted (with correct score)`` () =
    (Stayed (Score 0), [
        { Rank = Queen; Suit = Spades }; 
        { Rank = King; Suit = Hearts }; 
        { Rank = Five; Suit = Hearts }])
    |> prepareHand |> getStatus |> should equal (Busted (Score 25))




[<Fact>]
let ``setup dealer has 2 cards and deck has 50 cards`` () =
    let deck = createDeck
    let maybeDealerDeck = trySetupDealer drawCardFromDeck deck
    match maybeDealerDeck with
    | Error _ -> isFalse
    | Ok (p, d) -> (p.Hand |> numberOfCards, d.Length) |> should equal (2, 50)

[<Fact>]
let ``dealer plays`` () =
    let initialDeck = [
        { Rank = Queen; Suit = Spades }; 
        { Rank = Seven; Suit = Hearts };
        { Rank = King; Suit = Hearts }; 
        ]

    let maybeDealerDeck = trySetupDealer drawCardFromDeck initialDeck
    match maybeDealerDeck with
    | Error _ -> isFalse
    | Ok (dealer, deck) ->
        let dealerResponse = dealerPlays { Cards = dealer.Hand.Cards; Deck = deck }
        match dealerResponse with
        | DealerStayed (x, _, _) -> x |> should equal (Score 17)
        | _ -> isFalse

let p1test  (players: Player list) =
    players.[0] |> should equal 
        { 
            Id = PlayerId 1
            Hand = {
                Cards = HandCards [
                    { Rank = Two; Suit = Spades }
                    { Rank = Three; Suit = Spades }
                ]
                Status = CardsDealt}
        }
    players.[1] |> should equal 
        { 
            Id = PlayerId 2
            Hand = {
                Cards = HandCards [
                    { Rank = Four; Suit = Spades }
                    { Rank = Five; Suit = Spades }
                ]
                Status = CardsDealt}
        }
    players.[2] |> should equal 
        { 
            Id = PlayerId 3
            Hand = {
                Cards = HandCards [
                    { Rank = Six; Suit = Spades }
                    { Rank = Seven; Suit = Spades }
                ]
                Status = CardsDealt} 
        }

[<Fact>]
let ``initialize 3 players with minimal deck`` () =
    
    // Arrange
    let initialDeck = [
        { Rank = Two; Suit = Spades }
        { Rank = Three; Suit = Spades }
        { Rank = Four; Suit = Spades }
        { Rank = Five; Suit = Spades }
        { Rank = Six; Suit = Spades }
        { Rank = Seven; Suit = Spades }
        { Rank = Eight; Suit = Spades }
        ]

    // Act
    let (players, deck) = initializePlayers (NumberOfPlayers 3) initialDeck
    
    // Assert
    players.Length |> should equal 3
    deck.Length |> should equal 1
    deck.[0] |> should equal { Rank = Eight; Suit = Spades }

    p1test players
    
[<Fact>]
let ``initialize 3 players with understacked deck not enough cards for 3 players`` () =
    
    // Arrange
    let initialDeck = [
        { Rank = Two; Suit = Spades }
        { Rank = Three; Suit = Spades }
        { Rank = Four; Suit = Spades }
        { Rank = Five; Suit = Spades }
        { Rank = Six; Suit = Spades }
        ]

    // Act
    let (players, deck) = initializePlayers (NumberOfPlayers 3) initialDeck
    
    // Assert
    players.Length |> should equal 2
    deck.Length |> should equal 1
    deck.[0] |> should equal { Rank = Six; Suit = Spades }

    players.[0] |> should equal 
        { 
            Id = PlayerId 1
            Hand = {
                    Cards = HandCards [
                        { Rank = Two; Suit = Spades }
                        { Rank = Three; Suit = Spades }]
                    Status = CardsDealt
            }
        }
    players.[1] |> should equal 
        { 
            Id = PlayerId 2
            Hand = {
                Cards = HandCards [
                    { Rank = Four; Suit = Spades }
                    { Rank = Five; Suit = Spades }]
                Status = CardsDealt
            } 
        }

[<Fact>]
let ``try to initialize 3 players with minimal deck`` () =
    
    // Arrange
    let initialDeck = [
        { Rank = Two; Suit = Spades }
        { Rank = Three; Suit = Spades }
        { Rank = Four; Suit = Spades }
        { Rank = Five; Suit = Spades }
        { Rank = Six; Suit = Spades }
        { Rank = Seven; Suit = Spades }
        { Rank = Eight; Suit = Spades }
        ]

    // Act
    let opt = tryInitializePlayers (NumberOfPlayers 3) initialDeck
    
    match opt with
    | Error _ -> isFalse
    | Ok (players, deck) ->

        // Assert
        players.Length |> should equal 3
        deck.Length |> should equal 1
        deck.[0] |> should equal { Rank = Eight; Suit = Spades }
        
        p1test players
        
[<Fact>]
let ``try to initialize 3 players with understacked deck not enough cards for 3 players`` () =
    // Arrange
    let initialDeck = [
        { Rank = Two; Suit = Spades }
        { Rank = Three; Suit = Spades }
        { Rank = Four; Suit = Spades }
        { Rank = Five; Suit = Spades }
        { Rank = Six; Suit = Spades }
        ]

    // Act
    let opt = tryInitializePlayers (NumberOfPlayers 3) initialDeck

    // Assert
    match opt with
    | Error e -> e |> should equal ErrorInitializingPlayers
    | Ok _ -> isFalse

[<Fact>]
let ``get potential winning players - hand with single card`` () =
    let p1 = {Id = PlayerId 1; Hand = { Cards = HandCards [{ Rank = Two; Suit = Hearts}]; Status = Stayed (Score 2)}}
    let p2 = {Id = PlayerId 2; Hand = { Cards = HandCards [{ Rank = Two; Suit = Spades}]; Status = Stayed (Score 2)}}
    let p3 = {Id = PlayerId 3; Hand = { Cards = HandCards [{ Rank = Three; Suit = Hearts}]; Status = Stayed (Score 3)}}
    let p4 = {Id = PlayerId 4; Hand = { Cards = HandCards [{ Rank = Three; Suit = Spades}]; Status = Stayed (Score 3)}}

    let players = [p1;p2;p3;p4]
    
    // p3 and p4 are tied with score 3
    // p1 and p2 are below score 3
    let winningPlayers = getPotentialWinningPlayers players
    winningPlayers |> should equal (Some [p3;p4])

[<Fact>]
let ``get potential winning players - two players one busted`` () =
    let p1 = {Id = PlayerId 1; Hand = { Cards = HandCards [{ Rank = Two; Suit = Hearts}]; Status = Stayed (Score 2)}}
    let p2 = {Id = PlayerId 2; Hand = { Cards = HandCards [{ Rank = Two; Suit = Spades}; { Rank = Queen; Suit = Spades}; { Rank = King; Suit = Spades}]; Status = Busted (Score 22)}}

    let players = [p1;p2]
    
    let winningPlayers = getPotentialWinningPlayers players
    winningPlayers |> should equal (Some [p1])

[<Fact>]
let ``get potential winning players - hand with multiple cards`` () =
    let p1 = {
        Id = PlayerId 1
        Hand = {
            Cards = HandCards [{ Rank = Nine; Suit = Hearts}; { Rank = Ace; Suit = Hearts}]
            Status = Stayed (Score 20)}
        }
    let p2 = {
        Id = PlayerId 2
        Hand = {
            Cards = HandCards [ { Rank = Five; Suit = Spades}; { Rank = Five; Suit = Clubs}; { Rank = King; Suit = Spades}]
            Status = Stayed (Score 20)}
        }

    let p3 = {Id = PlayerId 3; Hand = { Cards = HandCards [{ Rank = Three; Suit = Hearts}]; Status = Stayed (Score 3)}}
    let p4 = {Id = PlayerId 3; Hand = { Cards = HandCards [{ Rank = Three; Suit = Hearts}]; Status = Stayed (Score 3)}}

    let players = [p1;p2;p3;p4]
    
    // p1 and p2 are have 20
    // p3 and p4 are below 20
    let winningPlayers = getPotentialWinningPlayers players
    winningPlayers |> should equal (Some [p1;p2])

[<Fact>]
let ``determine winners example 1 - tied winning players both win`` () =
    let p1 = {
        Id = PlayerId 1
        Hand = {
            Cards = HandCards [
                { Rank = Nine; Suit = Hearts}
                { Rank = Ace; Suit = Hearts}]
            Status = Stayed (Score 20)}
        }
    let p2 = {
        Id = PlayerId 2
        Hand = {
            Cards = HandCards [
                { Rank = Five; Suit = Spades}
                { Rank = Five; Suit = Clubs}
                { Rank = King; Suit = Spades}]
            Status = Stayed (Score 20)}
        }
    let p3 = {
        Id = PlayerId 3
        Hand = {
            Cards = HandCards [
                { Rank = Six; Suit = Spades}
                { Rank = Seven; Suit = Clubs}]
            Status = Stayed (Score 13)} 
        }
    let dealer = {
        Hand = {
            Cards = HandCards [
                {Rank = Five; Suit = Hearts}
                {Rank = Jack; Suit = Hearts}
                {Rank = Two; Suit = Hearts}
                {Rank = Ace; Suit = Hearts}]
            Status = Stayed (Score 18)}
        }
    let actual = determinWinner [p1;p2;p3] dealer
    actual |> should equal (Players [p1; p2])

[<Fact>]
let ``determine winners example 2 - busted dealer never wins`` () =
    let p1 = {
        Id = PlayerId 1
        Hand = {
            Cards = HandCards [
            { Rank = Two; Suit = Hearts}
            { Rank = Two; Suit = Hearts}]
            Status = Stayed (Score 2)}
        }
    let dealer = {
        Hand = {
            Cards = HandCards [
                {Rank = Ten; Suit = Hearts}
                {Rank = Jack; Suit = Hearts}
                {Rank = Queen; Suit = Hearts}]
            Status = Busted (Score 30)}
        }
    let actual = determinWinner [p1] dealer
    actual |> should equal (Players [p1])

[<Fact>]
let ``determine winners example 3`` () =
    let p1 = {
        Id = PlayerId 1
        Hand = {
            Cards = HandCards [
            { Rank = Nine; Suit = Hearts}
            { Rank = Ten; Suit = Hearts}]
            Status = Stayed (Score 19)}
        }
    let p2 = {
        Id = PlayerId 2
        Hand = {
            Cards = HandCards [
            { Rank = Five; Suit = Spades}
            { Rank = Queen; Suit = Clubs}
            { Rank = Four; Suit = Spades}]
            Status = Stayed (Score 19)}
        }
    let p3 = {
        Id = PlayerId 3
        Hand = {
            Cards = HandCards [
            { Rank = Jack; Suit = Spades}
            { Rank = Five; Suit = Spades}
            { Rank = Two; Suit = Clubs}]
            Status = Stayed (Score 17)}
        }
    let p4 = {
        Id = PlayerId 3
        Hand = {
            Cards = HandCards [
            { Rank = Jack; Suit = Spades}
            { Rank = Ten; Suit = Spades}]
            Status = Stayed (Score 20)}
        }
    let dealer = {
        Hand = {
            Cards = HandCards [
            {Rank = Seven; Suit = Hearts}
            {Rank = Jack; Suit = Hearts}
            {Rank = Two; Suit = Hearts}]
            Status = Stayed (Score 19)
        }
    }
    let actual = determinWinner [p1;p2;p3;p4] dealer
    actual |> should equal (Players [p4])