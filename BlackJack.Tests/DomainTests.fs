module DomainTests

open Xunit
open FsUnit.Xunit
open BlackJack.Domain

let isFalse = Assert.True(false)

let numberOfCards hand = hand |> getCards |> List.length

let setupCards rawCards =
    rawCards
    |> List.map (fun (rank, suit) -> {Rank = rank; Suit = suit})

let setupHand rawCards status =
    {
        Cards = HandCards (rawCards |> setupCards)
        Status = status
    }
    
let setupDeckCards rawCards =
    rawCards
    |> List.map (fun (rank, suit) -> {Rank = rank; Suit = suit})
    |> cards2deck
    
[<Fact>]
let ``a deck has 52 cards initially`` () =
    createDeck |> deck2cards |> List.length |> should equal 52
    
[<Fact>]
let ``drawing a card from the deck reduces number of cards in deck by one`` () =
    let deck = createDeck
    let maybeCardDeck = drawCardFromDeck deck
    match maybeCardDeck with
    | Ok (_, d) -> (d |> deck2cards).Length |> should equal ((deck |> deck2cards).Length - 1)
    | _ -> isFalse

[<Fact>]
let ``setup player has 2 cards and deck has 50 cards`` () =
    let deck = createDeck
    let maybePlayerDeck = trySetupPlayer drawCardFromDeck (PlayerId 1) deck
    match maybePlayerDeck with
    | Error _ -> isFalse
    | Ok (p, d) -> (p.Hand |> numberOfCards, (d |> deck2cards).Length) |> should equal (2, 50)

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
            (p1.Hand |> numberOfCards, p2.Hand |> numberOfCards, (d2 |> deck2cards).Length) |> should equal (2, 2, 48)

[<Fact>]
let ``trying to draw a card from an empty deck returns None`` () =
    let deck = DeckCards []
    let maybeCardDeck = drawCardFromDeck deck
    match maybeCardDeck with
    | Ok _ -> isFalse
    | Error e -> e |> should equal ErrorDrawCard

[<Fact>]
let ``calcScore returns 0 for an empty hand`` () =
    let emptyHand:HandCards = HandCards []
    emptyHand |> calcScore |> should equal (Score 0)

[<Fact>]
let ``Status and score: below 21`` () =
    setupHand [(Two, Spades); (King, Hearts)] CardsDealt
    |> getStatus
    |> should equal (Stayed (Score 12))
    
[<Fact>]
let ``Status and score: below 21 with Ace as 1`` () =
    setupHand [(Ace, Spades); (Nine, Hearts); (Five, Hearts)] (Stayed (Score 0))
    |> getStatus
    |> should equal (Stayed (Score 15))
    
[<Fact>]
let ``Status and score: below 21 with two Aces`` () =
    setupHand [(Ace, Spades); (Nine, Hearts); (Five, Hearts); (Ace, Clubs)] (Stayed (Score 0))
    |> getStatus
    |> should equal (Stayed (Score 16))
    
[<Fact>]
let ``Status and score: 21 (more than 2 cards)`` () =
    setupHand [(Two, Spades); (King, Hearts); (Nine, Clubs)] (Stayed (Score 0))
    |> getStatus
    |> should equal (Stayed (Score 21))

[<Fact>]
let ``Status and score: 21 freshly dealt: BlackJack`` () =
    setupHand [(Ace, Spades); (King, Hearts)] CardsDealt
    |> getStatus
    |> should equal BlackJack
    
[<Fact>]
let ``Status and score: Busted (with correct score)`` () =
    setupHand [(Queen, Spades); (King, Hearts); (Five, Hearts)] (Stayed (Score 0))
    |> getStatus
    |> should equal (Busted (Score 25))



[<Fact>]
let ``setup dealer has 2 cards and deck has 50 cards`` () =
    let deck = createDeck
    let maybeDealerDeck = trySetupDealer drawCardFromDeck deck
    match maybeDealerDeck with
    | Error _ -> isFalse
    | Ok (p, d) -> (p.Hand |> numberOfCards, (d |> deck2cards).Length) |> should equal (2, 50)

[<Fact>]
let ``dealer plays`` () =
    let initialDeck = setupCards [(Queen, Spades); (Seven, Hearts); (King, Hearts)] |> DeckCards

    let maybeDealerDeck = trySetupDealer drawCardFromDeck initialDeck
    match maybeDealerDeck with
    | Error _ -> isFalse
    | Ok (dealer, deck) ->
        let dealerResponse = dealerPlays { Cards = dealer.Hand.Cards; Deck = deck }
        match dealerResponse with
        | DealerStayed (x, _, _) -> x |> should equal (Score 17)
        | _ -> isFalse

let check3Players  (players: Player list) =
    players.[0] |> should equal 
        { 
            Id = PlayerId 1
            Hand = setupHand [(Two, Spades); (Three, Spades)] CardsDealt
        }
    players.[1] |> should equal 
        { 
            Id = PlayerId 2
            Hand = setupHand [(Four, Spades); (Five, Spades)] CardsDealt
        }
    players.[2] |> should equal 
        { 
            Id = PlayerId 3
            Hand = setupHand [(Six, Spades); (Seven, Spades)] CardsDealt
        }

[<Fact>]
let ``initialize 3 players with minimal deck`` () =
    
    // Arrange
    let initialDeck = setupDeckCards [
        (Two, Spades)
        (Three, Spades)
        (Four, Spades)
        (Five, Spades)
        (Six, Spades)
        (Seven, Spades)
        (Eight, Spades)
    ]

    // Act
    let (players, remainingDeck) = initializePlayers (NumberOfPlayers 3) initialDeck
    
    // Assert
    players.Length |> should equal 3
    (remainingDeck |> deck2cards).Length |> should equal 1
    let firstCardOnRemainingDeck = remainingDeck |> deck2cards |> List.head
    let lastCardOfInitialDeck =
        let (DeckCards initializedCards) = initialDeck
        initializedCards |> List.rev |> List.head
    
    firstCardOnRemainingDeck |> should equal lastCardOfInitialDeck

    check3Players players
    
[<Fact>]
let ``initialize 3 players with understacked deck not enough cards for 3 players`` () =
    
    // Arrange
    let initialDeck = setupDeckCards [
        (Two, Spades)
        (Three, Spades)
        (Four, Spades)
        (Five, Spades)
        (Six, Spades)
    ] 

    // Act
    let (players, (DeckCards remainingDeck)) = initializePlayers (NumberOfPlayers 3) initialDeck
    
    // Assert
    players.Length |> should equal 2
    remainingDeck.Length |> should equal 1
    let firstCardOnRemainingDeck = remainingDeck |> List.head
    let lastCardOfInitialDeck =
        let (DeckCards initializedCards) = initialDeck
        initializedCards |> List.rev |> List.head
    
    firstCardOnRemainingDeck |> should equal lastCardOfInitialDeck

    players.[0] |> should equal 
        { 
            Id = PlayerId 1
            Hand = setupHand [(Two, Spades); (Three, Spades)] CardsDealt
        }
    players.[1] |> should equal 
        { 
            Id = PlayerId 2
            Hand = setupHand [(Four, Spades); (Five, Spades)] CardsDealt
        }

[<Fact>]
let ``try to initialize 3 players with minimal deck`` () =
    
    // Arrange
    let initialDeck = setupDeckCards [
        (Two, Spades)
        (Three, Spades)
        (Four, Spades)
        (Five, Spades)
        (Six, Spades)
        (Seven, Spades)
        (Eight, Spades)
    ]

    // Act
    let opt = tryInitializePlayers (NumberOfPlayers 3) initialDeck
    
    match opt with
    | Error _ -> isFalse
    | Ok (players, deck) ->

        // Assert
        players.Length |> should equal 3
        (deck |> deck2cards).Length |> should equal 1
        (deck |> deck2cards)
        |> List.rev
        |> List.head
        |> should equal { Rank = Eight; Suit = Spades }
        
        check3Players players
        
[<Fact>]
let ``try to initialize 3 players with understacked deck not enough cards for 3 players`` () =
    // Arrange
    let initialDeck = setupCards [
        (Two, Spades)
        (Three, Spades)
        (Four, Spades)
        (Five, Spades)
        (Six, Spades)
    ]

    // Act
    let opt = tryInitializePlayers (NumberOfPlayers 3) (initialDeck |> cards2deck)

    // Assert
    match opt with
    | Error e -> e |> should equal ErrorInitializingPlayers
    | Ok _ -> isFalse

[<Fact>]
let ``get potential winning players - hand with single card`` () =
    let p1 = {Id = PlayerId 1; Hand = setupHand [(Two, Hearts)] (Stayed (Score 2))}
    let p2 = {Id = PlayerId 2; Hand = setupHand [(Two, Spades)] (Stayed (Score 2))}
    let p3 = {Id = PlayerId 3; Hand = setupHand [(Three, Hearts)] (Stayed (Score 3))}
    let p4 = {Id = PlayerId 4; Hand = setupHand [(Three, Spades)] (Stayed (Score 3))}

    let players = [p1;p2;p3;p4]
    
    let winningPlayers = getPotentialWinningPlayers players

    winningPlayers |> should equal (Some [p3;p4])

[<Fact>]
let ``get potential winning players - two players one busted`` () =
    let p1 = {Id = PlayerId 1; Hand = setupHand [(Two, Hearts)] (Stayed (Score 2))}
    let p2 = {Id = PlayerId 2; Hand = setupHand [(Two, Spades); (Queen, Spades); (King, Spades)] (Busted (Score 22))}

    let players = [p1;p2]
    
    let winningPlayers = getPotentialWinningPlayers players
    
    winningPlayers |> should equal (Some [p1])

[<Fact>]
let ``get potential winning players - hand with multiple cards`` () =
    let p1 = {Id = PlayerId 1; Hand = setupHand [(Nine, Hearts); (Ace, Hearts)] (Stayed (Score 20))}
    let p2 = {Id = PlayerId 2; Hand = setupHand [(Five, Spades); (Five, Clubs); (King, Spades)] (Stayed (Score 20))}

    let p3 = {Id = PlayerId 3; Hand = setupHand [(Three, Hearts)] (Stayed (Score 3))}
    let p4 = {Id = PlayerId 4; Hand = setupHand [(Three, Hearts)] (Stayed (Score 3))}

    let players = [p1;p2;p3;p4]
    
    let winningPlayers = getPotentialWinningPlayers players

    winningPlayers |> should equal (Some [p1;p2])
    
[<Fact>]
let ``determine winners example 1 - tied winning players both win`` () =
    let p1 = {
            Id = PlayerId 1
            Hand =  setupHand [(Nine, Hearts); (Ace, Hearts)] (Stayed (Score 20))
        }
    let p2 = {
            Id = PlayerId 2
            Hand = setupHand [(Five, Spades); (Five, Clubs); (King, Spades)]  (Stayed (Score 20))
        }
    let p3 = {
            Id = PlayerId 3
            Hand = setupHand [(Six, Spades); (Seven, Clubs)]  (Stayed (Score 13))
        }
    let dealer = {
            Hand = setupHand [(Five, Hearts); (Jack, Hearts); (Two, Hearts); (Ace, Hearts)]  (Stayed (Score 18))
        }
    
    let actual = determineWinner [p1;p2;p3] dealer
    
    actual |> should equal (Players [p1; p2])

[<Fact>]
let ``determine winners example 2 - busted dealer never wins`` () =
    let p1 = {
            Id = PlayerId 1
            Hand = setupHand [(Two, Hearts); (Two, Hearts)]  (Stayed (Score 4))
        }
    let dealer = {
            Hand = setupHand [(Ten, Hearts); (Jack, Hearts); (Queen, Hearts)]  (Busted (Score 30))
        }

    let actual = determineWinner [p1] dealer

    actual |> should equal (Players [p1])

[<Fact>]
let ``determine winners example 3`` () =
    let p1 = {
            Id = PlayerId 1
            Hand = setupHand [(Nine, Hearts); (Ten, Hearts)]  (Stayed (Score 20))
        }
    let p2 = {
            Id = PlayerId 2
            Hand = setupHand [(Five, Spades); (Queen, Clubs); (Four, Spades)]  (Stayed (Score 19))
        }
    let p3 = {
            Id = PlayerId 3
            Hand = setupHand [(Jack, Spades); (Five, Spades); (Two, Clubs)]  (Stayed (Score 17))
        }
    let p4 = {
            Id = PlayerId 4
            Hand = setupHand [(Jack, Spades); (Ten, Spades)]  (Stayed (Score 20))
        }
    let dealer = {
        Hand = setupHand [(Seven, Hearts); (Jack, Hearts); (Two, Hearts)]  (Stayed (Score 1))
        }
    
    let actual = determineWinner [p1;p2;p3;p4] dealer
    
    actual |> should equal (Players [p4])