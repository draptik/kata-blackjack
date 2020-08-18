module DomainTests

open Xunit
open FsUnit.Xunit
open BlackJack.Domain

let isFalse = Assert.True(false)

let numberOfCards hand = hand |> getCards |> List.length

let setupCards (rawCards: (Rank * Suit) list) =
    rawCards
    |> List.map (fun (rank, suit) -> {Rank = rank; Suit = suit})


let setupPlayer id rawCards =
        {
            Hand = (rawCards |> setupCards |> HandCards)
            Id = PlayerId id
        }

let setupInitialPlayerType id (rawCards: (Rank * Suit) list) =
    rawCards |> setupPlayer id |> InitializedPlayer 

let setupStayedPlayerType id (rawCards: (Rank * Suit) list) =
    rawCards |> setupPlayer id |> StayedPlayer 

let setupBlackJackedPlayerType id (rawCards: (Rank * Suit) list) =
    rawCards |> setupPlayer id |> BlackJackedPlayer 

let setupBustedPlayerType id (rawCards: (Rank * Suit) list) =
    rawCards |> setupPlayer id |> BustedPlayer 

let setupInitialPlayerTypeWithDefaultId (rawCards: (Rank * Suit) list) : PlayerType =
    rawCards |> setupInitialPlayerType 1
    
let setupStayedPlayerTypeWithDefaultId (rawCards: (Rank * Suit) list) =
    rawCards |> setupStayedPlayerType 1 

let setupBlackedPlayerTypeWithDefaultId (rawCards: (Rank * Suit) list) =
    rawCards |> setupBlackJackedPlayerType 1 
    
let setupBustedPlayerTypeWithDefaultId (rawCards: (Rank * Suit) list) =
    rawCards |> setupBustedPlayerType 1 

let handEquals (expectedScore: Score) (hand: HandCards) =
    hand |> calcScore |> should equal expectedScore

let setupDealer rawCards =
    {
        Hand = rawCards
               |> setupCards
               |> HandCards 
    }
    
let IsInitializedWithScore score playerType =
    match playerType with
    | InitializedPlayer p -> p.Hand |> handEquals score
    | _ -> isFalse

let IsStayedWithScore score playerType =
    match playerType with
    | InitializedPlayer p -> p.Hand |> handEquals score
    | StayedPlayer p -> p.Hand |> handEquals score
    | _ -> isFalse

let IsBustedWithScore score playerType =
    match playerType with
    | BustedPlayer p -> p.Hand |> handEquals score
    | _ -> isFalse

let IsBlackJacked playerType =
    match playerType with
    | BlackJackedPlayer _ -> true |> should equal true
    | _ -> isFalse
    
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
    | Ok (p, d) -> (p |> getPlayersCards |> numberOfCards, (d |> deck2cards).Length) |> should equal (2, 50)

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
            let p1NumberOfCards = p1 |> getPlayersCards |> numberOfCards
            let p2NumberOfCards = p2 |> getPlayersCards |> numberOfCards
            let deck = (d2 |> deck2cards).Length
            let result = (p1NumberOfCards, p2NumberOfCards, deck)
            let expected = (2, 2, 48)
            result |> should equal expected

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
    setupInitialPlayerTypeWithDefaultId [(Two, Spades); (King, Hearts)]
    |> IsStayedWithScore (Score 12)
    
[<Fact>]
let ``Status and score: below 21 with Ace as 1`` () =
    setupStayedPlayerTypeWithDefaultId [(Ace, Spades); (Nine, Hearts); (Five, Hearts)]
    |> IsStayedWithScore (Score 15)
    
[<Fact>]
let ``Status and score: below 21 with two Aces`` () =
    setupStayedPlayerTypeWithDefaultId [(Ace, Spades); (Nine, Hearts); (Five, Hearts); (Ace, Clubs)]
    |> IsStayedWithScore (Score 16)
    
[<Fact>]
let ``Status and score: 21 (more than 2 cards)`` () =
    setupStayedPlayerTypeWithDefaultId [(Two, Spades); (King, Hearts); (Nine, Clubs)]
    |> IsStayedWithScore (Score 21)

[<Fact>]
let ``Status and score: 21 freshly dealt: BlackJack`` () =
    setupBlackedPlayerTypeWithDefaultId [(Ace, Spades); (King, Hearts)]
    |> IsBlackJacked
    
[<Fact>]
let ``Status and score: Busted (with correct score)`` () =
    setupBustedPlayerType 1 [(Queen, Spades); (King, Hearts); (Five, Hearts)]
    |> IsBustedWithScore (Score 25)


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
        let dealerResponse = dealerPlays { Cards = dealer.Hand; Deck = deck }
        match dealerResponse with
        | DealerStayed (x, _, _) -> x |> should equal (Score 17)
        | _ -> isFalse

let check3Players  (players: PlayerType list) =
    players.[0] |> should equal (setupPlayer 1 [(Two, Spades); (Three, Spades)] |> InitializedPlayer)
    players.[1] |> should equal (setupPlayer 2 [(Four, Spades); (Five, Spades)] |> InitializedPlayer)
    players.[2] |> should equal (setupPlayer 3 [(Six, Spades); (Seven, Spades)] |> InitializedPlayer)

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
    players.[0] |> should equal (setupPlayer 1 [(Two, Spades); (Three, Spades)] |> InitializedPlayer)
    players.[1] |> should equal (setupPlayer 2 [(Four, Spades); (Five, Spades)] |> InitializedPlayer)

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
    let p1 = setupStayedPlayerType 1 [(Two, Hearts)]
    let p2 = setupStayedPlayerType 1 [(Two, Spades)]
    let p3 = setupStayedPlayerType 1 [(Three, Hearts)]
    let p4 = setupStayedPlayerType 1 [(Three, Spades)]

    let players = [p1;p2;p3;p4]
    
    let winningPlayers = getPotentialWinningPlayers players

    winningPlayers |> should equal (Some [p3;p4])

[<Fact>]
let ``get potential winning players - two players one busted`` () =
    let p1 = setupStayedPlayerType 1 [(Two, Hearts)]
    let p2 = setupBustedPlayerType 2 [(Two, Spades); (Queen, Spades); (King, Spades)]

    let players = [p1;p2]
    
    let winningPlayers = getPotentialWinningPlayers players
    
    winningPlayers |> should equal (Some [p1])

[<Fact>]
let ``get potential winning players - hand with multiple cards`` () =
    let p1 = setupStayedPlayerType 1 [(Nine, Hearts); (Ace, Hearts)]
    let p2 = setupStayedPlayerType 2 [(Five, Spades); (Five, Clubs); (King, Spades)]
    let p3 = setupStayedPlayerType 3 [(Three, Hearts)]
    let p4 = setupStayedPlayerType 4 [(Three, Hearts)]

    let players = [p1;p2;p3;p4]
    
    let winningPlayers = getPotentialWinningPlayers players

    winningPlayers |> should equal (Some [p1;p2])
    
[<Fact>]
let ``determine winners example 1 - tied winning players both win`` () =
    let p1 = setupStayedPlayerType 1 [(Nine, Hearts); (Ace, Hearts)]
    let p2 = setupStayedPlayerType 2 [(Five, Spades); (Five, Clubs); (King, Spades)]
    let p3 = setupStayedPlayerType 3 [(Six, Spades); (Seven, Clubs)]
    let dealer = setupDealer [(Five, Hearts); (Jack, Hearts); (Two, Hearts); (Ace, Hearts)]
    
    let actual = determineWinners [p1;p2;p3] dealer
    
    actual |> should equal (Players [p1; p2])

[<Fact>]
let ``determine winners example 2 - busted dealer never wins`` () =
    let p1 = setupStayedPlayerType 1 [(Two, Hearts); (Two, Hearts)]
    let dealer = setupDealer [(Ten, Hearts); (Jack, Hearts); (Queen, Hearts)]

    let actual = determineWinners [p1] dealer

    actual |> should equal (Players [p1])

[<Fact>]
let ``determine winners example 3`` () =
    let p1 = setupStayedPlayerType 1 [(Nine, Hearts); (Ten, Hearts)]
    let p2 = setupStayedPlayerType 2 [(Five, Spades); (Queen, Clubs); (Four, Spades)]
    let p3 = setupStayedPlayerType 3 [(Jack, Spades); (Five, Spades); (Two, Clubs)]
    let p4 = setupStayedPlayerType 4 [(Jack, Spades); (Ten, Spades)]
    let dealer = setupDealer [(Seven, Hearts); (Jack, Hearts); (Two, Hearts)]
    
    let actual = determineWinners [p1;p2;p3;p4] dealer
    
    actual |> should equal (Players [p4])