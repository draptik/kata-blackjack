open System

open BlackJack.Domain
open BlackJack.Game

let printStartMessage =
    printfn "Welcome to BlackJack %s" Environment.NewLine

let weWillDealWithErrorHandingLater s =
    printfn "Should never happen unless the deck has less than 2 cards %s" s

type GameStatus = 
    | Started 
    | PlayerBusted
    | PlayerFinished
    | PlayerError 
    | DealerError 
    | DealerBusted 
    | DealerFinished

type Game = {
    Player: Player
    Dealer: Dealer
    Deck: Deck
    GameStatus: GameStatus
}

// TODO: Next step: I'd like to have "Typed" Game-States!

let playerLoop game playerId =

    let rec promptPlay handstatusInternal handInternal deckInternal =
        printf "Current Hand:"
        printfn "%A" handInternal
        printfn "What do you want to do? (1) Hit or (2) Stand?"

        let playerChoice = Console.ReadLine().Trim()
        match playerChoice with
        | "1" -> 
            
            match drawCardToHand (deckInternal, handInternal) with
            | None -> { game with GameStatus = PlayerError }

            | Some (newDeck, newHand) ->
                match getStatus (handstatusInternal, newHand) with
                | HandStatus.Busted score -> 
                    {
                        Player = { 
                            Id = playerId
                            HandStatus = handstatusInternal
                            Hand = newHand
                         }
                        Dealer = game.Dealer
                        Deck = newDeck
                        GameStatus = PlayerBusted
                    }

                | HandStatus.Stayed score -> 
                    // recursion
                    promptPlay (HandStatus.Stayed score) newHand newDeck
                | _ -> 
                    { game with GameStatus = PlayerError}

        | "2" -> 
            {
                Player = { 
                    Id = playerId
                    HandStatus = handstatusInternal
                    Hand = handInternal
                 }
                Dealer = game.Dealer
                Deck = deckInternal
                GameStatus = PlayerFinished
            }
        | _ ->
            printfn "Unknown choice"
            promptPlay handstatusInternal handInternal deckInternal

    let player = game.Player // add filter here for multiple players here?
    promptPlay player.HandStatus player.Hand game.Deck

let dealerTurn game =
    let dealerResponse = dealerAction { Hand = game.Dealer.Hand; Deck = game.Deck }
    match dealerResponse with
    | DealerResponse.DealerError (error, hand, deck) -> 
        {
            Player = game.Player
            Dealer = { Hand = hand; HandStatus = game.Dealer.HandStatus }
            Deck = deck
            GameStatus = DealerError
        }
    | DealerResponse.DealerBusted  (score, hand, deck) -> 
        {
            Player = game.Player
            Dealer = { Hand = hand; HandStatus = HandStatus.Busted score }
            Deck = deck
            GameStatus = DealerBusted
        }
    | DealerResponse.DealerStayed (score, hand, deck) -> 
        {
            Player = game.Player
            Dealer = { Hand = hand; HandStatus = HandStatus.Stayed score }
            Deck = deck
            GameStatus = DealerFinished
        }

let outputWinner game =
    printfn "Result is:%s" Environment.NewLine
    
    printfn "final player hand: %A" game.Player.Hand
    printfn "final dealer hand: %A" game.Dealer.Hand

    // mmh this method can have invalid Hand (ie "Busted"). TODO More Typing required.
    // found another type hole. Need another type!

    let playerScore = calcScore game.Player.Hand
    let dealerScore = calcScore game.Dealer.Hand

    if playerScore >= dealerScore then
        printfn "Player wins!"
    else
        printfn "Dealer wins!"

let askForNumberOfPlayers =
    printfn "How many players (number between 1 and 3)?"
    let input = Console.ReadLine().Trim()
    // is number? is in range?
    2 // TODO implement
    
// must return game option    
// let initializePlayers numberOfPlayers =
//     trySetupPlayer

[<EntryPoint>]
let main argv =
    printStartMessage

    let deck = createDeck
    let numberOfPlayers = askForNumberOfPlayers
    let maybeInitializedPlayer = trySetupPlayer drawCard (PlayerId 1) deck
    match maybeInitializedPlayer with
    | None -> weWillDealWithErrorHandingLater "1"
    | Some (player, deckAfterPlayerInitialization) ->
        let maybeInitializedDealer = trySetupDealer drawCard deckAfterPlayerInitialization
        match maybeInitializedDealer with
        | None -> weWillDealWithErrorHandingLater "2"
        | Some (dealer, deckAfterDealerInitialization) ->

            printfn "initial player hand: %A" player.Hand
            printfn "initial dealer hand: %A" dealer.Hand

            let initialGameState = {
                Player = player
                Dealer = dealer
                Deck = deckAfterDealerInitialization
                GameStatus = Started
            }

            let gameAfterPlayerFinished = playerLoop initialGameState (PlayerId 1)
            let gameAfterDealerFinished = dealerTurn gameAfterPlayerFinished

            printfn "final player hand: %A" gameAfterDealerFinished.Player.Hand
            printfn "final dealer hand: %A" gameAfterDealerFinished.Dealer.Hand

            outputWinner gameAfterDealerFinished

            ()

    0 // return an integer exit code
