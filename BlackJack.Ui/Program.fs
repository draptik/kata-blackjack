open System

open BlackJack.Domain
open BlackJack.Game

let printStartMessage =
    printfn "Welcome to BlackJack %s" Environment.NewLine

let printShouldNeverHappen s =
    printfn "Should never happen unless the deck has less than 2 cards %s" s

type PlayerWins = Undefined
type HouseWins = Undefined

type GameStatus = Started | PlayerPlaying | PlayerBusted of Score | PlayerFinished of (Hand * Deck) | PlayerError | DealerError2 | DealerBusted | DealerFinished

type GameState = {
    Player: Player
    Dealer: Dealer
    Deck: Deck
    GameStatus: GameStatus
}

type Action =
    | PlayerHits
    | PlayerStays
    | DealerPlays

// let update gameState action =
//     match action with
//     | PlayerHits -> 
//         match (drawCard gameState.Deck) with
//         | None -> gameState
//         | Some (card, deck) -> 
//             let newHand = card :: gameState.Player.Hand
//             let status = getStatus (gameState.Player.Status, newHand)
//             let gameStatus =
//                 match status with
//                 | Status.Busted _ -> PlayerBusted
//                 | Status.Stayed _ -> PlayerPlaying
//                 | _ -> PlayerPlaying
//             {
//                 Player = { Id = gameState.Player.Id; Hand = newHand; Status = status }
//                 Dealer = gameState.Dealer
//                 Deck = deck
//                 GameStatus = gameStatus
//             }
//     | PlayerStays -> { gameState with GameStatus = PlayerFinished }
//     | DealerPlays ->
//         let dealerResponse = dealerAction { Hand = gameState.Dealer.Hand; Deck = gameState.Deck }
//         match dealerResponse with
//         | DealerResponse.DealerError (error, hand, deck) -> 
//             {
//                 Player = gameState.Player
//                 Dealer = { Hand = hand; Status = gameState.Dealer.Status }
//                 Deck = deck
//                 GameStatus = DealerError2
//             }
//         | DealerResponse.DealerBusted  (score, hand, deck) -> 
//             {
//                 Player = gameState.Player
//                 Dealer = { Hand = hand; Status = Status.Busted score }
//                 Deck = deck
//                 GameStatus = DealerBusted
//             }
//         | DealerResponse.DealerStayed (score, hand, deck) -> 
//             {
//                 Player = gameState.Player
//                 Dealer = { Hand = hand; Status = Stayed score }
//                 Deck = deck
//                 GameStatus = DealerFinished
//             }

type PlayerLoopResult = PlayerLoopError | PlayerLoopBusted of Score | PlayerLoopFinished of (Hand * Deck)

let playerLoop player deck =
    let rec promptPlay handstatusInternal handInternal deckInternal =
        printf "Current Hand:"
        printfn "%A" handInternal
        printfn "What do you want to do? (1) Hit or (2) Stand?"
        let playerChoice = Console.ReadLine().Trim()
        match playerChoice with
        | "1" -> 
            match drawCardToHand (deckInternal, handInternal) with
            | None -> PlayerLoopError
            | Some (newDeck, newHand) ->
                match getStatus (handstatusInternal, newHand) with
                | HandStatus.Busted score -> PlayerLoopBusted score
                | HandStatus.Stayed score -> promptPlay (HandStatus.Stayed score) newHand newDeck
                | _ -> PlayerLoopError
        | "2" -> PlayerLoopFinished (handInternal, deckInternal)
        | _ ->
            printfn "Unknown choice"
            promptPlay handstatusInternal handInternal deckInternal
    promptPlay player.HandStatus player.Hand deck

let dealerTurn gameState =
    let dealerResponse = dealerAction { Hand = gameState.Dealer.Hand; Deck = gameState.Deck }
    match dealerResponse with
    | DealerResponse.DealerError (error, hand, deck) -> 
        {
            Player = gameState.Player
            Dealer = { Hand = hand; HandStatus = gameState.Dealer.HandStatus }
            Deck = deck
            GameStatus = DealerError2
        }
    | DealerResponse.DealerBusted  (score, hand, deck) -> 
        {
            Player = gameState.Player
            Dealer = { Hand = hand; HandStatus = HandStatus.Busted score }
            Deck = deck
            GameStatus = DealerBusted
        }
    | DealerResponse.DealerStayed (score, hand, deck) -> 
        {
            Player = gameState.Player
            Dealer = { Hand = hand; HandStatus = HandStatus.Stayed score }
            Deck = deck
            GameStatus = DealerFinished
        }

[<EntryPoint>]
let main argv =
    printStartMessage

    let deck = createDeck
    let initializedPlayerOpt = setupPlayer drawCard (PlayerId 1) deck
    match initializedPlayerOpt with
    | None -> printShouldNeverHappen "1"
    | Some (player, deckAfterPlayerInitialization) ->
        let initializedDealerOpt = setupDealer drawCard deckAfterPlayerInitialization
        match initializedDealerOpt with
        | None -> printShouldNeverHappen "2"
        | Some (dealer, deckAfterDealerInitialization) ->

            // let initialGameState = {
            //     Player = player
            //     Dealer = dealer
            //     Deck = deckAfterDealerInitialization
            //     GameStatus = Started
            // }

            printfn "initial player hand: %A" player.Hand
            printfn "initial dealer hand: %A" dealer.Hand

            let playerLoopResult = playerLoop player deckAfterDealerInitialization
            match playerLoopResult with
            | PlayerLoopBusted score -> printfn "playerResult (busted): %A" score
            | PlayerLoopFinished (hand, deckAfterPlayerFinished) -> 
                printfn "playerResult (stayed): %A" hand

            | _ -> printf "end"
            
            // let result = update initialGameState playerAction
            // match result.GameStatus with
            // | DealerFinished -> printfn "dealer finished"
            // | PlayerFinished -> printfn "player finished"
            // | _ -> printfn "%A" result.GameStatus
            
            ()

    0 // return an integer exit code
