open System
open System.Text.RegularExpressions

open BlackJack.Domain
open BlackJack.Game

let printStartMessage =
    printfn "Welcome to BlackJack %s" Environment.NewLine

let weWillDealWithErrorHandlingLater s =
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
    Players: Player list
    Dealer: Dealer
    Deck: Deck
    GameStatus: GameStatus
}

// TODO: Next step: I'd like to have "Typed" Game-States!

let playerLoop game currentPlayerId =

    let rec promptPlay handstatusInternal handInternal deckInternal =
        printfn "%A Current Hand: %A" currentPlayerId (handInternal |> showHand)
        printfn "%A What do you want to do? (1) Hit or (2) Stand?" currentPlayerId

        let playerChoice = Console.ReadLine().Trim()
        match playerChoice with
        | "1" -> // Hit

            match drawCardToHand (deckInternal, handInternal) with

            | None -> { game with GameStatus = PlayerError }

            | Some (newDeck, newHand) ->
                match getStatus (handstatusInternal, newHand) with
                | HandStatus.Busted _ ->
                    (* Player looses and is removed from game *)
                    printfn "%A Busted! You're out of the game. Your hand: %A" currentPlayerId (showHand newHand)

                    (* remove player from game *)
                    let players =
                        game.Players |> List.filter (fun p -> p.Id <> currentPlayerId)

                    {
                        Players = players
                        Dealer = game.Dealer
                        Deck = newDeck
                        GameStatus = PlayerBusted // TODO: check if this game status is still needed, since we remove busted players
                    }

                | HandStatus.Stayed score ->
                    // recursion
                    promptPlay (HandStatus.Stayed score) newHand newDeck
                | _ ->
                    { game with GameStatus = PlayerError}

        | "2" -> // Stand
            let player = { Id = currentPlayerId; Hand = handInternal; HandStatus = handstatusInternal }
            let players = game.Players |> List.map (fun p ->
                if p.Id = currentPlayerId then player else p)
            {
                Players = players
                Dealer = game.Dealer
                Deck = deckInternal
                GameStatus = PlayerFinished
            }
        | _ ->
            printfn "%A Unknown choice" currentPlayerId
            promptPlay handstatusInternal handInternal deckInternal

    let player = game.Players |> List.find (fun x -> x.Id = currentPlayerId)
    promptPlay player.HandStatus player.Hand game.Deck

let dealerTurn game =
    let dealerResponse = dealerAction { Hand = game.Dealer.Hand; Deck = game.Deck }
    match dealerResponse with
    | DealerResponse.DealerError (error, hand, deck) ->
        {
            Players = game.Players
            Dealer = { Hand = hand; HandStatus = game.Dealer.HandStatus }
            Deck = deck
            GameStatus = DealerError
        }
    | DealerResponse.DealerBusted  (score, hand, deck) ->
        {
            Players = game.Players
            Dealer = { Hand = hand; HandStatus = HandStatus.Busted score }
            Deck = deck
            GameStatus = DealerBusted
        }
    | DealerResponse.DealerStayed (score, hand, deck) ->
        {
            Players = game.Players
            Dealer = { Hand = hand; HandStatus = HandStatus.Stayed score }
            Deck = deck
            GameStatus = DealerFinished
        }

type Winner =
    | Players of Player list
    | Dealer of Dealer
    | Nobody

let determineWinners game =
    printfn "Result is:%s" Environment.NewLine

    (* Side effect printing... *)
    game.Players |> List.iter (fun x -> printfn "%A final hand: %A " x.Id (showHand x.Hand))
    printfn "final dealer hand: %A" (showHand game.Dealer.Hand)

    let (pWin: Player list, pLoos: Player list) =
        game.Players |> splitPlayers

    // TODO:
    Nobody
    
let askForNumberOfPlayers =
    printfn "How many players (number between 1 and 3)?"
    let input = Console.ReadLine().Trim()

    let tryToNumberOfPlayers s =
        let numberCheck = Regex("^(1|2|3)$")
        let strContainsOnlyValidNumbers (s:string) = numberCheck.IsMatch s
        let isValidNumberOfPlayers = strContainsOnlyValidNumbers input
        match isValidNumberOfPlayers with
        | true -> Some (NumberOfPlayers (System.Int32.Parse input))
        | false -> None

    tryToNumberOfPlayers input

[<EntryPoint>]
let main argv =
    printStartMessage

    let initialDeck = createDeck
    let maybeNumberOfPlayers = askForNumberOfPlayers
    match maybeNumberOfPlayers with
    | None -> weWillDealWithErrorHandlingLater "invalid number of players"

    | Some numberOfPlayers ->
        let maybeInitializedPlayers = tryInitializePlayers numberOfPlayers initialDeck
        match maybeInitializedPlayers with
        | None -> weWillDealWithErrorHandlingLater "problem initializing players"
        | Some (players, deckAfterAllPlayersHaveBeenInitialized) ->
            let maybeInitializedDealer = trySetupDealer drawCard deckAfterAllPlayersHaveBeenInitialized
            match maybeInitializedDealer with
            | None -> weWillDealWithErrorHandlingLater "problem initializing dealer"
            | Some (dealer, deckAfterDealerInitialization) ->

                let initialGameState = {
                    Players = players
                    Dealer = dealer
                    Deck = deckAfterDealerInitialization
                    GameStatus = Started
                }

                let gameAfterAllPlayersFinished =
                    initialGameState.Players
                    |> List.map (fun p -> p.Id)
                    |> List.fold playerLoop initialGameState

                let gameAfterDealerFinished = dealerTurn gameAfterAllPlayersFinished
                printfn "Winner is %A" (determineWinners gameAfterDealerFinished)
                ()

    0 // return an integer exit code
