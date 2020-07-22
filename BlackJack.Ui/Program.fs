open System
open System.Text.RegularExpressions

open BlackJack.Domain

let printStartMessage =
    printfn "Welcome to BlackJack %s" Environment.NewLine

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
            | Error _ -> { game with GameStatus = PlayerError }
            | Ok (newDeck, newHand) ->
                match getStatus (handstatusInternal, newHand) with
                | HandStatus.Busted _ ->
                    (* Player looses and is removed from game *)
                    printfn "%A Busted! You're out of the game. Your hand: %A" currentPlayerId (showHand newHand)
                    (* remove player from game *)
                    let playersWithoutBustedPlayer = game.Players |> List.filter (fun p -> p.Id <> currentPlayerId)
                    {
                        Players = playersWithoutBustedPlayer
                        Dealer = game.Dealer
                        Deck = newDeck
                        GameStatus = PlayerBusted // TODO: check if this game status is still needed, since we remove busted players
                    }
                | HandStatus.Stayed score -> promptPlay (HandStatus.Stayed score) newHand newDeck // recursion
                | _ -> { game with GameStatus = PlayerError}
        | "2" -> // Stand
            let player = { Id = currentPlayerId; Hand = handInternal; HandStatus = handstatusInternal }
            let players = game.Players |> List.map (fun p -> if p.Id = currentPlayerId then player else p)
            {
                Players = players
                Dealer = game.Dealer
                Deck = deckInternal
                GameStatus = PlayerFinished
            }
        | _ ->
            printfn "%A Unknown choice" currentPlayerId
            promptPlay handstatusInternal handInternal deckInternal

    let player = game.Players |> List.find (fun p -> p.Id = currentPlayerId)
    promptPlay player.HandStatus player.Hand game.Deck

let dealerTurn game =
    let dealerPlayResult = dealerPlays { Hand = game.Dealer.Hand; Deck = game.Deck }
    match dealerPlayResult with
    | DealerPlayResult.DealerError (error, hand, deck) ->
        {
            Players = game.Players
            Dealer = { Hand = hand; HandStatus = game.Dealer.HandStatus }
            Deck = deck
            GameStatus = DealerError
        }
    | DealerPlayResult.DealerBusted  (score, hand, deck) ->
        {
            Players = game.Players
            Dealer = { Hand = hand; HandStatus = HandStatus.Busted score }
            Deck = deck
            GameStatus = DealerBusted
        }
    | DealerPlayResult.DealerStayed (score, hand, deck) ->
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

    let (winningPlayers: Player list, _) = game.Players |> splitPlayers

    let winningPlayerScore = calcScore winningPlayers.[0].Hand
    let dealerScore = calcScore game.Dealer.Hand

    match (winningPlayerScore, dealerScore) with
    | (p, d) when p = d -> Nobody
    | (p, d) when p > d -> Players (winningPlayers)
    | _ -> Dealer (game.Dealer)
    
let askForNumberOfPlayers =
    printfn "How many players (number between 1 and 7)?"
    let input = Console.ReadLine().Trim()

    let tryToNumberOfPlayers s =
        let numberCheck = Regex("^(1|2|3|4|5|6|7)$")
        let strContainsOnlyValidNumbers (s:string) = numberCheck.IsMatch s
        let isValidNumberOfPlayers = strContainsOnlyValidNumbers s
        match isValidNumberOfPlayers with
        | true -> Ok (NumberOfPlayers (System.Int32.Parse s))
        | false -> Error ErrorAskingForNumberOfPlayers

    tryToNumberOfPlayers input

[<EntryPoint>]
let main argv =
    printStartMessage

    let initialDeck = createDeck
    let maybeNumberOfPlayers = askForNumberOfPlayers

    maybeNumberOfPlayers
    |> Result.bind (fun numberOfPlayers -> tryInitializePlayers numberOfPlayers initialDeck)
    |> Result.bind (fun (players, deck) -> 
        let maybeInitializedDealer = trySetupDealer drawCard deck
        match maybeInitializedDealer with
        | Error e -> Error e
        | Ok (dealer, deckAfterDealerSetup) ->
            Ok {
                Players = players
                Dealer = dealer
                Deck = deckAfterDealerSetup
                GameStatus = Started
            })
    |> Result.bind (fun game ->
        let gameAfterAllPlayersFinished =
            game.Players
            |> List.map (fun p -> p.Id)
            |> List.fold playerLoop game // <- player interaction happens here!

        gameAfterAllPlayersFinished 
        |> dealerTurn 
        |> determineWinners 
        |> Ok)
    |> Result.map (fun (winners) ->
        match winners with
        | Nobody -> printfn "Nobody won ;-("
        | Dealer d -> printfn "Dealer won! %A" (showHand d.Hand)
        | Players ps -> printfn "The following players won: %A" (ps |> List.map (fun x -> x.Id)))
    |> Result.mapError (fun error -> printfn "Ups: %A" error)
    |> ignore

    0 // return an integer exit code
