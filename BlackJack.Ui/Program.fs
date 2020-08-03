open BlackJack.Domain
open BlackJack.Ui.Print
open BlackJack.Ui.App

[<EntryPoint>]
let main argv =
    printStartMessage

    ()
    |> askForNumberOfPlayers
    |> Result.bind initializePlayers
    |> Result.bind initializeDealer
    |> Result.bind playerLoops
    |> Result.bind dealerTurn
    |> Result.bind determineWinners
    |> Result.map (fun winners ->
        match winners with
        | Dealer d -> printWinDealerWins d
        | Players ps -> printWinPlayers ps
        | Nobody -> printWinNobody)
    |> Result.mapError printError
    |> ignore

    0 // return an integer exit code
