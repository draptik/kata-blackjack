module BlackJack.Ui.Print

open System
open BlackJack.Domain
open BlackJack.Ui.Display

let printStartMessage =
    printfn "Welcome to BlackJack %s" Environment.NewLine

let printCurrentHand player =
    match player with
    | PlayerType.BustedPlayer p -> printfn "%A %A Current Hand: %s " p.Id (calcScore p.Hand) (showHand p.Hand) 
    | PlayerType.InitializedPlayer p -> printfn "%A %A Current Hand: %s " p.Id (calcScore p.Hand) (showHand p.Hand) 
    | PlayerType.StayedPlayer p -> printfn "%A %A Current Hand: %s " p.Id (calcScore p.Hand) (showHand p.Hand) 
    | PlayerType.BlackJackedPlayer p -> printfn "%A %A Current Hand: %s " p.Id (calcScore p.Hand) (showHand p.Hand) 

let printBustedMessage playerType =
    match playerType with
    | BustedPlayer p ->
        printfn "%A Busted! You're out of the game. %A Your hand: %s "
            p.Id
            (calcScore p.Hand)
            (showHand p.Hand)
    | _ ->
        ()

let dumpGameStateForDebugging game handCards =
    printfn "ups: %s " (showHand handCards)
    printfn "ups: %A" game // dump complete game state in case things go wrong
    
let printWinnerHeader =    
    printfn "Result is:%s" Environment.NewLine
    
let printFinalPlayerHand (player:PlayerType) =
    printfn "%A %A final hand: %s "
        (player |> getPlayerId)
        (calcScore (player |> getPlayersCards))
        (player |> getPlayersCards |> showHand)

let printFinalDealerHand (dealer: Dealer) =
    printfn "final dealer score %A hand: %s "
        (calcScore dealer.Hand)
        (showHand dealer.Hand)
    
let printWinNobody =
    printfn "Nobody won ;-("

let printWinDealerWins (dealer: Dealer) =
    printfn "Dealer won! %A %s " (calcScore dealer.Hand) (showHand dealer.Hand)
    
let printWinPlayers players =
    printfn "The following players won: %A" (players |> List.map (fun x -> x |> getWinningPlayerId))
    
let printError error =
    printfn "Ups: %A" error
