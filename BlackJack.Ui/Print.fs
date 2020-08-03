module BlackJack.Ui.Print

open System
open BlackJack.Domain
open BlackJack.Ui.Display

let printStartMessage =
    printfn "Welcome to BlackJack %s" Environment.NewLine

let printCurrentHand player =
    printfn "%A Current Hand: %A" player.Id (showHand player.Hand)

let printBustedMessage player bustedCards =
    printfn "%A Busted! You're out of the game. Your hand: %A"
        player.Id
        (showHand { Cards = bustedCards; Status = player.Hand.Status})

let dumpGameStateForDebugging game =
    printfn "ups: %A" game // dump complete game state in case things go wrong
    
let printWinnerHeader =    
    printfn "Result is:%s" Environment.NewLine
    
let printFinalPlayerHand player =
    printfn "%A final hand: %A " player.Id (showHand player.Hand)

let printFinalDealerHand (dealer: Dealer) =
    printfn "final dealer hand: %A" (showHand dealer.Hand)
    
let printWinNobody =
    printfn "Nobody won ;-("

let printWinDealerWins (dealer: Dealer) =
    printfn "Dealer won! %A" (showHand dealer.Hand)
    
let printWinPlayers players =
    printfn "The following players won: %A" (players |> List.map (fun x -> x.Id))
    
let printError error =
    printfn "Ups: %A" error
