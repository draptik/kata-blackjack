module BlackJack.Ui.Print

open System
open BlackJack.Domain
open BlackJack.Ui.Display

let printStartMessage =
    printfn "Welcome to BlackJack %s" Environment.NewLine

let printCurrentHand player =
    match player with
    | BustedPlayer p -> printfn "%A Current Hand: %A" p.Id (showHand p.Hand) 
    | InitializedPlayer p -> printfn "%A Current Hand: %A" p.Id (showHand p.Hand) 
    | StayedPlayer p -> printfn "%A Current Hand: %A" p.Id (showHand p.Hand) 
    | BlackJackedPlayer p -> printfn "%A Current Hand: %A" p.Id (showHand p.Hand) 

let printBustedMessage (player: PlayerType) bustedCards =
    match player with
    | BustedPlayer p ->
        printfn "%A Busted! You're out of the game. Your hand: %A"
            p.Id
            (showHand bustedCards)
    | _ ->
        ()

let dumpGameStateForDebugging game =
    printfn "ups: %A" game // dump complete game state in case things go wrong
    
let printWinnerHeader =    
    printfn "Result is:%s" Environment.NewLine
    
let printFinalPlayerHand (player:PlayerType) =
    printfn "%A final hand: %A " (player |> getPlayerId) (player |> getPlayersCards |> showHand)

let printFinalDealerHand (dealer: Dealer) =
    printfn "final dealer hand: %A" (showHand dealer.Hand)
    
let printWinNobody =
    printfn "Nobody won ;-("

let printWinDealerWins (dealer: Dealer) =
    printfn "Dealer won! %A" (showHand dealer.Hand)
    
let printWinPlayers players =
    printfn "The following players won: %A" (players |> List.map (fun x -> x |> getPlayerId))
    
let printError error =
    printfn "Ups: %A" error
