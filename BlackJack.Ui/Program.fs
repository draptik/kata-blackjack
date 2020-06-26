open System

open BlackJack.Domain
open BlackJack.Game

let printStartMessage =
    printfn "Welcome to BlackJack %s" Environment.NewLine

[<EntryPoint>]
let main argv =
    printStartMessage
    0 // return an integer exit code
