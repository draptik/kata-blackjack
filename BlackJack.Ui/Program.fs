open System

open BlackJack.Domain
open BlackJack.Game

let printStartMessage =
    printfn "Welcome to BlackJack %s" Environment.NewLine

[<EntryPoint>]
let main argv =
    printStartMessage

    let deck = createDeck
    let initializedPlayerOpt = setupPlayer drawCard (PlayerId 1) deck
    // match initializedPlayerOpt with
    // | None -> 0
    // | Some (player, deck) -> // dealer is still missing...


    0 // return an integer exit code
