open System

open BlackJack.Domain
open BlackJack.Game

let printStartMessage =
    printfn "Welcome to BlackJack %s" Environment.NewLine

let printShouldNeverHappen s =
    printfn "Should never happen unless the deck has less than 2 cards %s" s

// let showStatus s status =
//     match status with
//     | Status.BlackJack -> printfn "%s BlackJack!" s
//     | Status.Busted (x) -> printfn "%s %A" s x
//     | Status.Stayed (x) -> printfn "%s %A" s x
//     | Status.CardsDealt -> printfn "%s ??" s

type PlayerWins = Undefined
type HouseWins = Undefined

type GameState = {
    Player: Player
    Dealer: Dealer
    Deck: Deck
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

            let gameState = {
                Player = player
                Dealer = dealer
                Deck = deckAfterDealerInitialization
            }

            printfn "player hand: %A" player.Hand
            printfn "dealer hand: %A" dealer.Hand


            ()

    0 // return an integer exit code
