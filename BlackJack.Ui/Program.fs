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

type GameStatus = Started | PlayerPlaying | PlayerBusted | PlayerFinished | DealerPlaying | DealerBusted | DealerFinished

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

let update gameState action =
    match action with
    | PlayerHits -> 
        match (drawCard gameState.Deck) with
        | None -> gameState
        | Some (card, d) -> 
            let newHand = card :: gameState.Player.Hand

            let status = getStatus (gameState.Player.Status, newHand)
            let gameStatus =
                match status with
                | Status.Busted _ -> PlayerBusted
                | Status.Stayed _ -> PlayerPlaying
                | _ -> PlayerPlaying

            let p = { 
                Id = gameState.Player.Id
                Hand = newHand
                Status = status 
            }
            
            {
                Player = p
                Dealer = gameState.Dealer
                Deck = d
                GameStatus = gameStatus
            }
    | PlayerStays -> { gameState with GameStatus = PlayerFinished }
    | DealerPlays ->
        match (drawCard gameState.Deck) with
        | None -> gameState
        | Some (card, d) -> 
            let newHand = card :: gameState.Dealer.Hand

            let status = getStatus (gameState.Dealer.Status, newHand)
            let gameStatus =
                match status with
                | Status.Busted _ -> DealerBusted
                | Status.Stayed _ -> DealerPlaying
                | _ -> DealerPlaying

            let dealer = { 
                Hand = newHand
                Status = status 
            }
            
            {
                Player = gameState.Player
                Dealer = dealer
                Deck = d
                GameStatus = gameStatus
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
                GameStatus = Started
            }

            printfn "player hand: %A" player.Hand
            printfn "dealer hand: %A" dealer.Hand


            ()

    0 // return an integer exit code
