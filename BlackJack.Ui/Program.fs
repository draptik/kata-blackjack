open System

open BlackJack.Domain
open BlackJack.Game

let printStartMessage =
    printfn "Welcome to BlackJack %s" Environment.NewLine

let printShouldNeverHappen s =
    printfn "Should never happen unless the deck has less than 2 cards %s" s

type PlayerWins = Undefined
type HouseWins = Undefined

type GameStatus = Started | PlayerPlaying | PlayerBusted | PlayerFinished | DealerError2 | DealerBusted | DealerFinished

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
        | Some (card, deck) -> 
            let newHand = card :: gameState.Player.Hand
            let status = getStatus (gameState.Player.Status, newHand)
            let gameStatus =
                match status with
                | Status.Busted _ -> PlayerBusted
                | Status.Stayed _ -> PlayerPlaying
                | _ -> PlayerPlaying
            {
                Player = { Id = gameState.Player.Id; Hand = newHand; Status = status }
                Dealer = gameState.Dealer
                Deck = deck
                GameStatus = gameStatus
            }
    | PlayerStays -> { gameState with GameStatus = PlayerFinished }
    | DealerPlays ->
        let dealerResponse = dealerAction { Hand = gameState.Dealer.Hand; Deck = gameState.Deck }
        match dealerResponse with
        | DealerResponse.DealerError (error, hand, deck) -> 
            {
                Player = gameState.Player
                Dealer = { Hand = hand; Status = gameState.Dealer.Status }
                Deck = deck
                GameStatus = DealerError2
            }
        | DealerResponse.DealerBusted  (score, hand, deck) -> 
            {
                Player = gameState.Player
                Dealer = { Hand = hand; Status = Status.Busted score }
                Deck = deck
                GameStatus = DealerBusted
            }
        | DealerResponse.DealerStayed (score, hand, deck) -> 
            {
                Player = gameState.Player
                Dealer = { Hand = hand; Status = Stayed score }
                Deck = deck
                GameStatus = DealerFinished
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

            let initialGameState = {
                Player = player
                Dealer = dealer
                Deck = deckAfterDealerInitialization
                GameStatus = Started
            }

            printfn "player hand: %A" player.Hand
            printfn "dealer hand: %A" dealer.Hand

            let result = update initialGameState PlayerHits
            match result.GameStatus with
            | DealerFinished -> printfn "dealer finished"
            | PlayerFinished -> printfn "player finished"
            | _ -> printfn "%A" result.GameStatus
            
            ()

    0 // return an integer exit code
