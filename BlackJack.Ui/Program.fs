﻿open System
open System.Text.RegularExpressions

open BlackJack.Domain
open BlackJack.Ui.Display

let printStartMessage =
    printfn "Welcome to BlackJack %s" Environment.NewLine

type GamePlayersWithDealtCards = {
    Players: Player list
    Deck: Deck
}

type Game = {
    Players: Player list
    Dealer: Dealer
    Deck: Deck
}

type GameState =
    | InitialGameState
    | CardsDealtToPlayers of GamePlayersWithDealtCards
    | CardsDealtToDealer of Game
    | PlayersPlaying of Game
    | PlayersFinished of Game
    | DealerPlaying of Game
    | DealerFinished of Game

let tryInitializePlayersInGame numberOfPlayers =
    tryInitializePlayers numberOfPlayers createDeck
    |> Result.bind (fun (players, modifiedDeck) -> 
        Ok <| CardsDealtToPlayers { 
            Players = players
            Deck = modifiedDeck 
        })

let trySetupDealerInGame gameState =
    match gameState with
    | CardsDealtToPlayers game ->
        trySetupDealer drawCardFromDeck game.Deck
        |> Result.bind (fun (dealer, modifiedDeck) ->
            Ok <| CardsDealtToDealer {
                Players = game.Players
                Dealer = dealer
                Deck = modifiedDeck
            })
    | _ -> Error ErrorDealerCanOnlyBeDealtCardsAfterPlayersHaveBeenDealt

let playerLoop game currentPlayerId =
    let rec promptPlay (handInternal: Hand) deckInternal =
        printfn "%A Current Hand: %A" currentPlayerId (showHand handInternal)
        printfn "%A What do you want to do? (1) Hit or (2) Stand?" currentPlayerId

        let playerChoice = Console.ReadLine().Trim()

        // TODO: Extract the following pattern match (no interaction required)
        match playerChoice with
        | "1" -> // Hit
            match drawCardToHand (deckInternal, handInternal) with
            | Error e -> Error e
            | Ok (newDeck, newHand) ->
                match getStatus { Cards = newHand.Cards; Status = handInternal.Status} with
                // match getStatus (handInternal.Status, newHand.Cards) with
                | Busted _ ->
                    (* Player looses and is removed from game *)
                    printfn "%A Busted! You're out of the game. Your hand: %A" currentPlayerId (showHand { Cards = newHand.Cards; Status = handInternal.Status})
                    (* remove player from game *)
                    let playersWithoutBustedPlayer = game.Players |> List.filter (fun p -> p.Id <> currentPlayerId)
                    Ok {
                        Players = playersWithoutBustedPlayer
                        Dealer = game.Dealer
                        Deck = newDeck
                    }
                | Stayed score -> promptPlay { Cards = newHand.Cards; Status = (Stayed score) } newDeck // recursion
                | _ -> Error ErrorPlayerPlayingInvalidHandState
        | "2" -> // Stand
            let activeHandStatus =
                match handInternal.Status with
                | CardsDealt -> Stayed (calcScore handInternal.Cards)
                | handStatus -> handStatus
            let hand = { Cards = handInternal.Cards; Status = activeHandStatus }
            let player = { Id = currentPlayerId; Hand = hand }
            let players = game.Players |> List.map (fun p -> if p.Id = currentPlayerId then player else p)
            Ok {
                Players = players
                Dealer = game.Dealer
                Deck = deckInternal
            }
        | _ ->
            printfn "%A Unknown choice" currentPlayerId
            promptPlay handInternal deckInternal // recursion

    let player = game.Players |> List.find (fun p -> p.Id = currentPlayerId)
    promptPlay player.Hand game.Deck

let playerLoops gameState =
    match gameState with
    | CardsDealtToDealer game -> 
        game.Players 
        |> List.map (fun p -> p.Id)
        |> List.fold 
            (fun gameM playerId -> 
                gameM |> Result.bind (fun currentGame -> playerLoop currentGame playerId))
            (Ok game)
        |> Result.bind (fun game -> 
            Ok <| PlayersFinished {
                Players = game.Players
                Dealer = game.Dealer
                Deck = game.Deck
            })
    | _ -> Error ErrorPlayerLoopsCanOlyStartAfterDealerHasBeenDealt

let dealerTurn gameState =
    match gameState with
    | PlayersFinished game ->
        let dealerPlayResult = dealerPlays { Cards = game.Dealer.Hand.Cards; Deck = game.Deck }
        match dealerPlayResult with
        | ErrorDuringPlay -> Error ErrorDuringDealerPlay
        | DealerBusted  (score, handCards, deck) ->
            Ok <| DealerFinished {
                Players = game.Players
                Dealer = { Hand = { Cards = handCards; Status = Busted score }}
                Deck = deck
            }
        | DealerStayed (score, handCards, deck) ->
            Ok <| DealerFinished {
                Players = game.Players
                Dealer = { Hand = { Cards = handCards; Status = Stayed score }}
                Deck = deck
            }
    | _ -> Error ErrorDealerTurnCanOnlyStartAfterAllPlayersFinished

let determineWinnersIO gameState =
    match gameState with
    | DealerFinished game ->
        printfn "Result is:%s" Environment.NewLine
        game.Players |> List.iter (fun player -> printfn "%A final hand: %A " player.Id (showHand player.Hand))
        printfn "final dealer hand: %A" (showHand game.Dealer.Hand)
        
        determinWinner game.Players game.Dealer |> Ok
    | _ -> Error ErrorWinnerCanOlyBeDeterminedAfterDealerIsFinished
    
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

    askForNumberOfPlayers
    |> Result.bind tryInitializePlayersInGame
    |> Result.bind trySetupDealerInGame
    |> Result.bind playerLoops
    |> Result.bind dealerTurn
    |> Result.bind determineWinnersIO
    |> Result.map (fun winners ->
        match winners with
        | Nobody -> printfn "Nobody won ;-("
        | Dealer d -> printfn "Dealer won! %A" (showHand d.Hand)
        | Players ps -> printfn "The following players won: %A" (ps |> List.map (fun x -> x.Id)))
    |> Result.mapError (fun error -> printfn "Ups: %A" error)
    |> ignore

    0 // return an integer exit code
