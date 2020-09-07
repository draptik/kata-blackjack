module BlackJack.Ui.Game

open BlackJack.Domain
open BlackJack.GamePlay
open BlackJack.Ui.Print
open BlackJack.Ui.Interaction

let initializePlayers numberOfPlayers =
    tryInitializePlayers numberOfPlayers createDeck
    |> Result.bind (fun (players, modifiedDeck) -> 
        Ok <| CardsDealtToPlayers { 
            Players = players
            Deck = modifiedDeck 
        })

let initializeDealer gameState =
    match gameState with
    | CardsDealtToPlayers game ->
        trySetupDealer drawCardFromDeck game.Deck
        |> Result.bind (fun (dealer, modifiedDeck) ->
            Ok <| CardsDealtToDealer {
                PlayerTypes = game.Players
                Dealer = dealer
                Deck = modifiedDeck
            })
    | _ -> Error ErrorDealerCanOnlyBeDealtCardsAfterPlayersHaveBeenDealt


let rec play (playerType: PlayerType) (game: Game) =
    
    printCurrentHand playerType
    let playerChoice = playerType |> getPlayerId |> promptPlayerToChooseBetweenHitOrStand 
    
    match playerChoice.GetValue() with
    | Hit ->
        (game.Deck, playerType |> getPlayersCards)
        |> drawCardToHand 
        |> Result.bind (
            fun (newDeck, newHand) ->
                let playerTypeWithNewlyDealtHand = playerType |> setHandForPlayerType <| newHand
                let updatedPlayerType = getStatus playerTypeWithNewlyDealtHand
                
                match updatedPlayerType with
                | BustedPlayer _ ->
                    printBustedMessage updatedPlayerType newHand
                    let playersWithoutBustedPlayer =
                        game.PlayerTypes
                        |> List.filter (fun pt -> areNotEqualPlayerIds pt playerType)
                    Ok {
                        PlayerTypes = playersWithoutBustedPlayer
                        Dealer = game.Dealer
                        Deck = newDeck
                    }
                | StayedPlayer p ->
                    let stayedPlayer = StayedPlayer { p with Hand = newHand }
                    let updatedPlayers =
                        game.PlayerTypes
                        |> List.map (fun x -> if (getPlayerId x) = p.Id then stayedPlayer else x)
                    play
                        stayedPlayer
                        {
                            PlayerTypes = updatedPlayers
                            Dealer = game.Dealer
                            Deck = newDeck
                        } // recursion
                | InitializedPlayer p ->
                    let stayedPlayer = StayedPlayer { p with Hand = newHand }
                    let updatedPlayers =
                        game.PlayerTypes
                        |> List.map (fun x -> if (getPlayerId x) = p.Id then stayedPlayer else x)
                    Ok {
                        PlayerTypes = updatedPlayers
                        Dealer = game.Dealer
                        Deck = newDeck
                    }
                | _ ->
                    dumpGameStateForDebugging game
                    Error ErrorPlayerPlayingInvalidHandState)
    | Stand ->
        let playerTypes =
            game.PlayerTypes
            |> List.map (fun pt ->
                if areEqualPlayerIds pt playerType then
                    StayedPlayer { Id = playerType |> getPlayerId; Hand = playerType |> getPlayersCards }
                else
                    pt)
        
        Ok {
            PlayerTypes = playerTypes
            Dealer = game.Dealer
            Deck = game.Deck
        }

let playerLoop game playerType =
    play playerType game

let playerLoops gameState =
    match gameState with
    | CardsDealtToDealer game -> 
        game.PlayerTypes 
        |> List.fold 
            (fun gameM player -> 
                gameM |> Result.bind (fun currentGame -> playerLoop currentGame player))
            (Ok game)
        |> Result.bind (fun game -> 
            Ok <| PlayersFinished {
                PlayerTypes = game.PlayerTypes
                Dealer = game.Dealer
                Deck = game.Deck
            })
    | _ -> Error ErrorPlayerLoopsCanOlyStartAfterDealerHasBeenDealt

let dealerTurn gameState =
    match gameState with
    | PlayersFinished game ->
        let dealerPlayResult = dealerPlays { Cards = game.Dealer.Hand; Deck = game.Deck }
        match dealerPlayResult with
        | ErrorDuringPlay -> Error ErrorDuringDealerPlay
        | DealerBusted  (_, handCards, deck) ->
            Ok <| DealerFinished {
                PlayerTypes = game.PlayerTypes
                Dealer = { Hand = handCards }
                Deck = deck
            }
        | DealerStayed (_, handCards, deck) ->
            Ok <| DealerFinished {
                PlayerTypes = game.PlayerTypes
                Dealer = { Hand = handCards }
                Deck = deck
            }
    | _ -> Error ErrorDealerTurnCanOnlyStartAfterAllPlayersFinished

let determineWinners gameState =
    match gameState with
    | DealerFinished game ->
        printWinnerHeader
        game.PlayerTypes |> List.iter printFinalPlayerHand
        game.Dealer |> printFinalDealerHand
        
        determineWinners game.PlayerTypes game.Dealer |> Ok
        
    | _ -> Error ErrorWinnerCanOlyBeDeterminedAfterDealerIsFinished
    
let askForNumberOfPlayers () =
    promptForNumberOfPlayers.GetValue() |> Ok
