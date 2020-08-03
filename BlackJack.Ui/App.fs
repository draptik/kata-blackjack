module BlackJack.Ui.App

open BlackJack.Domain
open BlackJack.Ui.Print
open BlackJack.Ui.Interaction

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
                Players = game.Players
                Dealer = dealer
                Deck = modifiedDeck
            })
    | _ -> Error ErrorDealerCanOnlyBeDealtCardsAfterPlayersHaveBeenDealt

let rec play (player: Player) (game: Game) =
    printCurrentHand player
    let playerChoice = promptPlayerToChooseBetweenHitOrStand player.Id
    
    match playerChoice.GetValue() with
    | Hit ->
        (game.Deck, player.Hand)
        |> drawCardToHand 
        |> Result.bind (
            fun (newDeck, newHand) ->
                match getStatus { Cards = newHand.Cards; Status = player.Hand.Status } with
                | Busted _ ->
                    printBustedMessage player newHand.Cards
                    let playersWithoutBustedPlayer = game.Players |> List.filter (fun p -> p.Id <> player.Id)
                    Ok {
                        Players = playersWithoutBustedPlayer
                        Dealer = game.Dealer
                        Deck = newDeck
                    }
                | Stayed score ->
                    play
                        { player with Hand = { Cards = newHand.Cards; Status = (Stayed score) }}
                        { game with Deck = newDeck } // recursion
                | _ ->
                    dumpGameStateForDebugging game
                    Error ErrorPlayerPlayingInvalidHandState)
    | Stand ->
        let activeHandStatus =
            match player.Hand.Status with
            | CardsDealt -> Stayed (calcScore player.Hand.Cards) // TODO missing state? this prevents false-blackjack state
            | handStatus -> handStatus
        
        let player = {
            Id = player.Id
            Hand = {
                Cards = player.Hand.Cards
                Status = activeHandStatus
            }
        }
        
        let players =
            game.Players
            |> List.map (fun p -> if p.Id = player.Id then player else p)
        
        Ok {
            Players = players
            Dealer = game.Dealer
            Deck = game.Deck
        }

let playerLoop game player =
    play {player with Hand = player.Hand |> handPlaying} game

let playerLoops gameState =
    match gameState with
    | CardsDealtToDealer game -> 
        game.Players 
        |> List.fold 
            (fun gameM player -> 
                gameM |> Result.bind (fun currentGame -> playerLoop currentGame player))
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

let determineWinners gameState =
    match gameState with
    | DealerFinished game ->
        printWinnerHeader
        game.Players |> List.iter printFinalPlayerHand
        game.Dealer |> printFinalDealerHand
        
        determineWinners game.Players game.Dealer |> Ok
        
    | _ -> Error ErrorWinnerCanOlyBeDeterminedAfterDealerIsFinished
    
let askForNumberOfPlayers () =
    promptForNumberOfPlayers.GetValue() |> Ok
