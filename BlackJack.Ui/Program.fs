open System
open System.Text.RegularExpressions

open BlackJack.Domain
open BlackJack.Game

let printStartMessage =
    printfn "Welcome to BlackJack %s" Environment.NewLine

let weWillDealWithErrorHandingLater s =
    printfn "Should never happen unless the deck has less than 2 cards %s" s

type GameStatus = 
    | Started 
    | PlayerBusted
    | PlayerFinished
    | PlayerError 
    | DealerError 
    | DealerBusted 
    | DealerFinished

type Game = {
    Players: Player list
    Dealer: Dealer
    Deck: Deck
    GameStatus: GameStatus
}

// TODO: Next step: I'd like to have "Typed" Game-States!

let playerLoop game playerId =

    let rec promptPlay handstatusInternal handInternal deckInternal =
        printf "Current Hand:"
        printfn "%A" handInternal
        printfn "What do you want to do? (1) Hit or (2) Stand?"

        let playerChoice = Console.ReadLine().Trim()
        match playerChoice with
        | "1" -> 
            
            match drawCardToHand (deckInternal, handInternal) with
            | None -> { game with GameStatus = PlayerError }

            | Some (newDeck, newHand) ->
                match getStatus (handstatusInternal, newHand) with
                | HandStatus.Busted _ ->
                    let bustedPlayer = { Id = playerId; HandStatus = handstatusInternal; Hand = newHand }
                    let players = 
                        game.Players 
                        |> List.map (fun p -> if p.Id = playerId then bustedPlayer else p)

                    {
                        Players = players
                        Dealer = game.Dealer
                        Deck = newDeck
                        GameStatus = PlayerBusted
                    }

                | HandStatus.Stayed score -> 
                    // recursion
                    promptPlay (HandStatus.Stayed score) newHand newDeck
                | _ -> 
                    { game with GameStatus = PlayerError}

        | "2" -> 
            {
                Players = game.Players
                Dealer = game.Dealer
                Deck = deckInternal
                GameStatus = PlayerFinished
            }
        | _ ->
            printfn "Unknown choice"
            promptPlay handstatusInternal handInternal deckInternal

    let player = game.Players |> List.find (fun x -> x.Id = playerId)
    promptPlay player.HandStatus player.Hand game.Deck

let dealerTurn game =
    let dealerResponse = dealerAction { Hand = game.Dealer.Hand; Deck = game.Deck }
    match dealerResponse with
    | DealerResponse.DealerError (error, hand, deck) -> 
        {
            Players = game.Players
            Dealer = { Hand = hand; HandStatus = game.Dealer.HandStatus }
            Deck = deck
            GameStatus = DealerError
        }
    | DealerResponse.DealerBusted  (score, hand, deck) -> 
        {
            Players = game.Players
            Dealer = { Hand = hand; HandStatus = HandStatus.Busted score }
            Deck = deck
            GameStatus = DealerBusted
        }
    | DealerResponse.DealerStayed (score, hand, deck) -> 
        {
            Players = game.Players
            Dealer = { Hand = hand; HandStatus = HandStatus.Stayed score }
            Deck = deck
            GameStatus = DealerFinished
        }

(*

Determine Winner

Precondition(s): exclude all busted players

- any player stayed, dealer stayed
    -> player or dealer with highest score wins
    -> tied for highest score -> no winner
- any player stayed, dealer busted
    -> playe with highest score wins
    -> tied for highest score -> no winner
       
- single player blackjack, dealer stayed
    -> player wins
- multiple players blackjack, dealer not blackjack
    -> no winner
- any player blackjack, dealer blackjack
    -> no winner
- no player blackjack, dealer blackjack
    -> dealer wins

*)
type Winner =
    | Player of Player
    | Dealer of Dealer
    | Nobody

let determineWinner game =
    printfn "Result is:%s" Environment.NewLine
    
    game.Players |> List.iter (fun x -> printfn "PlayerId: %A final hand: %A " x.Id x.Hand)
    printfn "final dealer hand: %A" game.Dealer.Hand

    // TODO:
    Nobody

let askForNumberOfPlayers =
    printfn "How many players (number between 1 and 3)?"
    let input = Console.ReadLine().Trim()

    let tryToNumberOfPlayers s =
        let numberCheck = Regex("^(1|2|3)$")    
        let strContainsOnlyValidNumbers (s:string) = numberCheck.IsMatch s
        let isValidNumberOfPlayers = strContainsOnlyValidNumbers input
        match isValidNumberOfPlayers with
        | true -> Some (NumberOfPlayers (System.Int32.Parse input))
        | false -> None

    tryToNumberOfPlayers input

[<EntryPoint>]
let main argv =
    printStartMessage

    let initialDeck = createDeck
    let maybeNumberOfPlayers = askForNumberOfPlayers
    match maybeNumberOfPlayers with
    | None -> weWillDealWithErrorHandingLater "invalid number of players"
    
    | Some numberOfPlayers ->
        let (players, deckAfterAllPlayersHaveBeenInitialized) = initializePlayers numberOfPlayers initialDeck
        let maybeInitializedDealer = trySetupDealer drawCard deckAfterAllPlayersHaveBeenInitialized
        match maybeInitializedDealer with
        | None -> weWillDealWithErrorHandingLater "problem initializing dealer"
        | Some (dealer, deckAfterDealerInitialization) ->

            let initialGameState = {
                Players = players
                Dealer = dealer
                Deck = deckAfterDealerInitialization
                GameStatus = Started
            }

            // TODO: implement multiplayer mode
            let gameAfterPlayerFinished = playerLoop initialGameState (PlayerId 1)
            let gameAfterDealerFinished = dealerTurn gameAfterPlayerFinished

            printfn "final player hand: %A" gameAfterDealerFinished.Players.[0].Hand
            printfn "final dealer hand: %A" gameAfterDealerFinished.Dealer.Hand
            printfn "Winner is %A" (determineWinner gameAfterDealerFinished)
            ()

    0 // return an integer exit code
