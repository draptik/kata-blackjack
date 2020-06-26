module BlackJack.Game
open BlackJack
open Domain

(*
    RULES from
        - https://en.wikipedia.org/wiki/Blackjack
        - https://de.wikipedia.org/wiki/Black_Jack
        
    Glossary:
        "hit" == "draw a card"
        "stand": player "freezes" his hand
*)

(*

Game states

- "Initial"
    - only when: initial
    - register players
    - create deck
    -> returned state: "GameStarted"

- "GameStarted"
    - only when: "GameStarted"
    - each player is dealt 2 cards
    - dealer is dealt 2 cards
    -> returned state: "CardsDealt"

- TODO "PlayersTurn" (multiplayer version)
    - only when: CardsDealt
    - player turn
        - only when: ??
        - each player can hit multiple times ("recursive")
            -> returned PlayerState is either
                - hit again -> "player turn ??" (recursion)
                - Stand: player wants this to be his final score
                - Busted: hand score is larger than 21

    -> returned state: "AllPlayersFinished" (all players are either Busted and/or Stand)

- "PlayerTurn" (single player mode)
    - only when: "CardsDealt"
    - player can hit multiple times ("recursive")
        -> returned PlayerState is either
            - hit again -> "player turn ??" (recursion)
            - Stand: player wants this to be his final score
            - Busted: hand score is larger than 21
    -> returned state: "PlayerFinished" (either Busted or Stand)

- "DealerTurn"
    - only when: "PlayerFinished" (single player mode) / PlayersFinished (multiple player mode)
    - must hit until hand score <= 17
    - dealer can't hit when hand score is >= 17
    - dealer must hit on "soft" 17 (ace and one or more other cards totaling six)
    -> returned state is "DealerFinished" with internal states
        - DealerStand
        - DealerBusted
        
- "GameOver"
    - only when: "DealerFinished" 
    - DealerBusted -> all remaining players win (determine 'remaining' players?)
    - all remaining players closer to 21 win than the dealer
    -
     tie between dealer and players -> Tie (aka stand off, push, tie, égalité, en cartes)
*)

(* States ----------------------------------------------- *)
type Undefined = exn

type Initial = Undefined
type GameStarted = Undefined

type SomebodyHasBlackJack = PlayerHasBlackJack | DealerHasBlackJack
type InitialCardsDealt = Undefined
type CardsDealt = 
    | InitialCardsDealt of InitialCardsDealt
    | SomebodyHasBlackJack of SomebodyHasBlackJack

type PlayerFinished = Stand of Score | Busted of Score
type CurrentHand = Undefined

type PlayerTurn =
    | InitialCardsDealt
    | CurrentHand
    | PlayerFinished of PlayerFinished

type DealerTurn = Undefined
type DealerFinished = Undefined
type GameOver = Undefined

type Game =
    | Initial
    | GameStarded
    | CardsDealt
    | PlayerTurn
    | PlayerFinished
    | DealerTurn
    | DealerFinished
    | GameOver

(* State Transitions -------------------------------------*)
type RegisterPlayers = Initial -> GameStarted
type DealInitialCards = GameStarted -> CardsDealt

// transitions after CardsDealt
type PlayerStartPlaying = InitialCardsDealt -> PlayerTurn
type DetermineWinnerAfterCardsDealt = SomebodyHasBlackJack -> GameOver

// transitions after PlayerTurn
type PlayerPlaying = CurrentHand -> PlayerTurn
type PlayerFinishes = PlayerTurn -> PlayerFinished

type DealerStartPlaying = PlayerFinished -> DealerTurn
type DealerPlaying = DealerTurn -> DealerTurn
type DealerFinishes = DealerTurn -> DealerFinished

type EndGame = DealerFinished -> GameOver
