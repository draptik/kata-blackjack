module BlackJack.Game

(*
    RULES from
        - https://en.wikipedia.org/wiki/Blackjack
        - https://de.wikipedia.org/wiki/Black_Jack
        
    Glossary:
        "hit" == "draw a card"
        "stand": player "freezes" his hand

Idea for game states:

Game states

- start game
    - only when: initial
    - register players
    - create deck
    -> returned state: GameStarted
    
- deal cards
    - only when: GameStarted
    - each player is dealt 2 cards
    - dealer is dealt 2 cards
    -> returned state: CardsDealt

- "Players play"
    - only when: CardsDealt

    - player turn
        - only when: ??
        - each player can hit multiple times ("recursive")
            -> returned state is either
                - hit again -> "player turn" (recursion)
                - Stand: player wants this to be his final score
                - Busted: hand score is larger than 21

    -> returned state: AllPlayersFinished (all players are either Busted and/or Stand)

- dealer turn
    - only when: all players have completed their hands (PlayersFinished)
    - must hit until hand score <= 17
    - dealer can't hit when hand score is >= 17
    - dealer must hit on "soft" 17 (ace and one or more other cards totaling six)
    -> returned state is
        - DealerStand
        - DealerBusted
        
- determine winner
    - only when: DealerStand or DealerBusted 
    - DealerBusted -> all remaining players win (determine 'remaining' players?)
    - all remaining players closer to 21 win than the dealer
    -
     tie between dealer and players -> Tie (aka stand off, push, tie, égalité, en cartes)
           
*)

