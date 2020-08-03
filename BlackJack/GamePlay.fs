module BlackJack.GamePlay

open BlackJack.Domain

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

