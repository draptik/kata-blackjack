module DummyTests


open Xunit
open FsUnit.Xunit
open BlackJack.Domain

(*
    'Hand' is a record type:
        {
            Cards: HandCards
            Status: HandStatus
        }

    'HandCards' is a single-case DU. Which in turn is a 'Card list'.
    
    Why the hassle with 'HandCards'?
    Card games tend to have a deck (a list of cards) and players,
    which in turn also have a hand (a list of cards).
*)    
[<Fact>]
let ``destructuring cards from a hand. Demo comparing equal patterns`` () =

    let getCards1 (hand: Hand) = match hand.Cards with | HandCards cs -> cs
    
    let getCards2 (hand: Hand) =
        let {Cards = handCards; Status = _} = hand
        let (HandCards cards) = handCards
        cards
        
    let getCards3 (hand: Hand) =
        let {Cards = (HandCards cards); Status = _} = hand
        cards

    let getCards4 {Cards = (HandCards cards); Status = _} = cards
    
    // When dropping `Status = _` from the previous function we have to make the type explicit (`: Hand`),
    // because there are multiple records containing `Cards`.
    let getCards5 ({Cards = (HandCards cards)} : Hand) = cards
    
    let cards = [ {Suit = Spades; Rank = Eight }; {Suit = Hearts; Rank = Queen} ]
    let hand = { Cards = HandCards cards; Status = CardsDealt }

    hand |> getCards1 |> should equal cards
    hand |> getCards2 |> should equal cards
    hand |> getCards3 |> should equal cards
    hand |> getCards4 |> should equal cards
    hand |> getCards5 |> should equal cards