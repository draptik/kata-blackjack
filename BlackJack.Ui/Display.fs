module BlackJack.Ui.Display

open BlackJack.Domain

let showCard card =
    // https://en.wikipedia.org/wiki/Playing_cards_in_Unicode#Block
    match card with
    | { Suit = Spades; Rank = Ace } -> sprintf "\U0001F0A1"
    | { Suit = Spades; Rank = Two } -> sprintf "\U0001F0A2"
    | { Suit = Spades; Rank = Three } -> sprintf "\U0001F0A3"
    | { Suit = Spades; Rank = Four } -> sprintf "\U0001F0A4"
    | { Suit = Spades; Rank = Five } -> sprintf "\U0001F0A5"
    | { Suit = Spades; Rank = Six } -> sprintf "\U0001F0A6"
    | { Suit = Spades; Rank = Seven } -> sprintf "\U0001F0A7"
    | { Suit = Spades; Rank = Eight } -> sprintf "\U0001F0A8"
    | { Suit = Spades; Rank = Nine } -> sprintf "\U0001F0A9"
    | { Suit = Spades; Rank = Ten } -> sprintf "\U0001F0AA"
    | { Suit = Spades; Rank = Jack } -> sprintf "\U0001F0AB"
    | { Suit = Spades; Rank = Queen } -> sprintf "\U0001F0AD"
    | { Suit = Spades; Rank = King } -> sprintf "\U0001F0AE"

    | { Suit = Hearts; Rank = Ace } -> sprintf "\U0001F0B1"
    | { Suit = Hearts; Rank = Two } -> sprintf "\U0001F0B2"
    | { Suit = Hearts; Rank = Three } -> sprintf "\U0001F0B3"
    | { Suit = Hearts; Rank = Four } -> sprintf "\U0001F0B4"
    | { Suit = Hearts; Rank = Five } -> sprintf "\U0001F0B5"
    | { Suit = Hearts; Rank = Six } -> sprintf "\U0001F0B6"
    | { Suit = Hearts; Rank = Seven } -> sprintf "\U0001F0B7"
    | { Suit = Hearts; Rank = Eight } -> sprintf "\U0001F0B8"
    | { Suit = Hearts; Rank = Nine } -> sprintf "\U0001F0B9"
    | { Suit = Hearts; Rank = Ten } -> sprintf "\U0001F0BA"
    | { Suit = Hearts; Rank = Jack } -> sprintf "\U0001F0BB"
    | { Suit = Hearts; Rank = Queen } -> sprintf "\U0001F0BD"
    | { Suit = Hearts; Rank = King } -> sprintf "\U0001F0BE"

    | { Suit = Diamonds; Rank = Ace } -> sprintf "\U0001F0C1"
    | { Suit = Diamonds; Rank = Two } -> sprintf "\U0001F0C2"
    | { Suit = Diamonds; Rank = Three } -> sprintf "\U0001F0C3"
    | { Suit = Diamonds; Rank = Four } -> sprintf "\U0001F0C4"
    | { Suit = Diamonds; Rank = Five } -> sprintf "\U0001F0C5"
    | { Suit = Diamonds; Rank = Six } -> sprintf "\U0001F0C6"
    | { Suit = Diamonds; Rank = Seven } -> sprintf "\U0001F0C7"
    | { Suit = Diamonds; Rank = Eight } -> sprintf "\U0001F0C8"
    | { Suit = Diamonds; Rank = Nine } -> sprintf "\U0001F0C9"
    | { Suit = Diamonds; Rank = Ten } -> sprintf "\U0001F0CA"
    | { Suit = Diamonds; Rank = Jack } -> sprintf "\U0001F0CB"
    | { Suit = Diamonds; Rank = Queen } -> sprintf "\U0001F0CD"
    | { Suit = Diamonds; Rank = King } -> sprintf "\U0001F0CE"

    | { Suit = Clubs; Rank = Ace } -> sprintf "\U0001F0D1"
    | { Suit = Clubs; Rank = Two } -> sprintf "\U0001F0D2"
    | { Suit = Clubs; Rank = Three } -> sprintf "\U0001F0D3"
    | { Suit = Clubs; Rank = Four } -> sprintf "\U0001F0D4"
    | { Suit = Clubs; Rank = Five } -> sprintf "\U0001F0D5"
    | { Suit = Clubs; Rank = Six } -> sprintf "\U0001F0D6"
    | { Suit = Clubs; Rank = Seven } -> sprintf "\U0001F0D7"
    | { Suit = Clubs; Rank = Eight } -> sprintf "\U0001F0D8"
    | { Suit = Clubs; Rank = Nine } -> sprintf "\U0001F0D9"
    | { Suit = Clubs; Rank = Ten } -> sprintf "\U0001F0DA"
    | { Suit = Clubs; Rank = Jack } -> sprintf "\U0001F0DB"
    | { Suit = Clubs; Rank = Queen } -> sprintf "\U0001F0DD"
    | { Suit = Clubs; Rank = King } -> sprintf "\U0001F0DE"

let showCardAscii (card: Card) =
    let rank = 
        match card.Rank with
        | Two -> " 2"
        | Three -> " 3"
        | Four -> " 4"
        | Five -> " 5"
        | Six -> " 6"
        | Seven -> " 7"
        | Eight -> " 8"
        | Nine -> " 9"
        | Ten -> "10"
        | Jack -> "Ja"
        | Queen -> "Qu"
        | King -> "Ki"
        | Ace -> "Ac"

    let suit =
        match card.Suit with
        | Hearts -> "H"
        | Spades -> "S"
        | Clubs -> "C"
        | Diamonds -> "D"

    sprintf "%s%s" rank suit

let showHand hand handStatus =
    hand 
    |> List.map showCard 
    |> String.concat " " 
    |> sprintf "%A %A %A" handStatus (calcScore hand)
