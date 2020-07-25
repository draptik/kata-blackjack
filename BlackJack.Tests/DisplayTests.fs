module DisplayTests

open Xunit
open FsUnit.Xunit
open BlackJack.Domain
open BlackJack.Ui.Display

let isFalse = Assert.True(false)

[<Fact>]
let ``showCardAscii`` () =
    fullDeck
    |> List.map showCardAscii
    |> should equal [
        " 2D"
        " 3D"
        " 4D"
        " 5D"
        " 6D"
        " 7D"
        " 8D"
        " 9D"
        "10D"
        "JaD"
        "QuD"
        "KiD"
        "AcD"
        " 2H"
        " 3H"
        " 4H"
        " 5H"
        " 6H"
        " 7H"
        " 8H"
        " 9H"
        "10H"
        "JaH"
        "QuH"
        "KiH"
        "AcH"
        " 2C"
        " 3C"
        " 4C"
        " 5C"
        " 6C"
        " 7C"
        " 8C"
        " 9C"
        "10C"
        "JaC"
        "QuC"
        "KiC"
        "AcC"
        " 2S"
        " 3S"
        " 4S"
        " 5S"
        " 6S"
        " 7S"
        " 8S"
        " 9S"
        "10S"
        "JaS"
        "QuS"
        "KiS"
        "AcS"
    ]
