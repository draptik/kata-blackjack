module BlackJack.Ui.Interaction

open System
open System.Text.RegularExpressions

open BlackJack.Domain
open BlackJack.Ui.Common

// TODO can be made "private" but still testable?
let tryConvertToPlayerChoiceFromPrompt (s: string) =
    if String.IsNullOrWhiteSpace(s) then
        None
    else
        match s.Trim() with
        | "1" -> Some CurrentPlayerAction.Hit
        | "2" -> Some CurrentPlayerAction.Stand
        | _ ->
            printfn "Unknown selection"
            None
        
let promptPlayerToChooseBetweenHitOrStand (playerId: PlayerId) =
    let message = sprintf "%A What do you want to do? (1) Hit or (2) Stand?" playerId
    ConsolePrompt(message, tryConvertToPlayerChoiceFromPrompt)


// TODO can be made "private" but still testable?
let tryToNumberOfPlayers s =
    let numberCheck = Regex("^(1|2|3|4|5|6|7)$")
    if numberCheck.IsMatch s then
        Some (NumberOfPlayers (System.Int32.Parse s))
    else
        None

let promptForNumberOfPlayers =
    ConsolePrompt("How many players (number between 1 and 7)?", tryToNumberOfPlayers)
    