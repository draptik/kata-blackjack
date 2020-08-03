module BlackJack.Ui.Common

open System

(*
    ConsolePrompt is from `Stylish F#` book.
    Can be extended to include retry attempts. ;-)
*)
type ConsolePrompt<'T>(message: string, tryConvert: string -> 'T option) =
    member this.GetValue() =
        printfn "%s:" message
        let input = Console.ReadLine()
        match input |> tryConvert with
        | Some v -> v
        | None -> this.GetValue() // invalid input, try again ("recursion") 

