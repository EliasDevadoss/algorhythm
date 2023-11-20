open Parser
open Evaluator

//note for later: \x to represent numbers in hex, File.writeBinary or something similar

let usage() = 
    printfn "Usage: dotnet run <Musical Notation> where Musical Notation is Note (A-G), octave (0-9), and length of note(1, 2, 4, 8, 16, 32)"
    printfn "Example: dotnet run \"C4 2 D4 2 E4 4\""

[<EntryPoint>]
let main (argv: string array) =
   if argv.Length = 1 then
       match parseMelody argv.[0] with
        | Some melody ->
            let evalResult = evaluateMusic melody
            printfn "Melody: %s" evalResult
            0
        | None ->
            printfn "Invalid input"
            usage()
            1
   else
       usage()
       1
