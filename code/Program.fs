open Parser
open Evaluator

let usage() = 
    printfn "Usage: dotnet run <Musical Composition file>"
    printfn "Example: dotnet run <music_file.txt>"

[<EntryPoint>]
let main (argv: string array) =
   if argv.Length = 1 then
        try
            let filename = argv.[0]
            let userInput = System.IO.File.ReadAllText(filename)
            match parseSong userInput with
            | Some song ->

                printfn "Evaluating your song..."
                printfn "%A" song

                printfn "Generating MIDI file..."
                evaluateSong song

                let outputFile = sprintf "%s.midi" filename
                writeMidiToFile outputFile
                printfn "Your MIDI file is generated: %s" outputFile
                0
            | None ->
                printfn "Invalid input"
                usage()
                1
        with
            | :? System.IO.FileNotFoundException ->
                printfn "File not found"
                usage()
                1
   else
       usage()
       1
