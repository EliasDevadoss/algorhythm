open Parser
open Evaluator

let usage() = 
    printfn "Usage: dotnet run \"Musical Composition file\""
    printfn "Example: dotnet run \"music_file.txt\""

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

                let newName = filename.Substring(0, (String.length (filename) - 4))
                let newFilename2 = newName + "_Chords"
                let newFilename = newName + "_Melody"
                let outputFile = sprintf "%s.midi" newFilename
                let outputFile2 = sprintf "%s.midi" newFilename2
                writeMidiToFile outputFile 1
                writeMidiToFile outputFile2 2
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
