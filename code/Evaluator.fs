module Evaluator

open AST

type EvalResult = string
let evalNoteType (noteType: NoteType): EvalResult =
    let ((note, octave), length) = noteType
    sprintf "Note: %c, Octave: %c, Length: %c" note octave length

let evalMelody (melody: Melody): EvalResult =
    melody
    |> List.map evalNoteType
    |> String.concat "\n"

let evaluateMusic (melody: Melody): EvalResult =
    evalMelody melody