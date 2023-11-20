module Parser

open Combinator
open AST

let pad p = pbetween pws0 p pws0

let pNum : Parser<int> =
    pmany1 pdigit
    |>> (fun digits -> System.Int32.Parse(stringify digits))
    |> (fun p -> p <|> pzero)

let pBpm : Parser<string> =
    pad (pstr "bpm")

let pTempo : Parser<Tempo> =
    pseq pNum pBpm (fun (num, _) -> num)

let pAccidental: Parser<Accidental> =
    (pchar '#' |>> (fun _ -> Sharp))
    <|> (pchar 'b' |>> (fun _ -> Flat))
    <|> (presult Natural)
let pPitch: Parser<(char*char)> =
     //Parses the note, octave into a tuple. Ex ('C', 4)
     //need to include accidentals later and change parser type to Pitch
    (pseq (pad pletter) pdigit (fun (note, octave) -> (note,octave)))

let pNoteType: Parser<NoteType> =
    //Parses the note, octave and note length into a double tuple. Ex (('C', 4), 2) 
    pseq pPitch (pad pdigit) (fun (pitch, length) -> (pitch,length))

//let pMeter : Parser<Meter> =

let pMelody : Parser<Melody> =
    pmany1 (pad pNoteType)

let parseMelody (input: string) =
    let preparedInput = prepare input
    match pMelody preparedInput with
    | Success (melody, _) -> Some melody
    | Failure _ -> None

let parseTempo (input: string) =
    let preparedInput = prepare input
    match pTempo preparedInput with
    | Success (tempo, _) -> Some tempo
    | Failure _ -> None

(**
let parseMeter (input: string) =
    let preparedInput = prepare input
    match pMeter preparedInput with
    | Success (meter, _) -> Some meter
    | Failure _ -> None
*)

