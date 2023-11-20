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

let pNoteType : Parser<NoteType> =
    //Parses the note, octave and note length into a double tuple. Ex (('C', 4), 2) 
    pseq (pseq (pad pletter) pdigit (fun (c, x) -> (c,x))) (pad pdigit) (fun (t, y) -> (t,y))

//let pMeter : Parser<Meter> =

let parseTempo (input: string) =
    let preparedInput = prepare input
    match pTempo preparedInput with
    | Success (tempo, _) -> Some tempo
    | Failure _ -> None

let parseMeter (input: string) =
    let preparedInput = prepare input
    match pMeter preparedInput with
    | Success (meter, _) -> Some meter
    | Failure _ -> None