module Parser

open Combinator
open AST

let stringOfCharList (chars: char list) : string =
    chars |> List.toArray |> System.String.Concat

let pNum : Parser<int> =
    pmany1 pdigit
    |>> (fun digits -> System.Int32.Parse(stringOfCharList digits))
    |> (fun p -> p <|> pzero)

let pBpm : Parser<string> =
    pstr "bpm"

let pTempo : Parser<Tempo> =
    pseq pNum pBpm (fun (num, _) -> num)

//let pNoteType : Parser<NoteType> =

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