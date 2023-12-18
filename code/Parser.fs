module Parser

open Combinator
open AST


//Pareses an input between spaces
let pad p = pbetween pwsNoNL0 p pwsNoNL0

let pNum : Parser<int> =
   pmany1 pdigit
   |>> (fun digits -> System.Int32.Parse(stringify digits))
   |> (fun p -> p <|> pzero)

let pBpm : Parser<string> =
   pad (pstr "bpm")

let pTempo : Parser<Tempo> =
   pseq pNum pBpm (fun (num, _) -> num)

let pPitch: Parser<Pitch> =
    //Parses the note, octave into a tuple. Ex (('C', s), 4)
   pseq (pseq (pad pletter) pletter (fun (letter, char) -> (letter, char))) pNum (fun (note, octave) -> (note, octave))

let pNote: Parser<Note> =
   //Parses the note, octave and note length into a tripe tuple. Ex ((('C', s), 4), 2)
   pseq pPitch (pad pNum) (fun (pitch, length) -> (pitch, length))

let pMelody : Parser<Melody> =
   pmany1 (pleft (pad pNote) (pstr (",")))

let pPercuss : Parser<Percuss> =
   pseq pNum (pmany1 (pad pNum)) (fun (sound, pList) -> (sound, pList))

let pBeat : Parser<Percuss list> =
   pmany0 (pleft (pad pPercuss) (pstr (",")))

let pChord: Parser<Chord> =
   pseq (pmany1 pPitch) (pad pNum) (fun (pitches, duration) -> (pitches, duration))

let pChordList: Parser<Chord list> =
   pmany0 (pleft (pad pChord) (pstr (",")))

let pSong: Parser<Song> =
   pseq
       (pseq (pleft pTempo pnl) pMelody (fun (tempo, melody) -> (tempo, melody)))
       (pseq (pright pnl pBeat) (pright pnl pChordList) (fun (beat, chords) -> (beat, chords)))
       (fun (tempoAndMelody, beatAndChords) -> (tempoAndMelody, beatAndChords))


let parseSong (input: string) =
   let preparedInput = prepare input
   match pSong preparedInput with
   | Success (song, _) -> Some song
   | Failure _ -> None
