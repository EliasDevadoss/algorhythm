module AST

type Num = int

type Tempo = Num

type NoteType = (char * char) * char

type Meter = Num * NoteType

type Accidental = Sharp | Flat | Natural

type Pitch = char * Accidental * Num

type Duration = Num

type Sound = Kick | Snare | HiHat | Crash | Ride | China | Splash

type Percuss = Sound * Num list

type Chord = Pitch list * Duration

type Melody = NoteType list

type Beat = Percuss list

type Song = Tempo * Meter * Melody * Beat * Chord list