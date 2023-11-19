module AST

type Num = int

type Tempo = Num

type NoteType = Num

type Meter = Num * NoteType

type Accidental = Sharp | Flat | Natural

type Pitch = char * Accidental * Num

type Duration = Num

type Note = Pitch * Duration

type Sound = Kick | Snare | HiHat | Crash | Ride | China | Splash

type Percuss = Sound * Num list

type Chord = Pitch list * Duration

type Melody = Note list

type Beat = Percuss list

type Song = Tempo * Meter * Melody * Beat * Chord list