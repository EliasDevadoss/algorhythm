module AST

type Num = int

type Tempo = Num

type Pitch = (char * char) * Num

type Duration = Num

type Note = Pitch * Duration

type Melody = Note list

type Sound = Num

type Percuss = Sound * Num list

type Chord = Pitch list * Duration

type Song = ((Tempo * Melody) * (Percuss list * Chord list))