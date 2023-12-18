module Evaluator
open System.IO
open Commons.Music.Midi
open AST

let midiMusic = new MidiMusic()
let midiMusic2 = new MidiMusic()
midiMusic.set_Format(byte 1)
midiMusic2.set_Format(byte 1)
let track1 = new MidiTrack()
let track2 = new MidiTrack()
midiMusic.Tracks.Add(track1) //hold midi events in the track
midiMusic2.Tracks.Add(track2)

let pitchToMidiNote (pitch: Pitch) = 
    let ((note, accidental), octave) = pitch
    let noteNum = match note with
                  | 'C' -> 0
                  | 'D' -> 2
                  | 'E' -> 4
                  | 'F' -> 5
                  | 'G' -> 7
                  | 'A' -> 9
                  | 'B' -> 11
                  | _ -> failwith "Invalid note"
    let accidentalNum = match accidental with
                        | 'n' -> 0
                        | 's' -> 1
                        | 'b' -> -1
                        | _ -> failwith "Invalid accidental"
    let octaveNum = (octave + 2) * 12
    noteNum + accidentalNum + octaveNum


let calculateDuration (length:float) tempo = 
    let quarterNote = (float) 57600 / tempo
    match length with
    | x -> int (quarterNote * x)
    //| _ -> failwith "Invalid length"


let addMidiEvent deltaTime eventType note velocity (track: MidiTrack) =
    let midiEvent = new MidiEvent(eventType, note, velocity, null) //create midi event
    let midiMessage = new MidiMessage(deltaTime, midiEvent)
    track.Messages.Add(midiMessage)

let addNoteToTrack (note: Note) start tempo rest = //(deltaTime) = 
    let (pitch, length) = note
    let duration = calculateDuration length tempo
    let midiNote = pitchToMidiNote pitch
    addMidiEvent start MidiEvent.NoteOn (byte midiNote) 0x40uy track1
    addMidiEvent duration MidiEvent.NoteOff (byte midiNote) 0x40uy track1
    duration

let addMelodyToTrack (melody: Melody) tempo =
//cannot have two consecutive
    let mutable temp = 0
    for note in melody do
        let (((thisNote, accidental), octave), length) = note
        if (thisNote = 'r') then
            temp <- calculateDuration length tempo
        else
            if (temp <> 0) then
                addNoteToTrack note temp tempo true |> ignore
                temp <- 0
            else
                addNoteToTrack note 0 tempo false |> ignore

let addChordToTrack (chord: Chord) tempo =
    let (pitches, length) = chord
    let duration = calculateDuration length tempo
    for pitch in pitches do
        let midiNote = pitchToMidiNote pitch
        addMidiEvent 0 MidiEvent.NoteOn (byte midiNote) 0x40uy track2 //set velocity for 64 (0x40) and uy for unsigned byte so range is good for Midi
    for pitch in pitches do
        if pitch = pitches[0] then
            let midiNote = pitchToMidiNote pitch
            addMidiEvent duration MidiEvent.NoteOff (byte midiNote) 0x40uy track2
        else
            let midiNote = pitchToMidiNote pitch
            addMidiEvent 0 MidiEvent.NoteOff (byte midiNote) 0x40uy track2

let addChordsToTrack (chords: Chord list) tempo =
    for chord in chords do
        addChordToTrack chord tempo

let addEndOfTrackEvent () =
    let endOfTrackData = Array.empty<byte>
    let endOfTrackEvent = new MidiEvent(MidiMetaType.EndOfTrack, 0uy, 0uy, endOfTrackData)
    let endOfTrackMessage = new MidiMessage(0, endOfTrackEvent)
    track1.Messages.Add(endOfTrackMessage)
    track2.Messages.Add(endOfTrackMessage)

let evaluateSong (song: Song) =
    let ((tempo, melody), (percuss, chord)) = song
    midiMusic.set_DeltaTimeSpec(int16 (calculateDuration 1 tempo))
    midiMusic2.set_DeltaTimeSpec(int16 (calculateDuration 1 tempo))
    addMelodyToTrack melody tempo
    addChordsToTrack chord tempo
    addEndOfTrackEvent()

let writeMidiToFile (filePath: string) num =
    use fileStream = new FileStream(filePath, FileMode.Create)
    let smfWriter = new SmfWriter(fileStream)
    if (num = 1) then
        smfWriter.WriteMusic(midiMusic)
    if (num = 2) then
        smfWriter.WriteMusic(midiMusic2)
    printfn "MIDI file written to: %s" filePath
    