module Evaluator
open System.IO
open Commons.Music.Midi

open AST



let midiMusic = new MidiMusic()
let track = new MidiTrack()
midiMusic.Tracks.Add(track) //hold midi events in the track

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
    let octaveNum = (octave) * 12
    noteNum + accidentalNum + octaveNum


let calculateDuration (length:float) = 
    let quarterNote = 480 //midi ticks
    
    let halfNote = quarterNote * 2
    let wholeNote = quarterNote * 4

    let eighthNote = quarterNote / 2
    let sixteenthNote = quarterNote / 4
    let thirtySecondNote = quarterNote / 8
    let sixtyFourthNote = quarterNote / 16

    match length with
    | 1.0 -> quarterNote
    | 2.0 -> halfNote
    | 4.0 -> wholeNote
    | 0.5 -> eighthNote
    | 0.25 -> sixteenthNote
    | 0.125 -> thirtySecondNote
    | 0.0625 -> sixtyFourthNote
    | _ -> failwith "Invalid length"


let addMidiEvent deltaTime eventType note velocity =
    let midiEvent = new MidiEvent(eventType, note, velocity, null) //create midi event
    let midiMessage = new MidiMessage(deltaTime, midiEvent)
    track.Messages.Add(midiMessage)

let addNoteToTrack (note:Note) (deltaTime) = 
    let (pitch, length) = note
    let midiNote = pitchToMidiNote pitch
    let duration = calculateDuration length
    addMidiEvent deltaTime MidiEvent.NoteOn (byte midiNote) 0x40uy //set velocity for 64 (0x40) and uy for unsigned byte so range is good for Midi
    addMidiEvent (duration + deltaTime) MidiEvent.NoteOff (byte midiNote) 0x40uy
    duration + deltaTime

let addMelodyToTrack (melody: Melody) =
    let mutable totalDeltaTime = 0
    for note in melody do
        totalDeltaTime <- addNoteToTrack note totalDeltaTime
let addChordToTrack (chord: Chord) =
    let (pitches, length) = chord
    let duration = calculateDuration length
    for pitch in pitches do
        let midiNote = pitchToMidiNote pitch
        addMidiEvent 0 MidiEvent.NoteOn (byte midiNote) 0x40uy //set velocity for 64 (0x40) and uy for unsigned byte so range is good for Midi
    for pitch in pitches do
        let midiNote = pitchToMidiNote pitch
        addMidiEvent duration MidiEvent.NoteOff (byte midiNote) 0x40uy

let addChordsToTrack (chords: Chord list) =
    chords
    |> List.iter addChordToTrack

let setTempo (tempo: Tempo) =
    0

let addEndOfTrackEvent () =
    let endOfTrackData = Array.empty<byte>
    let endOfTrackEvent = new MidiEvent(MidiMetaType.EndOfTrack, 0uy, 0uy, endOfTrackData)
    let endOfTrackMessage = new MidiMessage(0, endOfTrackEvent)
    track.Messages.Add(endOfTrackMessage)

let evaluateSong (song: Song) =
    let ((tempo, melody), (percuss, chord)) = song
    addMelodyToTrack melody
    addChordsToTrack chord
    addEndOfTrackEvent()

let writeMidiToFile (filePath: string) =
    use fileStream = new FileStream(filePath, FileMode.Create)
    let smfWriter = new SmfWriter(fileStream)
    smfWriter.WriteMusic(midiMusic)