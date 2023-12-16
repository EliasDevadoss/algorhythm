module Evaluator
open System.IO
open Commons.Music.Midi

open AST



let midiMusic = new MidiMusic()
midiMusic.set_Format(byte 1)
let track1 = new MidiTrack()
let track2 = new MidiTrack()
midiMusic.Tracks.Add(track1) //hold midi events in the track
midiMusic.Tracks.Add(track2)

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


let calculateDuration (length:float) = 
    let quarterNote = 480 //midi ticks
    let whole = quarterNote * 4
    
    let half = whole / 2
    let doubleWhole = whole * 2
    let quadWhole = whole * 4

    let eighth = whole / 8
    let sixteenth = whole / 16
    let thirtySecond = whole / 32
    let sixtyFourth = whole / 64

    let dottedSixteenth = sixteenth * 1.5
    let dottedEighth = eighth * 1.5
    let dottedQuarter = quarterNote * 1.5
    let dottedHalf = half * 1.5

    match length with
    | 1.0 -> quarterNote
    | 1.5 -> dottedQuarter
    | 2.0 -> half
    | 3.0 -> dottedHalf
    | 4.0 -> whole
    | 8.0 -> doubleWhole
    | 16.0 -> quadWhole
    | 0.75 -> dottedEighth
    | 0.5 -> eighth
    | 0.375 -> dottedSixteenth
    | 0.25 -> sixteenth
    | 0.125 -> thirtySecond
    | 0.0625 -> sixtyFourth
    | _ -> failwith "Invalid length"


let addMidiEvent deltaTime eventType note velocity (track: MidiTrack) =
    let midiEvent = new MidiEvent(eventType, note, velocity, null) //create midi event
    let midiMessage = new MidiMessage(deltaTime, midiEvent)
    track.Messages.Add(midiMessage)
    printfn "Added MIDI Event: DeltaTime=%i, EventType=%i, Note=%i, Velocity=%i" deltaTime eventType note velocity

let addNoteToTrack (note:Note) = //(deltaTime) = 
    let (pitch, length) = note
    let midiNote = pitchToMidiNote pitch
    let duration = calculateDuration length
    //addMidiEvent deltaTime MidiEvent.NoteOn (byte midiNote) 0x40uy //set velocity for 64 (0x40) and uy for unsigned byte so range is good for Midi
    //addMidiEvent (duration + deltaTime) MidiEvent.NoteOff (byte midiNote) 0x40uy
    addMidiEvent 0 MidiEvent.NoteOn (byte midiNote) 0x40uy track1
    addMidiEvent duration MidiEvent.NoteOff (byte midiNote) 0x40uy track1
    duration // + deltaTime

let addMelodyToTrack (melody: Melody) =
    //let mutable totalDeltaTime = 0
    for note in melody do
        addNoteToTrack note |> ignore
        //totalDeltaTime <- addNoteToTrack note //totalDeltaTime

let addChordToTrack (chord: Chord) =
    let (pitches, length) = chord
    let duration = calculateDuration length
    for pitch in pitches do
        let midiNote = pitchToMidiNote pitch
        addMidiEvent 0 MidiEvent.NoteOn (byte midiNote) 0x40uy track2//set velocity for 64 (0x40) and uy for unsigned byte so range is good for Midi
        printfn "Added Chord Note On: Note = %i, Velocity = 64" midiNote
    for pitch in pitches do
        if pitch = pitches[0] then
            let midiNote = pitchToMidiNote pitch
            addMidiEvent duration MidiEvent.NoteOff (byte midiNote) 0x40uy track2
            printfn "Added Chord Note Off: Note = %i, Velocity = 64" midiNote
        else
            let midiNote = pitchToMidiNote pitch
            addMidiEvent 0 MidiEvent.NoteOff (byte midiNote) 0x40uy track2
            printfn "Added Chord Note Off: Note = %i, Velocity = 64" midiNote

let addChordsToTrack (chords: Chord list) =
    chords
    |> List.iter addChordToTrack

let setTempo (tempo: Tempo) =
    let microsecondsPerMinute = 60000000
    let microsecondsPerQuarterNote = microsecondsPerMinute / tempo

    ////need to split the 24-bit integer into 3 bytes with most significant bytes first for the midi and store them in the tempoData array
    let tempoData = Array.create 3 0uy

    //right shift the 32-bit integer by 16 bits and then bitwise AND with 0xFF to get the first 8 bits
    tempoData.[0] <- byte ((microsecondsPerQuarterNote >>> 16) &&& 0xFF)

    //right shift the 32-bit integer by 8 bits and then bitwise AND with 0xFF to get the second 8 bits
    tempoData.[1] <- byte ((microsecondsPerQuarterNote >>> 8) &&& 0xFF)

    //bitwise AND with 0xFF to get the last 8 bits
    tempoData.[2] <- byte (microsecondsPerQuarterNote &&& 0xFF)

    printfn "Setting Tempo: %i BPM" tempo
    //create the midi event and message and add it to the track
    let midiEvent = new MidiEvent(MidiMetaType.Tempo, 0uy, 0uy, tempoData)
    let midiMessage = new MidiMessage(0, midiEvent)
    track1.Messages.Add(midiMessage)
    track2.Messages.Add(midiMessage)

let addEndOfTrackEvent () =
    let endOfTrackData = Array.empty<byte>
    let endOfTrackEvent = new MidiEvent(MidiMetaType.EndOfTrack, 0uy, 0uy, endOfTrackData)
    let endOfTrackMessage = new MidiMessage(0, endOfTrackEvent)
    track1.Messages.Add(endOfTrackMessage)
    track2.Messages.Add(endOfTrackMessage)
    printfn "Added End of Track Event"

let evaluateSong (song: Song) =
    let ((tempo, melody), (percuss, chord)) = son g
    midiMusic.set_DeltaTimeSpec(int16 (calculateDuration 1))
    setTempo tempo
    addMelodyToTrack melody
    addChordsToTrack chord
    addEndOfTrackEvent()

let writeMidiToFile (filePath: string) =
    use fileStream = new FileStream(filePath, FileMode.Create)
    let smfWriter = new SmfWriter(fileStream)
    smfWriter.WriteMusic(midiMusic)
    printfn "MIDI file written to: %s" filePath