module Evaluator
open System.IO
open Commons.Music.Midi

open AST



let midiMusic = new MidiMusic()
//midiMusic.set_Format(byte 0)
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
    let octaveNum = (octave + 2) * 12
    noteNum + accidentalNum + octaveNum


let calculateDuration (length:float) = 
    let quarterNote = 480 //midi ticks
    
    let halfNote = quarterNote * 2
    let wholeNote = quarterNote * 4
    let doubleWholeNote = quarterNote * 8

    let eighthNote = quarterNote / 2
    let sixteenthNote = quarterNote / 4
    let thirtySecondNote = quarterNote / 8
    let sixtyFourthNote = quarterNote / 16

    match length with
    | 1.0 -> quarterNote
    | 2.0 -> halfNote
    | 4.0 -> wholeNote
    | 8.0 -> doubleWholeNote
    | 0.5 -> eighthNote
    | 0.25 -> sixteenthNote
    | 0.125 -> thirtySecondNote
    | 0.0625 -> sixtyFourthNote
    | _ -> failwith "Invalid length"


let addMidiEvent deltaTime eventType note velocity =
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
    addMidiEvent 0 MidiEvent.NoteOn (byte midiNote) 0x40uy
    addMidiEvent duration MidiEvent.NoteOff (byte midiNote) 0x40uy
    duration // + deltaTime

let addMelodyToTrack (melody: Melody) =
    let mutable totalDeltaTime = 0
    for note in melody do
        totalDeltaTime <- addNoteToTrack note //totalDeltaTime
    midiMusic.set_DeltaTimeSpec(int16 totalDeltaTime)

let addChordToTrack (chord: Chord) =
    let (pitches, length) = chord
    let duration = calculateDuration length
    for pitch in pitches do
        let midiNote = pitchToMidiNote pitch
        addMidiEvent 0 MidiEvent.NoteOn (byte midiNote) 0x40uy //set velocity for 64 (0x40) and uy for unsigned byte so range is good for Midi
        printfn "Added Chord Note On: Note = %i, Velocity = 64" midiNote
    for pitch in pitches do
        if pitch = pitches[0] then
            let midiNote = pitchToMidiNote pitch
            addMidiEvent duration MidiEvent.NoteOff (byte midiNote) 0x40uy
            printfn "Added Chord Note Off: Note = %i, Velocity = 64" midiNote
        else
            let midiNote = pitchToMidiNote pitch
            addMidiEvent 0 MidiEvent.NoteOff (byte midiNote) 0x40uy
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
    track.Messages.Add(midiMessage)

let addEndOfTrackEvent () =
    let endOfTrackData = Array.empty<byte>
    let endOfTrackEvent = new MidiEvent(MidiMetaType.EndOfTrack, 0uy, 0uy, endOfTrackData)
    let endOfTrackMessage = new MidiMessage(0, endOfTrackEvent)
    track.Messages.Add(endOfTrackMessage)
    printfn "Added End of Track Event"

let evaluateSong (song: Song) =
    let ((tempo, melody), (percuss, chord)) = song
    setTempo tempo
    addMelodyToTrack melody
    addChordsToTrack chord
    addEndOfTrackEvent()

let writeMidiToFile (filePath: string) =
    use fileStream = new FileStream(filePath, FileMode.Create)
    let smfWriter = new SmfWriter(fileStream)
    smfWriter.WriteMusic(midiMusic)
    printfn "MIDI file written to: %s" filePath