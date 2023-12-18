namespace algoTest

open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open Evaluator
open Combinator

[<TestClass>]
type TestClass () =

    [<TestMethod>] //Parser test
    member this.TestpPitch () =
        let input = "120 bpm"
        let expected = 120
        let preparedInput = prepare input
        let actual = pTempo preparedInput
        match actual with
        | Success (result, _) -> 
            Assert.AreEqual(expected, result)
        | Failure _ ->
            Assert.IsTrue(false)
     
    [<TestMethod>] //Interpreter test
    member this.TestPitchToMidiNote () =
        let input = (('C', 'n'), 4) //middle C
        let expected = 72
        let actualMidiNote = pitchToMidiNote input
        Assert.AreEqual(expected, actualMidiNote)