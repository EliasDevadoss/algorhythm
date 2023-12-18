namespace algoTest

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open Evaluator
open AST

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestpNote () =
        let input = "C#4 2"
        let expected =  ((('C', '#'), 4), 2)
        let actual = pNote input
        Assert.AreEqual(expected, actual)