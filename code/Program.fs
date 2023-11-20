open Parser
open Evaluator

[<EntryPoint>]
let main (argv: string array) =
   if argv.Length = 1 then
       match parse argv.[0] with
       | Some _____ ->
           
           0
       | None ->
           usage()
           1
   else
       usage()
       1
