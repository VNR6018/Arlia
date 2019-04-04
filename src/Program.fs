open Parser
open AST
open Desugger
open FileHandler

System.Console.Clear()

let buildAst file = Desugger.desugger (parse (System.IO.File.ReadAllText file))

let progToList prog = match prog with Program p -> p

// Example of use:

// match buildAst "hello.arl" with
//      DProgram instrs ->
//          for instr in instrs do
//              try
//                  printfn "%A" instr
//              with Failure msg -> printfn "Error: %s" msg
