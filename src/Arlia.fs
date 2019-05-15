open Parser
open Typechecker
open CLI
open Printter

[<EntryPoint>]
let main argv =
    
    System.Console.Clear()

    //let file = System.IO.File.ReadAllText argv.[0]
    //let program = Desugger.desugger (parse file None)

    //printfn "%A" program

    //exit 0

    if argv.Length = 0
    then runCLI (newEnv None)
    else
        let file = System.IO.File.ReadAllText argv.[0]
        let fileinfo = System.IO.FileInfo argv.[0]
        let time = System.Diagnostics.Stopwatch.StartNew ()
        let program = Desugger.desugger (parse file (Some fileinfo.Name))
        time.Stop()
        let lines = (System.IO.File.ReadAllLines argv.[0]).Length
        let size = fileinfo.Length
        let time = time.Elapsed.TotalMilliseconds / float 1000
       // printfn " -- %s --\n\nFile size: %d bytes\nLines: %d\nTime taken: %f milliseconds\n" (fileinfo.Name) size lines time
        let typecheck =
            try typecheck_document (Some fileinfo) program
            with Failure msg ->
                Errors.typecheckingError msg
                Typechecker.newEnv (Some fileinfo)
        typecheck |> ignore
        runCLI typecheck
    0
