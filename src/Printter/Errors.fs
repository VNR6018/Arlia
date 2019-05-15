module Printter.Errors

    open Printter.Utils

    let rec unknownCommand (command: string) =
        arrow ()
        setColor System.ConsoleColor.DarkYellow
        printf "Unknown command: "
        setColor System.ConsoleColor.DarkCyan
        printfn "`%s`" (command.Trim())
        maybeYouWanted (command.Trim().Substring 1) commands

    and private commands =
        [ "?"; "h"; "help";
          "cls"; "clear"; "run";
          "load"; "t"; "type";
          "q"; "quit"; "ast";
          "cenv"; "i"; "infos" ]

    let fileLoadingError (file: string) (error: string) =
        arrow ()
        setColor System.ConsoleColor.DarkYellow
        printf "Error when loading "
        setColor System.ConsoleColor.Yellow
        printf "%s" file
        setColor System.ConsoleColor.DarkYellow
        printfn ":\n\t%s" error
        resetColor ()

    let unexistingFile (file: string) =
        arrow ()
        setColor System.ConsoleColor.Yellow
        printf "%s" file
        setColor System.ConsoleColor.DarkYellow
        printfn " don't exist or cannot be found."
        resetColor ()

    let unexistingFunction (id: string) =
        arrow ()
        setColor System.ConsoleColor.DarkYellow
        printf "The function "
        setColor System.ConsoleColor.Yellow
        printf "%s" id
        setColor System.ConsoleColor.DarkYellow
        printfn " dosn't exist"
        resetColor ()

    let typecheckingError (msg: string) =
        arrow ()
        setColor System.ConsoleColor.DarkYellow
        printf "Typechecking error, "
        setColor System.ConsoleColor.Yellow
        printfn "%s" msg
        resetColor ()

