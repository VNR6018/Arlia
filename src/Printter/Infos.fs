module Printter.Infos

    open Printter.Utils

    let public bye () =
        setColor System.ConsoleColor.White
        printfn "See you later!\n"
        resetColor ()

    let public help text =
        setColor System.ConsoleColor.White
        printfn "%s" text
        resetColor ()

    let public printInfoType (expr: string) (ty: string) =
        setColor System.ConsoleColor.White
        printf "-> "
        setColor System.ConsoleColor.Yellow
        printf "%s " expr
        setColor System.ConsoleColor.Cyan
        printfn ": %s" ty
        resetColor ()

