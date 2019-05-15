module Printter.Utils

    let public setColor (color: System.ConsoleColor) =
        System.Console.ForegroundColor <- color

    let public resetColor () =
        System.Console.ResetColor ()

    let public arrow () =
        setColor System.ConsoleColor.White
        printf "-> "
        resetColor ()

    let inline private min3 one two three = 
        if one < two && one < three then one
        elif two < three then two
        else three

    let private levenshtein (s: string) (t: string) =
        let m = s.Length
        let n = t.Length
        let d = Array2D.create (m+1) (n+1) -1
        let rec dist =
            function
            | i, 0 -> i
            | 0, j -> j
            | i, j when d.[i,j] <> -1 -> d.[i,j]
            | i, j ->
                let dval = 
                    if s.[i-1] = t.[j-1] then dist (i-1, j-1)
                    else
                        min3
                            (dist (i-1, j)   + 1)
                            (dist (i,   j-1) + 1)
                            (dist (i-1, j-1) + 1)
                d.[i, j] <- dval; dval 
        dist (m, n)

    /// choices : [(item: string, info: string)]
    let public maybeYouWanted'infos (source : string) (choices : (string * string) list) =
        setColor System.ConsoleColor.DarkGreen
        printfn "%s"
            (let s = (System.String.Join
                (", ", (List.map (fun ((item: string), info: string) ->
                    if (levenshtein source item <= 2 || source.Contains item) && item.Length > 1
                    then sprintf "`%s` (%s)" item info
                    else "") choices)
                |> List.where (fun item -> item <> ""))
                |> fun lst -> let place = lst.LastIndexOf ", "
                              if place = -1 then lst
                              elif place <= 0 then ""
                              else lst.Remove(place, ", ".Length).Insert(place, " or "))
             if s = String.empty then "No suggestion"
             else sprintf "Maybe you wanted: %s?" s)
        resetColor ()

    let public maybeYouWanted (source : string) (choices : string list) =
        setColor System.ConsoleColor.DarkGreen
        printfn "%s"
            (let s = (System.String.Join
                (", ", (List.map (fun (item: string) ->
                    if (levenshtein source item <= 2 || source.Contains item) && item.Length > 1
                    then sprintf "%s" item
                    else "") choices)
                |> List.where (fun item -> item <> ""))
                |> fun lst -> let place = lst.LastIndexOf ", "
                              if place = -1 then lst
                              elif place <= 0 then ""
                              else lst.Remove(place, ", ".Length).Insert(place, " or "))
             if s = String.empty then "No suggestion"
             else sprintf "Maybe you wanted: %s?" s)
        resetColor ()

