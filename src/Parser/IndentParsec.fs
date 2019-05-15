(*
    "IndentFParsec" comes from https://github.com/manio143/IndentFParsec (manio143).

    This module is an external extension of the FParsec parser combiner allowing indentation analysis
    according to the paper "Principled Parsing for Indentation-Sensitive Languages"
    (https://michaeldadams.org/papers/layout_parsing/LayoutParsing.pdf)

    It revolves around 4 indentation parsing functions:

        - `exact pos p`:
                    Allows to check if the position of the parser currently being analyzed (`p`)
                    is on the same line as the defined position (`pos`).

        - `greater pos p`:
                    Allows to check if the position of the parser currently being analyzed (`p`)
                    is further away than the defined position (`pos`).

        - `atLeast pos p`:
                    Allows to check if the position of the parser currently being analyzed (`p`)
                    is on the same OR line further than the defined position (`pos`).

        - `any p`
                    Simply check if the parser (`p`) exists, regardless of its position in the text to be analyzed.
*)

module IndentParser

    open FParsec

    type Indentation = 
            | Fail
            | Any
            | Greater of Position 
            | Exact of Position 
            | AtLeast of Position 
            | StartIndent of Position
            with
                member this.Position = match this with
                        | Any | Fail -> None
                        | Greater p -> Some p
                        | Exact p -> Some p
                        | AtLeast p -> Some p
                        | StartIndent p -> Some p

    type IndentState<'T> = { Indent : Indentation; UserState : 'T }
    type CharStream<'T> = FParsec.CharStream<IndentState<'T>>
    type IndentParser<'T, 'UserState> = Parser<'T, IndentState<'UserState>>

    let indentState u = {Indent = Any; UserState = u}
    let runParser p u s = runParserOnString p (indentState u) "" s
    let runParserOnFile p u path = runParserOnFile p (indentState u) path System.Text.Encoding.UTF8

    let getIndentation : IndentParser<_,_> =
        fun stream -> match stream.UserState with
                                    | {Indent = i} -> Reply i
    let getUserState : IndentParser<_,_> =
        fun stream -> match stream.UserState with
                                    | {UserState = u} -> Reply u

    let putIndentation newi : IndentParser<unit, _> =
        fun stream ->
            stream.UserState <- {stream.UserState with Indent = newi}
            Reply(Unchecked.defaultof<unit>)

    let failf fmt = fail << sprintf fmt

    let acceptable i (pos : Position) =
        match i with
        | Any _ -> true
        | Fail -> false
        | Greater bp -> bp.Column < pos.Column
        | Exact ep -> ep.Column = pos.Column
        | AtLeast ap -> ap.Column <= pos.Column
        | StartIndent _ -> true

    let nestableIn i o =
        match i, o with
        | Greater i, Greater o -> o.Column < i.Column
        | Greater i, Exact o -> o.Column < i.Column
        | Exact i, Exact o -> o.Column = i.Column
        | Exact i, Greater o -> o.Column <= i.Column
        | _, _ -> true

    //let tokeniser p = parse {
    //    let! pos = getPosition
    //    let! i = getIndentation
    //    if acceptable i pos then return! p
    //    else return! fail "incorrect indentation"
    //}

    let tokeniser p = parse {
        let! pos = getPosition
        let! i = getIndentation
        return! p
    }

    let nestP i o p = parse {
        do! putIndentation i
        let! x = p
        do! notFollowedBy (tokeniser anyChar) <?> (sprintf "unterminated %A" i)
        do! putIndentation o
        return x
    }

    let nest indentor p = parse {
        let! outerI = getIndentation
        let! curPos = getPosition
        let innerI = indentor curPos
        if nestableIn innerI outerI
        then return! nestP innerI outerI p
        else return! nestP Fail outerI p
    }

    let nestWithPos indentor pos p = parse {
        let! outerI = getIndentation
        let innerI = indentor pos
        if nestableIn innerI outerI
        then return! nestP innerI outerI p
        else return! nestP Fail outerI p
    }

    let neglectIndent p = parse {
        let! o = getIndentation
        do! putIndentation Any
        let! x = p
        do! putIndentation o
        return x
    }

    let checkIndent<'u> : IndentParser<unit, 'u> = tokeniser (preturn ())

    let indented<'a,'u> i (p : Parser<'a,_>) : IndentParser<_, 'u> = parse {
        do! putIndentation i
        do! spaces
        return! tokeniser p
    }

    /// Allows to check if the position of the parser currently being analyzed (`p`)
    /// is on the same line as the defined position (`pos`).
    let exact<'a,'u> pos p: IndentParser<'a, 'u> = indented (Exact pos) p

    /// Allows to check if the position of the parser currently being analyzed (`p`)
    /// is further away than the defined position (`pos`).
    let greater<'a,'u> pos p: IndentParser<'a, 'u> = indented (Greater pos) p

    /// Allows to check if the position of the parser currently being analyzed (`p`)
    /// is on the same OR line further than the defined position (`pos`).
    let atLeast<'a,'u> pos p: IndentParser<'a, 'u> = indented (AtLeast pos) p

    /// Simply check if the parser (`p`) exists, regardless of its position in the text to be analyzed.
    let any<'a,'u> pos p: IndentParser<'a, 'u> = indented Any p

    let newline<'u> : IndentParser<unit, 'u> = many (skipAnyOf " \t" <?> "whitespace") >>. newline |>> ignore

    let rec blockOf p = parse {
        do! spaces
        let! pos = getPosition        
        let! x = exact pos p
        let! xs = attempt (exact pos <| blockOf p) <|> preturn []
        return x::xs
    }
