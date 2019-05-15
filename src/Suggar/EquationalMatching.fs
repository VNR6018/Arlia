module EquationalMatching

    open DAST
    open AST
    open Utils
    open Module

    let rec equationalMatchingDesugger (lst: (Identifier * Case list * Expression) list) exprDesugger =

        let id = Parser.fst3 lst.[0] // id is always the same
        let nbrCase = (Parser.snd3 lst.[0]).Length // the size of case is always the same

        let letArgs ts =
            let args = (Array.take nbrCase [| 'a' .. 'z' |]) // Pray `nbrCase` being <= 26...
                       |> List.ofArray
            List.map (fun s -> ts (string s)) args

        let cases =
            List.map
                (fun (_, case, value) ->
                    DAST_Case.Tuple
                        ((List.map (fun c -> caseifyExpr c exprDesugger) case)),
                        exprDesugger value)
                lst

        let matching =
            DAST_Expr.Match(DAST_Expr.Tuple (letArgs (fun ts -> DAST_Expr.AbstractIdentifier(QualName([], ts)))), cases)

        DAST_Statement.Decl(id, makeLambdas (letArgs Arg) matching)
