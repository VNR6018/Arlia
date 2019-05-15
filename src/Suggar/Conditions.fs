module Conditions

    open DAST
    open AST

    let rec private cases if' elifs exprDesugger =
        let c1 = DAST_Case.Case (DAST.AbstractIdentifier (QualName([], "True")))
        let cs'1 = (c1, exprDesugger (snd if'))
        cs'1 :: (List.map (fun c -> (DAST_Case.Case (exprDesugger (fst c))),
            exprDesugger (snd c)) elifs)

    /// if-elif  -->  match
    and ifelifDesugger if' elifs exprDesugger =
        let toMatch = exprDesugger (fst if')
        let cases = cases if' elifs exprDesugger
        DAST_Expr.Match(toMatch, cases)

    /// if-elif-else  -->  match
    and ifelifelseDesugger if' elifs else' exprDesugger =
        let toMatch = exprDesugger (fst if')
        let cases = cases if' elifs exprDesugger @ ([DAST_Case.Wildcard, exprDesugger else'])
        DAST_Expr.Match(toMatch, cases)
