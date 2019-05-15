module Utils

    open DAST
    open System.Linq
    open AST

    let rec mapAccInsider f xs =
        List.map (fun (acc, e) -> (acc, f e)) xs

    and decomposeMethod (methods: Methods) =
        match methods with
        | AST.Methods r -> List.map (fun (x, y) -> (x, y)) r

    and caseifyExpr case exprDesugger =
        match case with
        | Case.Case ex -> DAST_Case.Case(exprDesugger ex)
        | Case.Variable id -> DAST_Case.Variable id
        | Case.Wildcard -> DAST_Case.Wildcard
        | Case.Tuple cuplets -> DAST_Case.Tuple (List.map (fun cuplet -> caseifyExpr cuplet exprDesugger) cuplets)

    and makeLambdas args value =
        let rec tex n =
            DAST_Expr.Lambda((Parser.remove 0 args).Reverse().ElementAt(n - 1),
                if n <> 1 then tex (n - 1)
                else value)
        if args.Length = 1
        then makeLambda args.[0] value
        else makeLambda (args.[0]) (tex (args.Length - 1))

    and makeLambda arg value = DAST_Expr.Lambda (arg, value)

    and isQualIdentifierConsistency (id: QualName) =
        // TODO -- check if the QualName is consistent 
        true

    and qualToString (id: QualName) =
        match id with
            QualName(loc, id') ->
                System.String.Join(".", loc @ [id'])
