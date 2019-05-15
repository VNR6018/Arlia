module Module

    open DAST
    open AST

    let moduleDesugger id exposing body stmtDesugger =
        let exposed =
            match exposing with
            | AST.Exposing ids -> ids
        if exposed.IsEmpty then
            let bodybdy body =
                let kind instr =
                    match instr with
                    | AST.Signature(id, _) -> id
                    | AST.SumType(id, _, _) -> id
                    | AST.NewEffect(id, _, _) -> id
                    | AST.Decl(id, _) -> id
                    | AST.Class(id, _, _) -> id
                    | AST.Handler(id, _, _) -> id
                    | _ -> "_"
                List.map kind body
                |> List.distinct |> List.filter (fun c -> c <> "_")
            DAST_Statement.Module(id, Exposing(bodybdy body), List.map stmtDesugger body)
        else DAST_Statement.Module(id, exposing, List.map stmtDesugger body)

