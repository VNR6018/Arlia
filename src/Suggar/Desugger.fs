module Desugger

    open DAST
    open AST
    open Module
    open EquationalMatching
    open Type
    open Conditions
    open Utils
    
    let rec statementDesugger stmt =

        match stmt with
        | AST.Module(id, exposing, body) ->
            moduleDesugger id exposing body statementDesugger

        | AST.Decl(id, value) ->
            DAST_Statement.Decl(id, exprDesugger value)

        | AST.Signature(id, ty) ->
            DAST_Statement.Signature(id, typeDesugger ty exprDesugger statementDesugger)

        | AST.Class(id, tys, body) ->
            DAST_Statement.Class
                (id,
                variablesTypesDesugger tys exprDesugger statementDesugger,
                DAST_Methods.Methods(mapAccInsider statementDesugger (decomposeMethod body)))

        | AST.NewEffect(id, tys, body) ->
            DAST_Statement.NewEffect
                (id,
                variablesTypesDesugger tys exprDesugger statementDesugger,
                DAST_Methods.Methods(mapAccInsider statementDesugger (decomposeMethod body)))
        
        | AST.SumType(tid, tvars, ctors) ->
            DAST_Statement.SumType
                (tid,
                 variablesTypesDesugger tvars exprDesugger statementDesugger,
                 List.map (fun ctor -> ctorDesugger ctor exprDesugger statementDesugger) ctors)

        | AST.EquationalMatching lst ->
            equationalMatchingDesugger lst exprDesugger

        | AST.External(id, ty) ->
            DAST_Statement.External(id, typeDesugger ty exprDesugger statementDesugger)

        | AST.Handler(id, tys, ty) ->
            DAST_Statement.Handler(id,
                variablesTypesDesugger tys exprDesugger statementDesugger,
                typeDesugger ty exprDesugger statementDesugger)

        | AST.Import uri ->
            DAST_Statement.Import uri
        
        | AST.Include(uri, as') ->
            DAST_Statement.Include(uri, as')

        | AST.InsideCLI cmd ->
            DAST_Statement.InsideCLI cmd

        | AST.Using ids ->
            DAST_Statement.Using ids

        | AST.AddOp ->
            DAST_Statement.AddOp



    and exprDesugger (expr: Expression) : DAST_Expr =
        match expr with
        
        | AST.Infix(lvalue, op, rvalue) ->
            DAST_Expr.App
                (DAST_Expr.App(DAST_Expr.AbstractIdentifier(QualName([], op)), exprDesugger lvalue),
                 exprDesugger rvalue)
        
        | AST.Let(id, exp, in') ->
            DAST_Expr.Let(id, exprDesugger exp, exprDesugger in')
                
        | AST.Expression.Lambda(id, value) ->
            DAST_Expr.Lambda(id, exprDesugger value)
        
        | AST.Expression.List xs -> DAST_Expr.List(List.map exprDesugger xs)
                
        | AST.Expression.UseEffect(id, tys, value) ->
            DAST_Expr.UseEffect(id,
                List.map (fun t -> typeDesugger t exprDesugger statementDesugger) tys,
                exprDesugger value)
        
        | AST.Expression.Tuple xs ->
            DAST_Expr.Tuple(List.map exprDesugger xs)
                
        | AST.Expression.Match(toMatch, cases) ->
            let cs = List.map (fun (c, e) -> caseifyExpr c exprDesugger, exprDesugger e) cases
            in DAST_Expr.Match(exprDesugger toMatch, cs)
        
        | AST.Expression.App(e1, e2) ->
            DAST_Expr.App(exprDesugger e1, exprDesugger e2)
                
        | AST.Expression.ObjMethodCall(expr, id) ->
            DAST_Expr.App(makeLambda (Arg "a")
                (DAST_Expr.AbstractIdentifier(QualName(["a"], id))),
                exprDesugger expr)

        | AST.Expression.Record lst ->
            DAST_Expr.Tuple(List.map (fun (_, x) -> exprDesugger x) lst)
        
        | AST.Expression.IfElifsElse(if', elifs, else') ->
            ifelifelseDesugger if' elifs else' exprDesugger

        | AST.Expression.HandleEffect(toHandle, cases) ->
            DAST_Expr.HandleEffect(exprDesugger toHandle,
                List.map (fun (x, y) -> ((caseifyExpr x exprDesugger),
                    exprDesugger y)) cases)

        | AST.Expression.HandlerCall(id, parameters) ->
            DAST_Expr.HandlerCall(id,
                List.map (fun c -> caseifyExpr c exprDesugger) parameters)

        | AST.Expression.Hole id ->
            DAST_Expr.Hole id

        | AST.Expression.Literal lit ->
            DAST_Expr.Literal lit

        | AST.Expression.ObjectReference ->
            DAST_Expr.ObjectReference

        | AST.Expression.AbstractIdentifier id ->
            if isQualIdentifierConsistency id
            then DAST.AbstractIdentifier id
            else failwithf "Incorrect identifier consistency: %s" (qualToString id)

    and public desugger prog =
        DProgram(match prog with Program stmt -> List.map statementDesugger stmt)
