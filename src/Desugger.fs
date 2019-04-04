module Desugger

    open AST
    
    open System.Linq

    exception Addop

    type DAST_Expr =
        
        | Module of Identifier * Exposing * DAST_Expr list

        | AddOp

        | Let of Identifier * DAST_Expr

        | Signature of Identifier * Type

        | Literal of Literal

        | ObjectReference

        | Class of Identifier * VariablesTypes * DAST_Methods

        | Lambda of LambdaId * DAST_Expr

        | List of DAST_Expr List

        | NewEffect of Identifier * VariablesTypes * DAST_Methods

        | NewType of Identifier * VariablesTypes * DataType * As option * DAST_Methods

        | UseEffect of QualName * Type list * DAST_Expr

        | CtorCall of Identifier * DAST_CtorParam list

        | Exclaimer of DAST_Expr

        | Hole of Identifier

        | Tuple of DAST_Expr list

        | ZeroObject

        | Match of DAST_Expr list * (DAST_Case list * DAST_Expr) list

        | TupleMatch of DAST_Case list

        | App of DAST_Expr * DAST_Expr

        | Var of QualName

        | ObjMethodCall of DAST_Expr * QualName

        | Record of (Identifier * DAST_Expr option) list

        | FirstClassType of Type

        | HandleEffect of DAST_Expr * (DAST_Case * DAST_Expr) list

        | Handler of Identifier * VariablesTypes * Type

        | HandlerCall of Identifier * CtorParam list

        | External of QualName * Type

        | Import of string

        | Using of QualName list

        | Include of string * Identifier

        | InsideCLI of string

    and DAST_Case =

        | Variable of Identifier

        | Case of DAST_Expr

        | Wildcard

    and DAST_CtorParam = DAST_Case

    and DAST_Methods = Methods of (Accessibility * DAST_Expr) list

    and DProgram = DProgram of DAST_Expr list

    let rec private mapAccInsider f xs =
        List.map (fun (acc, e) -> (acc, f e)) xs

    and private decomposeMethod (methods : Methods) =
        match methods with
        | AST.Methods r -> List.map (fun (x, y) -> (x, y)) r

    and private caseifyExpr case =
        match case with
        | Case.Case ex -> DAST_Case.Case(exprDesugger ex)
        | Case.Variable id -> DAST_Case.Variable id
        | Case.Wildcard -> DAST_Case.Wildcard

    and private makeLambdas args value =
        let rec tex n =
            DAST_Expr.Lambda((Parser.remove 0 args).Reverse().ElementAt(n - 1),
                if n <> 1 then tex (n - 1)
                else value)
        if args.Length = 1
        then makeLambda args.[0] value
        else makeLambda (args.[0]) (tex (args.Length - 1))

    and private makeLambda arg value = DAST_Expr.Lambda (arg, value)

    and exprDesugger expr : DAST_Expr =
        match expr with
        | AST.Module(id, exposing, body) -> moduleSpecialized id exposing body
        
        | AST.Infix(lvalue, op, rvalue) ->
            DAST_Expr.App
                (DAST_Expr.App(DAST_Expr.Var(QualName([], op)), exprDesugger lvalue),
                 exprDesugger rvalue)
        
        | AST.Let(id, value) ->
            DAST_Expr.Let(id, exprDesugger value)
        
        | AST.Signature(id, ty) ->
            DAST_Expr.Signature(id, ty)

        | AST.Expression.Class(id, tys, body) ->
            DAST_Expr.Class
                (id, tys, Methods(mapAccInsider exprDesugger (decomposeMethod body)))
        
        | AST.Expression.Lambda(id, value) ->
            DAST_Expr.Lambda(id, exprDesugger value)
        
        | AST.Expression.List xs -> DAST_Expr.List(List.map exprDesugger xs)
        
        | AST.Expression.NewEffect(id, tys, body) ->
            DAST_Expr.NewEffect
                (id, tys, Methods(mapAccInsider exprDesugger (decomposeMethod body)))
        
        | AST.Expression.NewType(id, tys, ty, as', body) ->
            let dataType =
                match ty with
                | DataType.AlgebraicType clst -> DataType.AlgebraicType (
                    List.map (fun t ->
                        match t with
                        | Ctor.Product(id, targs) -> Ctor.Product (id, targs)
                        | _ -> t) clst)
                | DataType.Alias t -> DataType.Alias t

            DAST_Expr.NewType
                (id, tys, dataType, as',
                 Methods(mapAccInsider exprDesugger (decomposeMethod body)))
        
        | AST.Expression.UseEffect(id, tys, value) ->
            DAST_Expr.UseEffect(id, tys, exprDesugger value)
        
        | AST.Expression.CtorCall(id, parameters) ->
            DAST_Expr.CtorCall(id, (List.map caseifyExpr parameters))
        
        | AST.Expression.Exclaimer e ->
            DAST_Expr.Exclaimer(exprDesugger e)
        
        | AST.Expression.Tuple xs ->
            DAST_Expr.Tuple(List.map exprDesugger xs)
        
        | AST.Expression.EquationalMatching lst ->
            equationalMatchingSpecialized lst
        
        | AST.Expression.Match(toMatch, cases) ->
            DAST_Expr.Match(List.map exprDesugger toMatch,
                List.map (fun (x, y) -> ((List.map caseifyExpr x), exprDesugger y)) cases)
        
        | AST.Expression.App(e1, e2) ->
            DAST_Expr.App(exprDesugger e1, exprDesugger e2)
                
        | AST.Expression.ObjMethodCall(expr, id) ->
            DAST_Expr.ObjMethodCall(exprDesugger expr, id)
        
        | AST.Expression.Record lst ->
            DAST_Expr.Record(List.map (fun (id, expr : Expression option) ->
                (id, (if expr.IsSome
                      then Some(exprDesugger expr.Value)
                      else None))) lst)
        
        | AST.Expression.TupleMatch cases ->
            let cases = List.map (fun case -> caseifyExpr case) cases
            DAST_Expr.TupleMatch cases
                
        | AST.Expression.IfElifs(if', elifs) ->
        
            let toMatch = [exprDesugger (fst if')]
            let cases =
                let c1 = DAST_Case.Case (CtorCall("True", []))
                let cs'1 = ([c1], exprDesugger (snd if'))
                cs'1 :: (List.map (fun c -> ([DAST_Case.Case (exprDesugger (fst c))]),
                            exprDesugger (snd c)) elifs)
            DAST_Expr.Match(toMatch, cases)

        | AST.Expression.IfElifsElse(if', elifs, else') ->
            
            let toMatch = [exprDesugger (fst if')]
            let cases =
                let c1 = DAST_Case.Case (CtorCall("True", []))
                let cs'1 = ([c1], exprDesugger (snd if'))
                cs'1 :: (List.map (fun c -> ([DAST_Case.Case (exprDesugger (fst c))]),
                            exprDesugger (snd c)) elifs)
                     @  ([[DAST_Case.Wildcard], exprDesugger else'])
            DAST_Expr.Match(toMatch, cases)

        | AST.Expression.FirstClassType ty ->
            DAST_Expr.FirstClassType ty

        | AST.Expression.HandleEffect(toHandle, cases) ->
            DAST_Expr.HandleEffect(exprDesugger toHandle,
                List.map (fun (x, y) -> ((caseifyExpr x), exprDesugger y)) cases)

        | AST.Expression.External(id, ty) ->
            DAST_Expr.External(id, ty)

        | AST.Expression.Handler(id, tys, ty) ->
            DAST_Expr.Handler(id, tys, ty)

        | AST.Expression.HandlerCall(id, parameters) ->
            DAST_Expr.HandlerCall(id, parameters)

        | AST.Expression.Hole id ->
            DAST_Expr.Hole id

        | AST.Expression.Import uri ->
            DAST_Expr.Import uri
        
        | AST.Expression.Include(uri, as') ->
            DAST_Expr.Include(uri, as')

        | AST.Expression.InsideCLI cmd ->
            DAST_Expr.InsideCLI cmd

        | AST.Expression.Literal lit ->
            DAST_Expr.Literal lit

        | AST.Expression.ObjectReference ->
            DAST_Expr.ObjectReference

        | AST.Expression.Using ids ->
            DAST_Expr.Using ids

        | AST.Expression.Var id ->
            DAST_Expr.Var id

        | AST.Expression.ZeroObject ->
            DAST_Expr.ZeroObject

        | AST.Expression.AddOp ->
            DAST_Expr.AddOp

    and private moduleSpecialized id exposing body =
        let exposed =
            match exposing with
            | AST.Exposing ids -> ids
        if exposed.IsEmpty then
            let bodybdy body =
                let kind instr =
                    match instr with
                    | AST.Signature(id, _) -> id
                    | AST.NewType(id, _, _, _, _) -> id
                    | AST.NewEffect(id, _, _) -> id
                    | AST.Let(id, _) -> id
                    | AST.Class(id, _, _) -> id
                    | AST.Handler(id, _, _) -> id
                    | _ -> "_"
                List.map kind body
                |> List.distinct
            DAST_Expr.Module(id, Exposing(bodybdy body), List.map exprDesugger body)
        else DAST_Expr.Module(id, exposing, List.map exprDesugger body)


    and private equationalMatchingSpecialized (lst: (Identifier * Case list * Expression) list) =

        let id = Parser.fst3 lst.[0] // id is always the same
        let nbrCase = (Parser.snd3 lst.[0]).Length // the size of case is always the same

        let letArgs ts =
            let args = List.take nbrCase ([ 'a'..'z' ]) // Pray `nbrCase` < 27
            List.map (fun s -> ts (s.ToString())) args

        let cases =
            List.map
                (fun ice -> (List.map caseifyExpr (Parser.snd3 ice), exprDesugger (Parser.thr3 ice))) lst

        let matching =
            DAST_Expr.Match(letArgs (fun ts -> Var(QualName([], ts))), cases)

        DAST_Expr.Let(id, makeLambdas (letArgs Arg) matching)

    
    and public desugger prog =
        DProgram (match prog with Program p -> List.map exprDesugger p)
