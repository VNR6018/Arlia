module Typechecker

    open System.Linq
    open DAST
    open AST
    open System.IO
    open Printter

    open Exceptions

    type Scheme = Scheme of string list * Type

    and TypeEnv = Map<QualName, Scheme>

    and Subst = Map<string, Type>

    and Type =
        | T_Integer | T_Float | T_Char | T_String                              // Basic types
        | T_Type | T_Effect of Type                                            // Special types
        | T_Typename of QualName | T_VarType of string                         // User defined types
        | T_DependentType of Type * DAST_Expr | T_Flag of string * Type        // Dependent types-based
        | T_Tuple of Type list | T_List of Type                                // Container types
        | T_App of Type * Type | T_Arrow of Type * Type                        // Applicated types
        | T_Record of (Identifier * Type) list                                 // Record type
    
    type Env =
        { file: System.IO.FileInfo option;
          mutable types: DataType list;
          mutable functions: Function list }
          with

               member this.filename =
                    if this.file.IsNone
                    then "prelude"
                    else this.file.Value.Name

               member this.containsFunction name =
                    this.functions |> List.exists (fun f -> f._name = name)

               member this.getFunction name =
                    this.functions
                    |> List.find (fun (f: Function) -> f._name = name)

               member this.getDataType name =
                    this.types
                    |> List.find (fun (f: DataType) -> f._name = name)

               member this.atLine n =
                    (System.IO.File.ReadAllLines this.filename).[n]

               member this.getFunctionType name =
                    (this.getFunction name)._type

               member this.hasFunctionImplementation name =
                    if this.containsFunction name
                    then (this.getFunction name)._value.IsSome
                    else false

               member this.getFunctionValue name =
                    (this.getFunction name)._value.Value

               member this.updateFunctionValue name value =
                    (this.getFunction name)._value <- Some value
                    this

               member this.updateFunctionType name ty =
                    (this.getFunction name)._type <- ty
                    this

               member this.containsDataType name =
                    this.types |> List.exists (fun d -> d._name = name)

               member this.addFunction name ty value =
                    { file = this.file;
                      types = this.types;
                      functions =
                        List.append
                            this.functions
                            [{ _name = name;
                               _type = ty;
                               _value = value }] }

               member this.addDataType_alias name alias =
                    { file = this.file;
                      types =
                        List.append
                            this.types
                            [{ _name = name;
                               _isAlias = true;
                               _constructors = None;
                               _alias = Some alias }]
                      functions = this.functions }

               member this.addDataType_sum name ctors =
                    { file = this.file;
                      types =
                        List.append
                            this.types
                            [{ _name = name;
                               _isAlias = false;
                               _constructors = Some ctors;
                               _alias = None }]
                      functions = this.functions }

    and Function =
        { _name: Identifier;
          mutable _type: Type;
          mutable _value: DAST_Expr option }

    and DataType =
        { _name: Identifier;
          _isAlias: bool;
          _constructors: Ctor list option
          _alias: Type option }

    //and Ctor = Term of Identifier | Product of Type list
    and Ctor = DAST_Ctor

    let newEnv file = { file = file; types = []; functions = [] }

    let qualNameToString (qualName: QualName) =
        match qualName with QualName(loc, id) ->
            System.String.Join(".", loc @ [id])

    let makeSimpleQualName id = QualName([], id)

    module Type =
        let rec ftv = function
            | T_Integer -> Set.empty
            | T_Float -> Set.empty
            | T_Char -> Set.empty
            | T_String -> Set.empty
            | T_VarType name -> Set.singleton name
            | T_Arrow(t1, t2) -> Set.union (ftv t1) (ftv t2)
            | T_App(t1, t2) -> Set.union (ftv t1) (ftv t2)
            | T_Tuple ts -> List.fold (fun acc t -> Set.union acc (ftv t)) Set.empty ts
            | T_List t -> Set.singleton (toString t)

        and apply s t =
            match t with
            | T_VarType name ->
                match Map.tryFind name s with
                | Some t -> t
                | None -> T_VarType name
            | T_Arrow(t1, t2) ->
                T_Arrow(apply s t1, apply s t2)
            | T_Integer | T_Float | T_Char | T_String -> t
            | T_Type -> T_Type
            | T_Tuple ts -> T_Tuple(List.map (fun t -> apply s t) ts)
            | T_List t -> T_List(apply s t)
            | _ -> t

        and parens s =
            sprintf "(%s)" s

        and braces s =
            sprintf "{ %s }" s

        and toString t =
            let rec parenType t' =
                match t' with
                | T_Arrow(_, _) -> parens (toString t')
                | _ -> toString t'
            
            let rec expr e =
                match e with
                | DAST_Expr.App(e1, e2) -> expr e1 + " (" + expr e2 + ")"
                | DAST_Expr.AbstractIdentifier id -> qualNameToString id
                | DAST_Expr.Literal lit ->
                    match lit with
                    | Int x -> x.ToString()
                    | Float x -> x.ToString()
                    | String x -> x.ToString()
                    | Char x -> x.ToString()
                | _ -> e.ToString()

            match t with
                | T_VarType name -> name
                | T_Integer -> "Integer"
                | T_Float -> "Float"
                | T_Char -> "Char"
                | T_String -> "String"
                | T_Arrow(t1, t2) ->
                    (parenType t1) + " -> " + (toString t2)
                | T_Typename name -> qualNameToString name
                | T_Type -> "Type"
                | T_Tuple ts -> sprintf "(%s)" (System.String.Join(", ", List.map toString ts))
                | T_List t -> sprintf "[%s]" (toString t)
                | T_DependentType(t, e) -> sprintf "%s %s" (toString t) (System.String.Join(" ", expr e))
                | T_Flag(s, t) -> sprintf "(%s: %s)" s (toString t)

    module Scheme =
        let rec ftv (scheme: Scheme) =
            match scheme with
            | Scheme(variables, t) ->
                Set.difference(Type.ftv t) (Set.ofList variables)
         
        and apply (s: Subst) (scheme: Scheme) =
            match scheme with
            | Scheme(variables, t) ->
                let newSubst = List.foldBack (fun key currentSubst -> Map.remove key currentSubst) variables s
                let newType = Type.apply newSubst t
                Scheme(variables, newType)

    module TypeEnv =
        let rec remove (env: TypeEnv) (var: QualName) =
            Map.remove var env

        and ftv (typeEnv: TypeEnv) =
            Seq.foldBack (fun (KeyValue(_, v)) state ->
                Set.union state (Scheme.ftv v)) typeEnv Set.empty

        and apply (s: Subst) (env: TypeEnv) =
            Map.map (fun _ value -> Scheme.apply s value) env

    module Subst =
        let compose s1 s2 =
            Map.union (Map.map (fun _ (v: Type) -> Type.apply s1 v) s2) s1


    let rec generalize (env: TypeEnv) (t: Type) =
        let variables =
            Set.difference (Type.ftv t) (TypeEnv.ftv env)
            |> Seq.toList
        Scheme(variables, t)


    and private currentId = ref 'a'

    and nextId () =
        let id = !currentId
        currentId := (char ((int !currentId) + 1))
        id

    and resetId () = currentId := 'a'

    and lambdaIdToString (lambdaId: LambdaId) =
        match lambdaId with
        | Arg x -> x
        | _ -> "_"

    and newTyVar () =
       T_VarType(sprintf "%c" (nextId ()))

    and instantiate (ts: Scheme) =
        match ts with
        | Scheme(variables, t) ->
            let nvars = variables |> List.map (fun name -> newTyVar ())
            let s = List.zip variables nvars |> Map.ofList
            Type.apply s t

    and varBind a t =
        match t with
        | T_VarType name when name = a -> Map.empty
        //| _ when Set.contains a (Type.ftv t) ->
        //    failwithf "Occur check fails: `%s` vs `%s`" a (Type.toString t)
        | _ -> Map.singleton a t

    and unify (t1: Type) (t2: Type) : Subst =
        match t1, t2 with
        | T_VarType a, t | t, T_VarType a -> varBind a t
        | T_Integer, T_Integer -> Map.empty
        | T_Float, T_Float -> Map.empty
        | T_Char, T_Char -> Map.empty
        | T_String, T_String -> Map.empty
        | T_Tuple ts, T_Tuple ts' ->
            if ts.Length <> ts'.Length
            then failwithf "Types do not unify: `%s` vs `%s`" (Type.toString t1) (Type.toString t2)
            else List.fold Subst.compose Map.empty (List.map2 unify ts ts')
        | T_List t, T_List t' ->
            unify t t'
        | T_Arrow(l, r), T_Arrow(l', r') ->
            let s1 = unify l l'
            let s2 = unify (Type.apply s1 r) (Type.apply s1 r')
            Subst.compose s2 s1
        | T_App(l, r), T_App(l', r') ->
            let s1 = unify l l'
            let s2 = unify (Type.apply s1 r) (Type.apply s1 r')
            Subst.compose s2 s1
        | T_Flag(_, t), t' ->
            unify t t'
        | _ -> failwithf "Types do not unify: `%s` vs `%s`" (Type.toString t1) (Type.toString t2)
    
    and tiLit = function
        | Literal.Int _ -> Type.T_Integer
        | Literal.Float _ -> Type.T_Float
        | Literal.Char _ -> Type.T_Char
        | Literal.String _ -> Type.T_String

    and dast_typeToType (env: Env) = function
        | DAST_Type.AppType(t1, t2) -> T_App(dast_typeToType env t1, dast_typeToType env t2)
        | DAST_Type.ArrType(t1, t2) -> T_Arrow(dast_typeToType env t1, dast_typeToType env t2)
        | DAST_Type.DependentType(t, e) -> T_DependentType(dast_typeToType env t, e)
        | DAST_Type.Effect t -> T_Effect (dast_typeToType env t)
        | DAST_Type.Flag(id, t) -> T_Flag(id, dast_typeToType env t)
        | DAST_Type.Tuple ts -> T_Tuple(List.map (dast_typeToType env) ts)
        | DAST_Type.Type               -> T_Type
        | DAST_Type.AbstractIdentifier id ->
            match id with
            | QualName ([], "Integer") -> T_Integer
            | QualName ([], "Float")   -> T_Float
            | QualName ([], "Char")    -> T_Char
            | QualName ([], "String")  -> T_String
            // !!!!!!!!! Vérifier avec 'env' !!!!!!!!!!!
            //| _                        -> T_Typename name
            | abstract_id ->
                if env.containsFunction (qualNameToString abstract_id)
                then
                    let f = env.getFunction (qualNameToString abstract_id)
                    printfn "ok"
                    T_Char
                else T_VarType (qualNameToString abstract_id)

                //if env |> Map.containsKey abstract_id
                //then
                //    let t = Map.find abstract_id env
                //    match t with Scheme (ids, ty) ->
                //        printfn "Type: %s" (Type.toString ty)
                //    T_Char
                //else T_VarType (qualNameToString abstract_id)
        | _                            -> failwithf "Internal | Not added again"

    and tiExpr (env: TypeEnv) (exp: DAST_Expr) : Subst * Type =
        match exp with
        /// !!!!!!!!!!!
        /// Ici, pour DAST.AbstractIdentifier :
        ///     Vérifier s'il s'agit de :
        ///         > Simple variable
        ///         > Fonction
        ///         > Constructeur
        ///         > Type
        /// Et inférer en ce sens
        ///     => Lancer une exception si une annotation de type est nécessaire ?!
        /// !!!!!!!!!!!
        | DAST_Expr.AbstractIdentifier name ->
            match Map.tryFind name env with
            | Some sigma ->
                let t = instantiate sigma
                (Map.empty, t)
            | None ->
                match name with
                // Is type ? => ok = T_Type
                | QualName ([], "Integer") -> (Map.empty, T_Type)

                //| _ -> (Map.empty, T_VarType (qualNameToString name))
                | _ -> // Créer un gestionnaire d'erreur :ok_hand:
                    Printter.Errors.unexistingFunction (qualNameToString name)
                    Utils.maybeYouWanted'infos
                        (qualNameToString name)
                        (List.map
                            (fun var ->
                                let info =
                                    match (env.TryFind var).Value with
                                        Scheme(_, t) -> Type.toString t
                                qualNameToString var, "`" + info + "`")
                            (env
                             |> Map.toList
                             |> List.map fst))
                    raise TypeCheckingErrorInsideHandled
                    //failwithf "Unbound variable: `%s`" (qualNameToString name)
        | DAST_Expr.Literal lit -> (Map.empty, tiLit lit)
        | DAST_Expr.App(e1, e2) ->
            let s1, t1 = tiExpr env e1
            let s2, t2 = tiExpr (TypeEnv.apply s1 env) e2
            let tv = newTyVar ()
            let s3 = unify (Type.apply s2 t1) (T_Arrow(t2, tv))
            (Subst.compose (Subst.compose s3 s2) s1, Type.apply s3 tv)
        | DAST_Expr.Let(id, expr, in') ->
            let s1, t1 = tiExpr env expr
            let env1 = TypeEnv.remove env (makeSimpleQualName id)
            let scheme = generalize (TypeEnv.apply s1 env) t1
            let env2 = Map.add (makeSimpleQualName id) scheme env1
            let s2, t2 = tiExpr (TypeEnv.apply s1 env2) in'
            (Subst.compose s2 s1, t2)
        | DAST_Expr.Lambda(id, value) ->
            let tv = newTyVar ()
            let env1 = TypeEnv.remove env (makeSimpleQualName (lambdaIdToString id))
            let env2 = Map.union env1 (Map.singleton (makeSimpleQualName (lambdaIdToString id)) (Scheme([], tv)))
            let s1, t1 = tiExpr env2 value
            (s1, T_Arrow(Type.apply s1 tv, t1))
        | DAST_Expr.Tuple values ->
            (Map.empty, T_Tuple(List.map (fun v -> snd (tiExpr env v)) values))
        | DAST_Expr.List ts ->
            let tv = newTyVar ()
            if ts.IsEmpty
            then (Map.empty, T_List tv)
            else let _, t1 = tiExpr env ts.[0]
                 if List.forall (fun t -> snd (tiExpr env t) = t1) ts
                 then (Map.empty, T_List t1)
                 else failwith "Not all items in the list are of the same type"
        | DAST_Expr.Match(expr, cases) ->
            let _, ety = tiExpr env expr
            let inT, retT = tiCase cases ety env
            (inT, retT)
        | _ -> failwithf "Not again added: %A" exp

    and tiCase patterns ft env =
        let cenv =
            let rec tcase = function
                | DAST_Case.Case expr -> snd (tiExpr env expr)
                | DAST_Case.Variable v -> snd (tiExpr env (DAST.AbstractIdentifier (makeSimpleQualName v)))
                | DAST_Case.Wildcard -> ft
                | DAST_Case.Tuple cuplets -> T_Tuple(List.map tcase cuplets)
                // ADD CTOR => Ctor x y -> ...
            let cases = List.map fst patterns in
            let tcases = List.map tcase cases in
            List.fold (fun subst tc -> Subst.compose subst (unify (Type.apply subst ft) tc)) Map.empty tcases
        let retTy =
            let values = List.map snd patterns in
            let tvalues = List.map (fun value -> snd (tiExpr env value)) values in
            let fvt = tvalues.First() in
            let outTy = List.fold (fun subst tc -> Subst.compose subst (unify (Type.apply subst fvt) tc)) cenv tvalues in
            Type.apply outTy fvt in
        (cenv, retTy)
        
    and expression_typeInference env exp =
        let s, t = tiExpr env exp
        Type.apply s t

    and updateExprTypeEnv (env: Env) =
        let mutable env' = Map.empty
        List.iter
            (fun (f: Function) ->
                env' <- env'.Add(makeSimpleQualName f._name, Scheme([f._name], f._type))
            ) env.functions
        env'
        
    let rec public typecheck_statement (env: Env) stmt =

        let exprEnv = updateExprTypeEnv env

        match stmt with
        | DAST_Statement.Signature(id, dastType) ->
            typecheck_signature env id dastType
        | DAST_Statement.Decl(id, value) ->
            typecheck_decl env id value exprEnv
        | DAST_Statement.AddOp -> env
        | DAST_Statement.SumType(tid, tvars, ctors) -> typecheck_sumType env tid tvars ctors

    and private typecheck_sumType (env: Env) id tvars ctors =
        // kind-checker ?
        // TODO

        // Modifier l'implémentation du parser / typechecker / desucreur pour les ajours de types

        let e = env.addDataType_sum id ctors
        //printfn "%A" e
        e

    and private typecheck_signature (env: Env) id dastType =
        if env.hasFunctionImplementation id
        then failwithf "The type of a function cannot be defined after its implementation (`%s`)" id
        else env.addFunction id (dast_typeToType env dastType) None

    and private typecheck_decl (env: Env) id value exprEnv =
        let _, t_exp = tiExpr exprEnv value
        if env.containsFunction id
        then if env.hasFunctionImplementation id
             then failwithf "Already declared function: `%s`" id
             else
                let t_sgn = (env.getFunction id)._type
                let unapp = try (Type.apply ((unify t_sgn t_exp)) t_exp)
                                |> Ok with exn -> failwithf "%s" exn.Message
                if match unapp with Result.Ok _ -> true
                then env.updateFunctionValue id value
                else failwithf "The signature of `%s` is different than the type of its value\n  (`%s` vs `%s`)"
                                       id (Type.toString t_sgn) (Type.toString t_exp)
        else env.addFunction id t_exp (Some value)

    let typecheck_document (file: FileInfo option) document =
        printfn "typecheck .\\%s" file.Value.Name
        List.fold
            (fun acc stmt -> typecheck_statement acc stmt)
            (newEnv file)
            (match document with DProgram stmts -> stmts)
