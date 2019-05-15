module Type

    open DAST
    open AST
    open Utils

    let rec variablesTypesDesugger vt exprDesugger statementDesugger =
        match vt with
        | VariablesTypes ts ->
            DAST_VariablesTypes.VariablesTypes
                (List.map (fun (id, t) -> (id, typeDesugger t exprDesugger statementDesugger)) ts)

    and typeDesugger ty f f' =
        match ty with
        | Type.RefinedType(id, ty, predicates) ->
            let refMap p =
                match p with
                | Predicate.Predicate ex ->
                    DAST_Predicate.Predicate (f ex)
                | Predicate.Requirement signature ->
                    DAST_Predicate.Requirement (f' signature)
            DAST_Type.RefinedType(id, typeDesugger ty f f', List.map refMap predicates)
        | Type.DependentType(t, e) ->
            DAST_Type.DependentType(typeDesugger t f f', f e)
        | Type.Effect e -> DAST_Type.Effect (typeDesugger e f f')
        | Type.AppType(t1, t2) -> DAST_Type.AppType(typeDesugger t1 f f', typeDesugger t2 f f')
        | Type.ArrType(t1, t2) -> DAST_Type.ArrType(typeDesugger t1 f f', typeDesugger t2 f f')
        | Type.Flag(id, t) -> DAST_Type.Flag(id, typeDesugger t f f')
        | Type.Infered | Type.Type -> DAST_Type.Type
        | Type.Tuple ts -> DAST_Type.Tuple(List.map (fun t -> typeDesugger t f f') ts)
        | Type.AbstractIdentifier id -> DAST_Type.AbstractIdentifier id

    let ctorDesugger ctor ed sd =
        match ctor with
        | AST.Ctor.Term atom -> DAST_Ctor.Term atom
        | AST.Ctor.Product (atom, tys) -> DAST_Ctor.Product(atom, List.map (fun t -> typeDesugger t ed sd) tys)


    //let dataTypeDesugger ty exprDesugger statementDesugger =
    //    match ty with
    //    | DataType.AlgebraicType clst -> DAST_DataType.AlgebraicType (
    //        List.map (fun t ->
    //            match t with
    //            | Ctor.Product(id, targs) ->
    //                DAST_Ctor.Product (id, List.map (fun t -> typeDesugger t exprDesugger statementDesugger) targs)
    //            | Ctor.Term id -> DAST_Ctor.Term id) clst)
    //    | DataType.Alias t -> DAST_DataType.Alias (typeDesugger t exprDesugger statementDesugger)
