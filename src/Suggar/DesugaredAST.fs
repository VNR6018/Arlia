module DAST

    open AST

    type DAST_Expr =
        
        | Let of Identifier * DAST_Expr * DAST_Expr

        | Literal of Literal

        | ObjectReference

        | Lambda of LambdaId * DAST_Expr

        | List of DAST_Expr List

        | UseEffect of QualName * DAST_Type list * DAST_Expr

        | Hole of Identifier

        | Tuple of DAST_Expr list

        | Match of DAST_Expr * (DAST_Case * DAST_Expr) list

        | App of DAST_Expr * DAST_Expr

        /// Var, type, function, ctor, ...
        | AbstractIdentifier of QualName

        | HandleEffect of DAST_Expr * (DAST_Case * DAST_Expr) list

        | HandlerCall of Identifier * DAST_CtorParam list

    and DAST_Statement =
        
        | Module of Module

        | AddOp

        | Decl of Identifier * DAST_Expr

        | Signature of Identifier * DAST_Type

        | Class of Identifier * DAST_VariablesTypes * DAST_Methods

        | NewEffect of Identifier * DAST_VariablesTypes * DAST_Methods

        | SumType of Identifier * DAST_VariablesTypes * DAST_Ctor list

        | Handler of Identifier * DAST_VariablesTypes * DAST_Type

        | External of QualName * DAST_Type

        | Import of string

        | Using of QualName list

        | Include of string * Identifier

        | InsideCLI of string

    and DAST_Type =

        | Tuple of DAST_Type list

        | ArrType of DAST_Type * DAST_Type

        | AppType of DAST_Type * DAST_Type

        | Flag of Identifier * DAST_Type

        | RefinedType of Identifier * DAST_Type * DAST_Predicate list

        | DependentType of DAST_Type * DAST_Expr

        | Type

        | AbstractIdentifier of QualName

        | Effect of DAST_Type

    and DAST_Predicate =

        | Predicate of DAST_Expr

        | Requirement of DAST_Statement

    and DAST_Case =

        | Variable of Identifier

        | Case of DAST_Expr

        | Tuple of DAST_Case list

        | Wildcard

    and DAST_CtorParam = DAST_Case

    and DAST_Methods = Methods of (Accessibility * DAST_Statement) list

    and DAST_VariablesTypes = VariablesTypes of (Identifier * DAST_Type) list

    and DAST_DataType =

        | AlgebraicType of DAST_Ctor list

        | Alias of DAST_Type

    and DAST_Ctor =

        | Product of Identifier * DAST_Type list

        | Term of Identifier

    and Module = Identifier * Exposing * DAST_Statement list

    and DProgram = DProgram of DAST_Statement list
