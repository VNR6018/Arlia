module AST

    type Position = FParsec.Position

    type Identifier = string

    and Location = Identifier list

    /// Math.cos  -->  Qual ["Math"] "cos"
    /// or
    /// Example.Color  -->  Qual ["Example"] "Color"
    and QualName = QualName of Location * Identifier

    and Type =
    
        /// typename / variable type / function / ...
        | AbstractIdentifier of QualName

        /// T e
        | DependentType of Type * Expression

        /// (x: T)
        | Flag of Identifier * Type

        /// (T1, T2, ...)
        | Tuple of Type list

        /// T1 -> T2
        | ArrType of Type * Type

        /// T t
        | AppType of Type * Type

        /// { v: T | P(x) }
        | RefinedType of Identifier * Type * Predicate list

        /// Type <=> represent all types
        | Type

        /// Effect => represents an effect, and its result data type
        | Effect of Type

        /// When the type is not specified, it will be inferred from the type checking
        | Infered

    and VariableTypes = VariableTypes of Identifier list

    and Predicate =

        | Predicate of Expression

        | Requirement of Statement

    and VariablesTypes = VariablesTypes of (Identifier * Type) list

    and Expression =

        | Let of Identifier * Expression * Expression

        | Lambda of LambdaId * Expression

        | Match of Expression * (Case * Expression) List

        | Literal of Literal

        | HandlerCall of Identifier * CtorParam list

        | Record of (Identifier * Expression) list

        | IfElifsElse of
            (Expression * Expression) *        // if
            (Expression * Expression) list *   // elifs
            (Expression)                       // else

        | Tuple of Expression list

        | List of Expression list

        | App of Expression * Expression

        | Infix of Expression * string * Expression

        | AbstractIdentifier of QualName

        /// (<Expr>).identifier
        | ObjMethodCall of Expression * Identifier

        /// Represents a reference of a defined object within its own implementation.
        /// In traditional object oriented languages it is `this`, in Arlia we have opted for `it`.
        | ObjectReference

        /// A hole allows to receive information on a specific part of the code,
        /// without it being complete
        | Hole of Identifier

        | UseEffect of QualName * Type list * Expression

        | HandleEffect of Expression * (Case * Expression) List

    and Statement =

        | Signature of Identifier * Type

        | Decl of Identifier * Expression

        /// pattern matching as function(s)
        | EquationalMatching of (Identifier * Case list * Expression) list
        
        /// include <file> as identifier
        | Include of string * Identifier

        /// Import a file (.arl)
        | Import of string

        /// An external function
        | External of QualName * Type

        | Handler of Identifier * VariablesTypes * Type

        /// Use a module
        | Using of QualName list

        | Class of Identifier * VariablesTypes * Methods

        | SumType of Identifier * VariablesTypes * Ctor list

        | NewEffect of Identifier * VariablesTypes * Methods

        | Module of Identifier * Exposing * Statement list

        | InsideCLI of string

        /// This special leaf is only used to return data of the 'Expression' type for the operator addition function,
        /// depending on its precedence and associativity (paddOperator)
        /// In practice, it is useless, and will not be processed later
        | AddOp

    and LambdaId =

        | Wildcard

        | Arg of Identifier

    and Case =
        
        | Variable of Identifier

        | Case of Expression

        | Tuple of Case list

        | Wildcard

    and CtorParam = Case

    and Exposing = Exposing of Identifier list

    /// Allows to link one or more functions to an object type => """object methods"""
    and Methods = Methods of (Accessibility * Statement) list

    and Parameter =

        | Param of Expression

        | Wildcard

    /// There are several algebraic types, these are presented in the possible constructor types.
    /// Generally speaking, we have the sum type, the product type (tuple alias), and others that will soon be added
    and DataType =

        | AlgebraicType of Ctor list

        | Alias of Type

    /// Kinds of constructors
    and Ctor =

        | Product of Identifier * Type list

        | Term of Identifier

    and As = As of Identifier

    and Accessibility =

        | Public
        
        | Private
        
    and Literal =

        | Int of int

        | Float of float

        | String of string

        | Char of char

    /// Is the first node of the current tree
    type Program = Program of Statement list

