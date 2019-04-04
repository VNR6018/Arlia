module AST

    type Position = FParsec.Position

    type Identifier = string

    and Location = Identifier list

    /// Math.cos  -->  Qual ["Math"] "cos"
    /// or
    /// Example.Color  -->  Qual ["Example"] "Color"
    and QualName = QualName of Location * Identifier

    and Type =
    
        /// T
        | Typename of Identifier

        /// (T1, T2, ...)
        | Tuple of Type list

        /// ()
        | TerminalObject

        /// T1 -> T2
        | AppType of Type * Type

        /// a
        | VariableType of Identifier

        /// T a
        | TypeConstructor of Type (*Identifier*) * Type

        /// (x: T)
        | Flag of Identifier * Type

        /// !x --> get the type of an object
        | Exclaimer of Expression

        /// { v<: T> | PREDICATE1 | PREDICATE2 | ... }
        | RefinedType of Identifier * Type * Predicate list

        /// func x
        | DependentType of Identifier (*QualName*) * Expression list

        /// { x: T1, y: T2, z: T3, ... }
        | RecordType of (Identifier * Type) list

        /// Type <=> represent all types
        | Type

        /// Effect => represents an effect, and its result data type
        | Effect of Type

        /// [T]
        | List of Type

        /// When the type is not specified, it will be inferred from the type checking
        | Infered

    and VariableTypes = VariableTypes of Identifier list

    and Predicate =

        | Predicate of Expression

        | Requirement of Expression list

    and VariablesTypes = VariablesTypes of (Identifier * Type) list

    and Expression =

        | Let of Identifier * Expression

        /// This special leaf is only used to return data of the 'Expression' type for the operator addition function,
        /// depending on its precedence and associativity (paddOperator)
        /// In practice, it is useless, and will not be processed later
        | AddOp

        | Lambda of LambdaId * Expression

        | Signature of Identifier * Type

        | Match of Expression list * (Case list * Expression) List

        /// pattern matching as function(s)
        | EquationalMatching of (Identifier * Case list * Expression) list

        | Literal of Literal

        | CtorCall of Identifier * CtorParam list

        | HandlerCall of Identifier * CtorParam list

        | Record of (Identifier * Expression option) list

        | IfElifsElse of
            (Expression * Expression) *        // if
            (Expression * Expression) list *   // elifs
            (Expression)                       // else

        | IfElifs of
            (Expression * Expression) *        // if
            (Expression * Expression) list     // elifs

        | Tuple of Expression list

        /// Empty tuple => ()
        | ZeroObject

        | TupleMatch of Case list

        | List of Expression list

        | App of Expression * Expression

        | Var of QualName

        /// (<Expr>).identifier
        | ObjMethodCall of Expression * QualName

        /// Represents a reference of a defined object within its own implementation.
        /// In traditional object oriented languages it is `this`, in Arlia we have opted for `it`.
        | ObjectReference

        /// A hole allows to receive information on a specific part of the code,
        /// without it being complete
        | Hole of Identifier

        /// An exclaimer allow to get the type (as Type) of the expression
        //  Ex: `!x` or `!(3 + 2)` or `!(true, false)`
        | Exclaimer of Expression

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

        | NewType of Identifier * VariablesTypes * DataType * As option * Methods

        | NewEffect of Identifier * VariablesTypes * Methods

        | UseEffect of QualName * Type list * Expression

        | HandleEffect of Expression * (Case * Expression) List

        | FirstClassType of Type

        | Module of Identifier * Exposing * Expression list

        | Infix of Expression * string * Expression

        | InsideCLI of string

    and LambdaId =

        | Wildcard

        | Arg of Identifier

    and Case =
        
        | Variable of Identifier

        | Case of Expression

        | Wildcard

    and CtorParam = Case

    and Exposing = Exposing of Identifier list

    /// Allows to link one or more functions to an object type => """object methods"""
    and Methods = Methods of (Accessibility * Expression) list

    and Parameter =

        | Param of Expression

        /// Represents the next token: _, applicable for the parameters of any functions / constructors
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

        /// Public member, but requiring an instantiation of the object
        | Public
        
        /// Private member, accessible only within the object
        | Private
        
    and Literal =

        | Int of int

        | Float of float

        | Bool of bool

        | String of string

        | Char of char

    /// Is the first node of the current tree
    type Program = Program of Expression list

