module Typechecker

    type Env =
        { file: System.IO.FileInfo option
          mutable types: DataType list
          mutable functions: Function list }
         
    with member containsFunction : AST.Identifier -> bool
         member filename : string
         member getFunction : AST.Identifier -> Function
         member getDataType : AST.Identifier -> DataType
         /// Get the 'n' line in the environment file
         member atLine : int -> AST.Identifier
         /// Check if the given function has is already added
         member hasFunctionImplementation : AST.Identifier -> bool
         member getFunctionValue : AST.Identifier -> DAST.DAST_Expr
         member getFunctionType : AST.Identifier -> Type
         /// Updates the value of a function in the environment
         member updateFunctionValue : AST.Identifier -> DAST.DAST_Expr -> Env
         /// Updates the type of a function in the environment
         member updateFunctionType : AST.Identifier -> Type -> Env
         member addFunction : AST.Identifier -> Type -> DAST.DAST_Expr option -> Env
         /// Add data type as alias
         member addDataType_alias : AST.Identifier -> Type -> Env
         /// Add data type as sum type
         member addDataType_sum : AST.Identifier -> Ctor list -> Env
         
    and DataType
    and Ctor = DAST.DAST_Ctor
    and Function 
    and Type

    type Scheme = Scheme of string list * Type
    and TypeEnv = Map<AST.QualName, Scheme>
    and Subst = Map<string, Type>

    module Type =
        val toString : Type -> string

    val newEnv : System.IO.FileInfo option -> Env

    val resetId : unit -> unit

    val qualNameToString : AST.QualName -> string

    val tiExpr : TypeEnv -> DAST.DAST_Expr -> (Subst * Type)

    val typecheck_statement : Env -> DAST.DAST_Statement -> Env

    val typecheck_document : System.IO.FileInfo option -> DAST.DProgram -> Env
    