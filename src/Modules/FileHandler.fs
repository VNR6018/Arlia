module FileHandler

    open AST
    open Desugger

    type ArliaFile =
        { name: string; // the name of the file (ex: main.arl)
          uri: string ; // the path of the file (ex: dir/main.arl)
          importedFiles: string list; // list of all imported file of this file (ex: [dio.arl; exception.arl])
          ast: Program } // the AST of the file
    
    type FileHandler = Unit
