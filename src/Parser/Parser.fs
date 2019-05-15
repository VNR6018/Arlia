module Parser
    
    #nowarn "40"

    open FParsec
    open IndentParser
    open AST

    open System.Linq

    /// `pstr'u str` parse the `str` and return `str`.
    /// Has a similar comportement to the native `pstring` function of fparsec
    let inline private pstr's s = stringReturn s s <?> sprintf "`%s`" s
    
    /// `pstr'u str` parse the `str` and return `()` => unit value.
    /// Has a similar comportement to the native `pstring` function of fparsec
    let inline private pstr'u s = stringReturn s () <?> sprintf "`%s`" s

    /// between popen pclose p label` parse `p` which must be between `popen` and `pclose`.
    /// `label` is what must appear if there is an error in the message  
    let inline private between popen pclose p label =
        popen >>? (p <?> label) .>>? pclose

    /// Parse line comment
    let private lineComment =
        pstr'u "--" >>. skipRestOfLine true <?> ""

    /// Parse plain comments
    let private plainComment =
        pstr'u "`"  >>. skipCharsTillString "`" true (System.Int32.MaxValue) <?> ""

    let private justWhitespace =
        skipMany (pchar ' ' <|> pchar '\t') <?> ""

    let private justWhitespace1 =
        skipMany1 (pchar ' ' <|> pchar '\t' <?> "space") <?> "space(s)"

    let private ws =
        justWhitespace >>? many (justWhitespace >>? plainComment >>? justWhitespace)
        >>? (attempt lineComment <|> justWhitespace)
    
    let private ws1 =
        justWhitespace1 >>? many (justWhitespace >>? plainComment >>? justWhitespace)

    /// Define a parser between white spaces
    let private bws p =
        let spaces = spaces >>? many (spaces >>? plainComment >>? spaces)
        spaces >>? p .>>? spaces
        
    /// Define a keyword
    let inline private keyword word =
        pstr'u word >>? nextCharSatisfiesNot
            (fun c -> isLetter c)
        <?> sprintf "`%s`" word

    /// Parse a suite of parser espacing by one space or more
    let rec private suiteOf p =
        attempt (sepEndBy p ws1) .>>? newline <|>
        (sepEndBy p ws1)

    /// Parse a suite of parser espacing by one space or more | must contain one item at least
    let private suiteOf1 p =
        attempt (sepEndBy1 p ws1) .>>? newline <|>
        (sepEndBy p ws1)

    let rec public remove i l =
        match i, l with
        | 0, x :: xs -> xs
        | i, x :: xs -> x :: remove (i - 1) xs
        | i, [] -> failwith "index out of range"

    let public fst3 (x, _, _) = x
    let public snd3 (_, y, _) = y
    let public thr3 (_, _, z) = z

    /// All allowed symbols for custom operator
    let private allowedSymbols =
        [ '!'; '@'; '&'; '£'; '%'; '^'; '.';
          '§'; '*'; '°'; '$'; '~'; ':'; '-';
          '+'; '='; '?'; '/'; '>'; '<'; '|'; ]

    let private reservedSymbols =
        [ "->"; "<-"; ":"; "|"; "?"; "!"; "="; "@" ]
        
    let private reservedIdentifiers =
        [ "import"; "include"; "module"; "if"; "elif";
          "else"; "extern"; "match"; "with"; "then"; "in";
          "exposing"; "require"; "it"; "public"; "from";
          "private"; "effect"; "handler"; "handle"; "let";
          "do"; "as"; "Type"; "class"; "use"; "using";
          "λ"; "Effect"; "infixl"; "infixr"; "infixn" ]

    /// Parse `p` between parentheses
    let inline private betweenParentheses p label =
        between (pstr'u "(") (pstr'u ")") p (label + " between parentheses")

    let inline public makeLambda arg value = Lambda (arg, value)

    let public makeLambdas args value =
        let rec tex n =
            Lambda((remove 0 args).Reverse().ElementAt(n - 1),
                if n <> 1 then tex (n - 1)
                else value)
        if args.Length = 1
        then makeLambda args.[0] value
        else makeLambda (args.[0]) (tex (args.Length - 1))

// Identifiers ------------------------------------------------------------------------------------------
//

    let rec private simpleIdentifierP predicate : Parser<string, IndentState<Unit>> =
        many1Satisfy
            (fun c -> isLetter c || isDigit c || c = ''') >>=
        fun id ->
            if isDigit id.[0]
            then fail "An identifier cannot start with a number"
            else
                if id.Length = 3 && id.[0] = ''' && id.[2] = '''
                then fail "An identifier cannot start with a character"
                else if reservedIdentifiers |> List.contains id
                     then fail "Reserved identifier!"
                     else
                         if predicate id
                         then preturn id
                         else fail "Bad identifier!"

    /// Returns "true" if "id" starts with a capital letter
    and private idP_capitalized (id: string) = System.Char.IsUpper id.[0]

    /// Returns "true" if "id" starts with a lowercase letter
    and private idP_lowered (id: string) = System.Char.IsLower id.[0]

    /// Do no check
    and private idP_nospecified (_: string) = true

    and public qualIdentifier =
        (sepBy1 (simpleIdentifierP idP_nospecified <|> betweenParentheses operator "") (pstr'u "."))
        |>> fun lst -> QualName(lst |> List.rev |> List.tail |> List.rev, lst.Last())

    // Désucrage ! --> Var / CtorCall ==> Var dans AST et Var / Ctor dans DAST

// Operators ---------------------------------------------------------------------
//

    /// Used to define a new operator
    and private newOpIdentifier =
        many1Satisfy
            (fun c -> allowedSymbols |> List.contains c) >>=
        fun op -> if reservedSymbols |> List.contains op
                  then fail "Reserved operator"
                  else preturn op
    
    /// Used to use an operator
    and private operator = newOpIdentifier

    type private Fixity =
        | Infixl    // left
        | Infixr    // right
        | InfixN    // neutral
        | Prefix    // prefixed

    let private fixity : Parser<Fixity, IndentState<Unit>> =    // TODO
        choice [ keyword "infixl" >>% Infixl ;
                 keyword "infixr" >>% Infixr ;
                 keyword "infixn" >>% InfixN ; ]
        //       keyword "prefix" >>% Prefix ]

    type Assoc = Associativity

    let opp = new OperatorPrecedenceParser<_, _, _>()

    let adjustPosition offset (pos: Position) =
        Position(pos.StreamName, pos.Index + int64 offset,
                pos.Line, pos.Column + int64 offset)

    let addInfixOperator str prec assoc toRet =
        let op = InfixOperator(str, getPosition .>>? ws, prec, assoc, (),
                            fun opPos lvalue rvalue ->
                                Infix(lvalue, str, rvalue))
        opp.AddOperator op
        toRet

    // Adding basic operators

    addInfixOperator "||" 2 Assoc.Right AddOp |> ignore   // logial OR
    addInfixOperator "&&" 3 Assoc.Right AddOp |> ignore   // logical AND
    addInfixOperator "==" 4 Assoc.None  AddOp |> ignore   // logial EQUAL
    addInfixOperator "!=" 4 Assoc.None  AddOp |> ignore   // logial NOT
    addInfixOperator "<"  4 Assoc.None  AddOp |> ignore   // logial LESSER
    addInfixOperator ">"  4 Assoc.None  AddOp |> ignore   // logial GEEATER
    addInfixOperator "<=" 4 Assoc.None  AddOp |> ignore   // logial LESSER EQUAL
    addInfixOperator ">=" 4 Assoc.None  AddOp |> ignore   // logial GEEATER EQUAL
    addInfixOperator "+"  6 Assoc.Left  AddOp |> ignore   // addition
    addInfixOperator "-"  6 Assoc.Left  AddOp |> ignore   // subtraction
    addInfixOperator "*"  7 Assoc.Left  AddOp |> ignore   // multiplication
    addInfixOperator "/"  7 Assoc.Left  AddOp |> ignore   // division
    addInfixOperator "%"  7 Assoc.Left  AddOp |> ignore   // modulo
    addInfixOperator "**" 8 Assoc.Right AddOp |> ignore   // power
    addInfixOperator "."  9 Assoc.Right AddOp |> ignore   // composition

    let rec paddOperator =
        parse { let! pos = getPosition
                let! fixity = exact pos fixity
                let! precedence = greater pos pint32
                let! op = greater pos (attempt operator <|> betweenParentheses operator "operator")
                if isAlreadyAdded op
                then failf "Already existing operator and properties (%s)" op
                else return match fixity with
                    | Infixl -> addInfixOperator op precedence Assoc.Left  AddOp
                    | Infixr -> addInfixOperator op precedence Assoc.Right AddOp
                    | InfixN -> addInfixOperator op precedence Assoc.None  AddOp }

    and isAlreadyAdded op =
        (Seq.map (fun (o: Operator<'e, 'p, 's>) -> o.String = op) opp.Operators).Contains true

    let returnByAddingOp id toRet =
        if isAlreadyAdded id = false
        // Default operator properties ==>
        then addInfixOperator id 1 Assoc.Left toRet
        else toRet

// Literals ----------------------------------------------------------------------
//

    /// Represents all accepted formats for numbers
    let private numberFormat =
        NumberLiteralOptions.AllowBinary
        ||| NumberLiteralOptions.AllowMinusSign
        ||| NumberLiteralOptions.AllowHexadecimal
        ||| NumberLiteralOptions.AllowOctal
        ||| NumberLiteralOptions.AllowPlusSign
        ||| NumberLiteralOptions.AllowFraction

    /// Represents an integer and floating point number
    let inline private number<'u> : IndentParser<Literal, 'u> =
            (numberLiteral numberFormat "number" |>> fun nl ->
                    if nl.IsInteger then Int(int nl.String)
                    else Float(float nl.String))

    /// Represents all possible characters as character literal
    let inline private char<'u> : IndentParser<Literal, 'u> =
            ((between (pstr'u "'") (pstr'u "'")
                   (satisfy (fun c -> c <> '\'')) "char literal") |>> Char)

    /// Represents all possible characters as string literal
    let inline private string<'u> : IndentParser<Literal, 'u> =
            (let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')

             // To perform
             let inline unescape c =
                match c with
                | 'n' -> '\n'
                | 'r' -> '\r'
                | 't' -> '\t'
                | c -> c

             let escapedChar = pstr'u "\\" >>. (anyOf "\\nrt\"" |>> unescape)

             between (pstr'u "\"") (pstr'u "\"")
                (manyChars (normalChar <|> escapedChar)) "string literal" |>> String)

    let private literal =
        (number  <?> "number literal"  <|>
         string  <?> "string literal"  <|>
         char    <?> "char literal")   |>> Literal
        <?> "literal"

    /// Represents all accepted exponents (Unicode) 
    let private exponents =
        [ '⁰'; '¹'; '²'; '³'; '⁴'; '⁵'; '⁶'; '⁷'; '⁸'; '⁹' ]

    /// Allows to parse an exponent as an integer
    let private pexponent =
        many1Satisfy (fun c -> exponents |> List.contains c) >>=
            fun exp ->
                let getExpConv chr =
                        match chr with
                        | '⁰' -> "0" | '¹' -> "1"
                        | '²' -> "2" | '³' -> "3"
                        | '⁴' -> "4" | '⁵' -> "5"
                        | '⁶' -> "6" | '⁷' -> "7"
                        | '⁸' -> "8" | '⁹' -> "9"
                let rconv (s: string) =
                    int (String.concat "" (seq { for chr in s -> getExpConv chr }))
                preturn (int (rconv exp))

// Types -------------------------------------------------------------------------
//
    
    let public expression, expressionImpl = createParserForwardedToRef ()

    let rec private typeType = keyword "Type" >>% Type.Type

    and private effectType =
        parse { let! pos = getPosition
                do! exact pos (keyword "Effect")
                let! ty = greater pos type'
                return Type.Effect ty }

    and private flagType =
        parse { let! pos = getPosition
                do! exact pos (pstr'u "(")
                let! id = atLeast pos (simpleIdentifierP idP_lowered)
                let! ty = typeAssignment false
                do! atLeast pos (pstr'u ")")
                return Type.Flag(id, ty) }

    //and private dependentType =
    //    parse { let! pos = getPosition
    //            let! t = exact pos tCtor
    //            let! terms = suiteOf1 (greater pos dependentTerm)
    //            if terms.Length = 1
    //            then return Type.DependentType(t, terms.First())
    //            else return
    //                    (let mutable td = Type.DependentType(t, terms.First()) in
    //                        List.iter
    //                            (fun x ->
    //                                td <- Type.DependentType(td, x))
    //                            (terms |> List.rev |> List.dropLast |> List.rev);
    //                     td) }
    //                    //List.fold
    //                    //    (fun acc e -> Type.DependentType(acc, e))
    //                    //    (Type.DependentType(t, terms.First()))
    //                    //    terms
    //                    //  }

    //and private dependentTerm =
    //    attempt literal <|>
    //    attempt exprCondition <|>
    //    attempt var <|>
    //    attempt tuple <|>
    //    betweenParenthesesExpression

    and private dependentType =
        parse { let! pos = getPosition
                let! t = exact pos tCtor
                let! e = greater pos expression
                return DependentType(t, e) }

    and private appType =
        parse { let! pos = getPosition
                let! t1 = exact pos tCtor
                let! t2 = greater pos type'
                return AppType(t1, t2) }

    and private tCtor =
        attempt allowedArrowTypes <|>
        (betweenParentheses arrowType "")

    and private tupleType =
        parse { let! pos = getPosition
                let! types = tupleType' pos
                return types }

    and private tupleType' pos =
        attempt (exact pos (betweenParentheses (sepBy (greater pos type') (pstr'u ",")) "type"))
        |>> Type.Tuple

    and private refinedType =
        parse { do! pstr'u "{" .>>? ws
                let! id = (simpleIdentifierP idP_lowered) .>>? ws
                let! ty = typeAssignment true
                do! pstr'u "|" .>>? ws
                let! predicate = predicate'
                do! pstr'u "}" .>>? ws
                return Type.RefinedType(id, ty, predicate) }

    and private predicate =
        (refinedAllowedExpression <?> "predicate")
        |>> Predicate.Predicate

    and private predicate' =
        sepBy (attempt ws >>? require <|> predicate) (ws >>? pstr'u "|") .>>? ws

    and private require =
        parse { do! keyword "require"
                let! req = requirePredicate
                return Predicate.Requirement req }

    and private preq = bws signature

    and private requirePredicate =
        ((between (pstr'u "{") (pstr'u "}") preq "method required" <|> preq)
        |>> fun req -> req)

    and private refinedAllowedExpression = expression // to affine

    and private abstractIdentifier =
        qualIdentifier |>> Type.AbstractIdentifier

    and private arrowType =
        (chainr1 (attempt (bws dependentType) <|> allowedArrowTypes) (pstr'u "->" >>% fun t1 t2 -> Type.ArrType(t1, t2)))

    and private allowedArrowTypes =
        bws (attempt abstractIdentifier <|>
             attempt typeType <|>
             attempt effectType <|>
             attempt flagType <|>
             attempt betweenParenthesesType <|>
             attempt tupleType)
        //bws (attempt typename <|>
        //     attempt varType <|>
        //     attempt typeType <|>
        //     attempt effectType <|>
        //     attempt flagType <|>
        //     attempt betweenParenthesesType <|>
        //     attempt tupleType)
        //bws (attempt varType <|>
        //     attempt dependentType <|>
        //     attempt appType <|>
        //     attempt typename <|>
        //     attempt typeType <|>
        //     attempt effectType <|>
        //     attempt flagType <|>
        //     attempt exclaimerType <|>
        //     attempt betweenParenthesesType <|>
        //     attempt tupleType <|>
        //     attempt listType <|>
        //     attempt recordType <|>
        //     refinedType)

    and private betweenParenthesesType =
        parse { let! ty = betweenParentheses type' "type"
                return ty }

    and private type' = attempt (bws arrowType) <|> allowedArrowTypes

    and private typeAssignment allowInference =
        let assign = pstr'u ":" >>? ws >>?  type'
        if allowInference then assign <|> (ws >>% Type.Infered)
        else assign
    
// Expressions -------------------------------------------------------------------------------
//

    and private betweenParenthesesExpression =
        parse { let! ex = betweenParentheses expression "expression"
                return ex }

    and private tuple =
        parse { let! pos = getPosition
                do! exact pos (pstr'u "(" <?> "tuple")
                let! value' = greater pos (sepBy (expression <?> "up-let(s)") (pstr'u ","))
                do! atLeast pos (pstr'u ")" <?> "right parenthese or up-let(s)")
                return Expression.Tuple value' }

    and private list =
        parse { let! pos = getPosition
                do! exact pos (pstr'u "[" <?> "list")
                let! value' = greater pos (sepBy (expression <?> "item(s)") (pstr'u ","))
                do! atLeast pos (pstr'u "]" <?> "right hook or item(s)")
                return List value' }
         
    and private var =
        (attempt (betweenParentheses operator "operator"
                |>> fun op -> QualName([], op)) <|>
        qualIdentifier) <?> "variable"
        |>> fun qid -> Expression.AbstractIdentifier qid

    and private objMethodCall =
        parse { let! pos = getPosition
                do! exact pos (pstr'u "(" <?> "object member call")
                let! expr = greater pos (expression <?> "expression")
                do! greater pos (pstr'u ")" <?> "right parenthese")
                do! pstr'u "." <?> "dot"
                let! objMember = simpleIdentifierP idP_lowered <?> "object member"
                return ObjMethodCall(expr, objMember) }

    and private app =
        chainl1 (value' <?> "application") (ws1 >>% fun e1 e2 -> App(e1, e2)) // The `ws1` allows application spacing 

    and private module' =
        parse { let! pos = getPosition
                do! exact pos (keyword "module" <?> "module")
                let! name = atLeast pos (simpleIdentifierP idP_capitalized <?> "module name (capitalized)")
                let! exposing = exposing pos
                let! body = greater pos document
                return Module(name, exposing, body) }

    and private exposing pos =
        attempt (greater pos (ws >>? keyword "exposing" >>? ws >>?
            greater pos (betweenParentheses (sepBy
                (bws ((simpleIdentifierP idP_nospecified) <|>
                      (betweenParentheses operator "")))
                (pstr'u ","))
            "to expose" |>> Exposing))) <|>
        (ws >>% Exposing [])

    and private record =
        parse { let! pos = getPosition
                do! exact pos (pstr'u "{" <?> "record") .>>? ws
                let! record = recordValue pos
                do! ws >>? greater pos (pstr'u "}" <?> "right bracket or item(s)")
                return Record record }

    and private recordValue pos =
        atLeast pos
            (sepBy1 (atLeast pos recordItemValue) (pstr'u ","))

    and private recordItemValue =
        pipe2
            (ws >>? (simpleIdentifierP idP_lowered <?> "identifier"))
            (ws >>? (pstr'u "=" >>? ws >>? expression <?> "value assignment"))
            (fun id ex -> (id, ex))

    and private lambda1 =
        parse { let! pos = getPosition
                do! exact pos (pstr'u "\\" <?> "lambda function")
                let! arg = greater pos lambdaArg
                do! greater pos (pstr'u "->" <?> "value assignment")
                let! expr = greater pos (expression <?> "value")
                return Lambda(arg, expr) }

    and private lambda2 =
        parse { let! pos = getPosition
                do! exact pos (pstr'u "λ" <?> "lambda function")
                let! arg = greater pos lambdaArg
                do! (pstr'u "." .>>? ws) <?> "value assignment"
                let! expr = greater pos (expression <?> "value")
                return Lambda(arg, expr) }

    and private lambda1'sugar =
        parse { let! pos = getPosition
                do! exact pos (pstr'u "\\" <?> "lambda function")
                let! args = suiteOf1 lambdaArg
                do! greater pos (pstr'u "->")
                let! expr = greater pos (expression <?> "value")
                return makeLambdas args expr }

    and private lambdaArg =
        attempt (simpleIdentifierP idP_lowered) |>> LambdaId.Arg <|>
                (pstr'u "_" >>% LambdaId.Wildcard) <?> "argument"

    and private lambda =
        attempt lambda1'sugar <|>
        attempt lambda1       <|>
                lambda2

    and private ifElifsElse =
        parse { let! pos = getPosition
                do! exact pos (keyword "if" <?> "if-elif-else")
                let! if' = greater pos expression
                do! atLeast pos (keyword "then")
                let! then' = greater pos expression
                let! elifs = attempt (blockOf elif') <|> (ws >>% [])
                do! atLeast pos (keyword "else")
                let! else' = greater pos expression
                return IfElifsElse((if', then'), elifs, else') }

    and private elif' = 
        parse { let! pos = getPosition
                do! exact pos (keyword "elif")
                let! elif' = greater pos expression
                do! atLeast pos (keyword "then")
                let! then' = greater pos expression
                return (elif', then') }

    and private if'cond =
        pipe2
            (ws >>? pstr'u "|" >>? expression)
            (ws >>? pstr'u "->" >>? expression)
            (fun cond then' -> (cond, then'))

    and private match' =
        parse { let! pos = getPosition <?> ""
                do! exact pos (keyword "match")
                let! toMatch = greater pos expression
                do! atLeast pos (keyword "with")
                let! cases = blockOf match'pattern
                return Match(toMatch, cases) }

    and private match'pattern =
        parse { let! pos = getPosition
                do! exact pos (attempt (pstr'u "|") <|> (ws >>% ()))
                let! case = greater pos (bws matchingCaseExtendedToMatchKword)
                do! atLeast pos (pstr'u "->")
                let! value = greater pos expression
                return (case, value) }

    /// Used to check if all the identifiers of the equation match block are identical
    and private eqid = ref ""

    and private equationalMatching' =
        parse { let! pos = getPosition <?> ""
                let! id = exact pos (simpleIdentifierP idP_lowered)
                let! cases = greater pos (suiteOf1 matchingCase)
                do! ws >>? atLeast pos (pstr'u "=")
                let! value = greater pos expression
                eqid := id
                return (id, cases, value) }

    and private equationalMatching'' =
        parse { let! pos = getPosition <?> ""
                let! id = exact pos (pstr's !eqid)  // TODO
                let! cases = greater pos (suiteOf1 matchingCase)
                do! ws >>? atLeast pos (pstr'u "=")
                let! value = greater pos expression
                return (id, cases, value) }

    and private equationalMatching =
        attempt (pipe2
                    (equationalMatching')
                    (blockOf equationalMatching'')
                    (fun x xs -> x :: xs)) |>> EquationalMatching <|>
                (equationalMatching' |>> fun eq -> EquationalMatching [eq])
                    
    and private equationalOperatorMatching' =
        parse { let! pos = getPosition
                let! lcase = exact pos matchingCase
                let! op = greater pos operator
                let! rcase = greater pos matchingCase
                do! atLeast pos (pstr'u "=")
                let! value = greater pos expression
                return (op, lcase :: [rcase], value) }

    and private equationalOperatorMatching =
        blockOf equationalOperatorMatching'
        |>> EquationalMatching

    and private matchingCase =
        attempt (pstr'u "_" >>% Case.Wildcard) <|>
        attempt ((simpleIdentifierP idP_lowered) |>> Case.Variable) <|>
        attempt tupleMatch <|> (matchingCaseValue |>> Case.Case)

    and private matchingCaseExtendedToMatchKword = matchingCase

    and private tupleMatch =
        parse { let! pos = getPosition
                do! exact pos (pstr'u "(")
                let! uplets = greater pos tupleMatchUplets
                do! greater pos (pstr'u ")")
                return Case.Tuple uplets }

    and private tupleMatchUplets =
        sepBy
            (bws ctorCallParameter)
            (pstr'u ",")

    and private matchingCaseValue =
        attempt betweenParenthesesExpression <|>
        attempt literal <|>
        attempt tuple <|>
        attempt record <|>
        attempt var <|>
        (betweenParentheses var "")

    and private newEffect =
        parse { let! pos = getPosition <?> "new effect"
                do! exact pos (keyword "effect")
                let! id = atLeast pos (attempt (simpleIdentifierP idP_capitalized) <|> pstr's "it")
                let! varTypes = greater pos (suiteOf variableType)
                do! greater pos (pstr'u "=") // do with 'with'
                let! functions = newTypeMethods pos
                return NewEffect(id, VariablesTypes varTypes, Methods.Methods functions) }

    // Variable types with handler ?
    and private handler =
        parse { let! pos = getPosition <?> "handler"
                do! exact pos (keyword "handler")
                let! id = greater pos (simpleIdentifierP idP_capitalized)
                let! tys = greater pos (suiteOf variableType) |>> VariablesTypes
                do! atLeast pos (pstr'u "=")
                let! ty = greater pos type'
                return Handler(id, tys, ty) }

    and private useEffect =
        parse { let! pos = getPosition <?> "use effect"
                do! exact pos (keyword "use")
                let! id = greater pos qualIdentifier
                let! tys = greater pos (suiteOf type')
                do! atLeast pos (keyword "with")
                let! usingEffectValue = ws >>? expression
                return UseEffect(id, tys, usingEffectValue) }

    and private handleEffect =
        parse { let! pos = getPosition <?> "handle effect"
                do! exact pos (keyword "handle")
                let! effectExprToHandle = greater pos expression
                do! atLeast pos (keyword "with")
                let! cases = attempt (blockOf effectHandlerPattern) <|> (ws >>? effectHandlerPattern |>> fun e -> [e])
                return HandleEffect(effectExprToHandle, cases) }

    and private effectHandlerPattern =
        pipe3
            (ws >>? matchingCaseExtendedToHandleKword)
            (ws >>? pstr'u "->")
            (ws >>? expression)
            (fun case _ expr -> (case, expr))

    and private matchingCaseExtendedToHandleKword =
        attempt handlerCall |>> Case.Case <|>
                matchingCase

    and private handlerCall = 
        parse { let! pos = getPosition <?> ""
                let! ctor = exact pos (simpleIdentifierP idP_capitalized)
                let! parameters = greater pos (suiteOf ctorCallParameter)
                return HandlerCall(ctor, parameters) }

    and private class' =
        parse { let! pos = getPosition <?> "class"
                do! exact pos (keyword "class")
                let! id = greater pos (simpleIdentifierP idP_capitalized)
                let! vargs = greater pos (suiteOf variableType) |>> VariablesTypes
                do! atLeast pos (pstr'u "=")
                let! functions = newTypeMethods pos |>> Methods
                return Class(id, vargs, functions) }

    //and private newType =
    //    parse { let! pos = getPosition <?> "new type"
    //            let! id = exact pos (simpleIdentifierP idP_capitalized)
    //            let! varTypes = greater pos (suiteOf variableType)
    //            do! greater pos (pstr'u "=")
    //            let! dataType = atLeast pos dataType
    //            let! as' = atLeast pos newTypeAs
    //            let! methods = greater pos (newTypeMembers pos)
    //            return NewType(id, VariablesTypes varTypes, dataType, as', Methods.Methods methods) }

    //and private newTypeAs =
    //    attempt (ws >>? keyword "as" >>?
    //                ws >>? (simpleIdentifierP idP_capitalized)
    //                |>> fun x -> Some (As x)) <|>
    //            (ws >>% None)

    //and private newTypeMembers pos =
    //    attempt (ws >>? keyword "with" >>? newTypeMethods pos) <|>
    //            (ws >>% [])

    and private sumType =
        parse { let! pos = getPosition <?> "new sum type"
                let! tid = exact pos (simpleIdentifierP idP_capitalized)
                let! tvars = greater pos (suiteOf variableType)
                do! greater pos (pstr'u "=")
                let! ctors = greater pos sums
                return SumType(tid, VariablesTypes tvars, ctors) }

    and private newType =
        attempt sumType

    /// A single lower-case letter with optionally some ' represents a variable type (a, a', ...) 
    and private variableType =
        bws (attempt
            ((betweenParentheses
                (pipe2
                    (simpleIdentifierP idP_lowered)
                    (typeAssignment false)
                    (fun id ty -> (id, ty)))
                "refined variable type")) <|>
            (simpleIdentifierP idP_lowered |>> fun id -> (id, Infered)))

    and private dataType =
        attempt (sums |>> DataType.AlgebraicType) <|>
                (type' |>> DataType.Alias)

    and private sums =
        attempt (blockOf sum2) <|>
                (pipe2 sum1 (many sum2) (fun s1 s2 -> s1 :: s2))

    and private sum1 =
            (pipe2 (simpleIdentifierP idP_capitalized)
                   (ws >>? suiteOf productTypes)
                   (fun id tys ->
                        if tys.IsEmpty
                        then Ctor.Term id
                        else Ctor.Product(id, tys)))

    and private sum2 =
        pstr'u "|" >>? ws >>?
            (pipe2 (simpleIdentifierP idP_capitalized)
                   (ws >>? suiteOf productTypes)
                   (fun id tys ->
                        if tys.IsEmpty
                        then Ctor.Term id
                        else Ctor.Product(id, tys)))

    and private productTypes =
        attempt dependentType <|>
        attempt appType <|>
        attempt betweenParenthesesType <|>
        tupleType

    and private newTypeMethods pos =
        greater pos (blockOf newTypeMethod)

    and private newTypeMethod =
        pipe2
            (ws >>? accessibility)
            (ws >>? insideTypeStatement)
            (fun acc stmt -> (acc, stmt))

    and private insideTypeStatement =
        attempt using <|>
        attempt external <|>
        attempt handler <|>
        attempt infixDecl' <|>
        attempt paddOperator <|>
        attempt signature <|>
        attempt decl' <|>
        attempt decl'sugar <|>
        attempt equationalMatching <|>
        attempt equationalOperatorMatching <|>
        newEffect

    and private accessibility =
        choice
            [
                keyword "private"  >>% Accessibility.Private;
                keyword "public"   >>% Accessibility.Public ;
                ws                 >>% Accessibility.Public
            ]

    and private ctorCallParameter =
        attempt (pstr'u "_" >>% CtorParam.Wildcard) <|>
        attempt ((simpleIdentifierP idP_lowered) |>> CtorParam.Variable) <|>
                (ctorCallParameterValue |>> CtorParam.Case)

    and private signature'function =
        parse { let! pos = getPosition
                let! id = exact pos (simpleIdentifierP idP_lowered)
                do! atLeast pos (pstr'u ":")
                let! ty = greater pos type'
                return Signature(id, ty) }

    and private signature'operator =
        parse { let! pos = getPosition
                let! id = exact pos (betweenParentheses operator "")
                do! atLeast pos (pstr'u ":")
                let! ty = greater pos type'
                return returnByAddingOp id (Signature(id, ty)) }

    and private signature : Parser<Statement, IndentState<Unit>> =
        (attempt signature'function <|>
                 signature'operator)
        <?> "signature"

    and private decl'function =
        parse { let! pos = getPosition
                let! id = exact pos (simpleIdentifierP idP_lowered)
                do! greater pos (pstr'u "=")
                let! value = greater pos expression
                return Decl(id, value) }

    and private decl'operator =
        parse { let! pos = getPosition
                let! op = exact pos (betweenParentheses operator "")
                do! greater pos (pstr'u "=")
                let! value = greater pos expression
                return returnByAddingOp op (Decl(op, value))}

    and private decl' =
        (attempt decl'function <|>
                 decl'operator)
        <?> "function"

    and private infixDecl' =
        parse { let! pos = getPosition
                let! larg = exact pos arg
                let! op = greater pos operator
                let! rarg = greater pos arg
                do! atLeast pos (pstr'u "=")
                let! value = greater pos expression
                return returnByAddingOp op (Decl(op, makeLambda larg (makeLambda rarg value))) }

    and private decl'sugar'function =
        parse { let! pos = getPosition
                let! id = exact pos (simpleIdentifierP idP_lowered)
                let! args = greater pos (suiteOf1 arg)
                do! greater pos (pstr'u "=")
                let! value = greater pos expression
                return Decl(id, makeLambdas args value) }

    and private arg =
        (attempt (simpleIdentifierP idP_lowered) <|>
        (betweenParentheses (simpleIdentifierP idP_lowered) "argument"))
        |>> Arg

    and private decl'sugar'operator =
        parse { let! pos = getPosition
                let! op = exact pos operator
                let! args = greater pos (suiteOf1 arg)
                do! greater pos (pstr'u "=")
                let! value = greater pos expression
                return returnByAddingOp op (Decl(op, makeLambdas args value)) }

    and private decl'sugar =
        (attempt decl'sugar'operator <|>
                 decl'sugar'function)
        <?> "function"

    and private let'1 =
        parse { let! pos = getPosition
                do! exact pos (keyword "let")
                let! id = greater pos (simpleIdentifierP idP_lowered)
                do! greater pos (pstr'u "=")
                let! value = greater pos expression
                do! atLeast pos (attempt (keyword "in") <|> (ws >>% ()))
                let! in' = greater pos expression
                return Let(id, value, in') }

    and private let'sugar'function =
        parse { let! pos = getPosition
                do! exact pos (keyword "let")
                let! id = greater pos (simpleIdentifierP idP_lowered)
                let! args = greater pos (suiteOf1 arg)
                do! greater pos (pstr'u "=")
                let! value = greater pos expression
                do! atLeast pos (attempt (keyword "in") <|> (ws >>% ()))
                let! in' = greater pos expression
                return Let(id, makeLambdas args value, in') }

    and private let' =
        attempt let'1 <|>
                let'sugar'function 

    (*
        
        The do-notation is natively implemented in the parser, but needs to be reconsidered for several reasons
        (monads ? algebraic effects ? authorized block?); indeed, Arlia looks like Haskell at first sight, but
        they are different languages, Arlia does not include some features commonly used by Haskell at the monad level.
        It is therefore necessary to reconsider the do-notation for Arlia,
        at least in its implementation and internal functioning (monad)...
    
    *)


    and private doNotation1 =
        parse { let! pos = getPosition
                do! exact pos (keyword "do")
                let! do' = greater pos _doNotation'do
                return do' }
    and private _doNotation'do =
        attempt doSugarStatement <|> doSugarVar

    and private doNotation2 =
        parse { let! pos = getPosition
                do! exact pos (keyword "do")
                let! expr = greater pos expression
                return expr }

    and private doNotation =
        attempt doNotation1 <|>
                doNotation2

    and private doSugarVar =
        parse { let! pos = getPosition
                let! var = exact pos (simpleIdentifierP idP_lowered)
                do! ws >>? pstr'u "<-"
                let! value = _doSugarVar'value
                let! rest = _doSugarVar'rest
                return Infix(value, ">>=", Lambda(Arg var, rest)) }
    and private _doSugarVar'value =
        ws >>? attempt doSugarStatement <|> expression
    and private _doSugarVar'rest =
        ws >>? attempt doSugarStatement <|> expression

    and private doSugarStatement =
        parse { let! pos = getPosition
                let! stmt = exact pos _doSugarStatement'stmt
                let! rest = ws >>? doSugarVar
                return Infix(stmt, ">>", rest) }
    and private _doSugarStatement'stmt =
        attempt doSugarVar <|> expression

    and private implicitMul =
        parse { let! factor = number |>> Literal
                let! product = _implicitMul'product
                return Infix(factor, "*", product) }
    and private _implicitMul'product =
        attempt var <|> betweenParenthesesExpression

    and private implicitExp =
        parse { let! base' = _implicitExp'base
                let! exp = pexponent |>> fun n -> Literal (Int n)
                return Infix(base', "**", exp) }
    and private _implicitExp'base =
        attempt var <|> betweenParenthesesExpression

    and private implicitMulExp =
        parse { let! base' = implicitMul
                let! exp = pexponent |>> fun n -> Literal (Int n)
                return Infix(exp, "**", base') } // Reverse for the priority of operations 

    and private ctorCallParameterValue = matchingCaseValue // Is the same

    /// Used as an expression, a condition must be able to give a result according to at least two possibilities:
    /// true or false, and not just one of them
    and private exprCondition = ifElifsElse

    and private objectReference =
        keyword "it" <?> "object reference (`it`)"
        >>% ObjectReference

    and private hole =
        pstr'u "?" <?> "hole" >>? (simpleIdentifierP idP_lowered <?> "name of the hole")
        |>> Hole 

    and private external =
        parse { let! pos = getPosition
                do! exact pos (keyword "external" <?> "external")
                let! name = greater pos qualIdentifier <?> "name"
                do! atLeast pos (pstr'u "::")
                let! ty = greater pos (type' <?> "type")
                return External(name, ty) }

    and private include =
        parse { let! pos = getPosition
                do! exact pos (keyword "include" <?> "include")
                let! file = greater pos includeFile <?> "file to include"
                do! atLeast pos (keyword "as")
                let! as' = greater pos (simpleIdentifierP idP_capitalized <?> "file manipulation name")
                return Include(file, as') }

    and private includeFile =
        between
            (pstr'u "\"")
            (pstr'u "\"")
            (manyChars (satisfy (fun c -> c <> '\\' && c <> '"')))
            ("file name")

    and private import =
        parse { let! pos = getPosition
                do! exact pos (keyword "import" <?> "import")
                let! file = greater pos furi <?> "file path"
                return Import (file + ".arl") }

    and public furi : Parser<string, IndentState<Unit>> =
        attempt (chainl1
            (many1Satisfy (fun c ->
                isAsciiLetter c
                || isDigit c
                || c = '''
                || c = '_'))
            (pstr'u "." >>% fun x y -> sprintf "%s\\%s" x y)) <|>
        (simpleIdentifierP idP_nospecified)

    // Append: using <module> from <file>

    and private using = 
        parse { let! pos = getPosition
                do! exact pos (keyword "using" <?> "using")
                let! modules = _use'modules pos <?> "module(s)"
                return Using modules }
    and private _use'modules pos =
        sepBy1 (greater pos qualIdentifier) (pstr'u ",")

    /// Any expression that can be used as a value
    and private value' =
        ws >>? (attempt objMethodCall <|>
                attempt betweenParenthesesExpression <|>
                attempt implicitMulExp <|>
                attempt implicitMul <|>
                attempt implicitExp <|>
                attempt let' <|>
                attempt literal <|>
                attempt var <|>
                attempt tuple <|>
                attempt list <|>
                attempt hole <|>
                attempt record <|>
                attempt exprCondition <|>
                attempt match' <|>
                attempt lambda <|>
                attempt doNotation <|>
                attempt useEffect <|>
                attempt handleEffect <|>
                objectReference)

    and public statement =
        bws (*ws >>?*)
               (attempt import <|>
                attempt include <|>
                attempt using <|>
                attempt external <|>
                attempt newEffect <|>
                attempt handler <|>
                attempt class' <|>
                attempt infixDecl' <|>
                attempt signature <|>
                attempt paddOperator <|>
                attempt decl' <|>
                attempt decl'sugar <|>
                attempt equationalMatching <|>
                attempt equationalOperatorMatching <|>
                attempt newType <|>
                attempt module')
        <?> "statement"

    and private document =
        bws (attempt (blockOf statement) <|>
            (ws >>% []))

    and private value =

        attempt opp.ExpressionParser

    expressionImpl.Value <-
        bws value <?> "expression"

    opp.TermParser <-
        attempt (bws app) <|>
        (value' .>>? ws)

    let private program =
        document |>> Program

    //System.Console.Clear()
    //let res = runParser (let' .>>? eof) () @"let x = 432 in (x, x)"
    //printfn "%A" res
    //exit 0

// -------------------------------------------------------------------------------

    let parse code (filename: string option) =

        if filename.IsSome
        then printfn "parse .\\%s" (filename.Value)

        let parser = program .>>? (ws >>. eof)

        match runParser (bws parser) () code with
        | Success(result, _, _) -> result
        | Failure(msg, _, _) ->
            printfn "%s" msg
            Program []

// + http://math.andrej.com/2010/09/27/programming-with-effects-ii-introducing-eff/
// >> https://www.eff-lang.org/handlers-tutorial.pdf
// > https://www.eff-lang.org/learn/
// > http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html

// --> Définir la priorité et la précédence des opérateurs

(* Type-checking

    m a   :: Type
    a     :: Type
    m     :: Type -> Type

    m a b :: Type
    a     :: Type
    b     :: Type
    m     :: Type -> Type -> Type

*)
