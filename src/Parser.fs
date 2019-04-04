module Parser
    
    #nowarn "40"

    open FParsec
    open IndentParser
    open AST

    open System.Linq

    /// `pstr'u str` parse the `str` and return `str`.
    /// Has a similar comportement to the native `pstring` function of fparsec
    let inline private pstr's s = stringReturn s s
    
    /// `pstr'u str` parse the `str` and return `()` => unit value.
    /// Has a similar comportement to the native `pstring` function of fparsec
    let inline private pstr'u s = stringReturn s ()

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
        <?> word

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
        [ "->"; "<-"; "::"; "|"; "?"; "!"; "="; "@" ]
        
    let private reservedIdentifiers =
        [ "import"; "include"; "module"; "if"; "elif";
          "else"; "extern"; "match"; "with"; "then";
          "exposing"; "require"; "it"; "public"; "from";
          "private"; "effect"; "handler"; "handle";
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
                if reservedIdentifiers |> List.contains id
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

    and private qualIdentifier lastIdPredicate =
        (sepBy1 (simpleIdentifierP idP_nospecified (* <|> ...*)) (pstr'u "."))
        |>> fun lst -> QualName(lst |> List.rev |> List.tail |> List.rev, lst.Last())

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

    let addInfixOperator str prec assoc (toRet: Expression) =
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
    let private number<'u> : IndentParser<Literal, 'u> =
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
    
    let private expression, expressionImpl = createParserForwardedToRef ()

    let rec private typename = simpleIdentifierP idP_capitalized |>> Type.Typename

    and private typeType = keyword "Type" >>% Type.Type

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

    and private exclaimerType = pstr'u "!" >>? expression |>> Type.Exclaimer

    and private dependentType =
        parse { let! pos = getPosition
                let! id = exact pos (simpleIdentifierP (fun s -> System.Char.IsLower s.[0] && s.Length > 1))
                let! args = greater pos dependentTypeFunArgs
                return Type.DependentType(id, args) }

    and private varType =
        simpleIdentifierP (fun s -> System.Char.IsLower s.[0] && s.Length = 1)
        |>> Type.VariableType

    and private dependentTypeFunArgs = sepEndBy1 value ws1

    and private typeConstructor =
        parse { let! pos = getPosition
                let! id = exact pos (simpleIdentifierP idP_nospecified)
                let! cstr = greater pos tCtor
                if System.Char.IsLower id.[0] && id.Length > 1
                then fail "Hesitation between variable type and dependent type,\n\t
                           the name of a dependent function must therefore be more than one character "
                else return Type.TypeConstructor((if System.Char.IsUpper id.[0]
                                                  then Typename id
                                                  else VariableType id), cstr) }

    and private tCtor =
        attempt allowedArrowTypes <|>
        (betweenParentheses arrowType "")

    and private tupleType =
        parse { let! pos = getPosition
                let! types = tupleType' pos
                return types }

    and private listType =
        parse { let! pos = getPosition
                do! exact pos (pstr'u "[")
                let! ty = greater pos type'
                do! atLeast pos (pstr'u "]")
                return Type.List ty }

    and private tupleType' pos =
        attempt (exact pos (betweenParentheses (sepBy (greater pos type') (pstr'u ",")) "type"))
        |>> Type.Tuple <|> ((betweenParentheses ws "terminal object") >>% Type.TerminalObject)

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
        attempt
            (between (ws >>? pstr'u "{") (ws >>? pstr'u "}")
                (sepBy1 preq (ws >>? pstr'u ",")) "method(s) required")
        <|> ((between (pstr'u "{") (pstr'u "}") preq "method required" <|> preq)
            |>> fun req -> [ req ])

    and private refinedAllowedExpression = expression // to affine

    and private recordType =
        parse { let! pos = getPosition
                do!  exact pos (pstr'u "{") .>>? ws
                let! record =
                    atLeast pos
                        (sepBy1 (atLeast pos (pipe2
                            (ws >>? (simpleIdentifierP idP_lowered))
                            (ws >>? pstr'u ":" >>? ws >>? type')
                        (fun id ty ->  (id, ty)))) (pstr'u ","))
                do! ws >>? greater pos (pstr'u "}")
                return Type.RecordType record }

    and private arrowType =
        attempt (chainr1 allowedArrowTypes
                (pstr'u "->" >>% fun t1 t2 -> Type.AppType(t1, t2))) <|>
        typename

    and private allowedArrowTypes =
        bws (attempt typeConstructor <|>
             attempt varType <|>
             attempt dependentType <|>
             attempt typename <|>
             attempt typeType <|>
             attempt effectType <|>
             attempt flagType <|>
             attempt exclaimerType <|>
             attempt betweenParenthesesType <|>
             attempt tupleType <|>
             attempt listType <|>
             attempt recordType <|>
             refinedType)

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
                          <?> "expression between parentheses"
                return ex }

    and private tuple =
        parse { let! pos = getPosition <?> "tuple"
                do! exact pos (pstr'u "(")
                let! value' = greater pos (sepBy expression (pstr'u ","))
                do! atLeast pos (pstr'u ")")
                if value' = [] then
                     return Expression.ZeroObject
                else return Expression.Tuple value' }

    and private list =
        parse { let! pos = getPosition <?> "list"
                do! exact pos (pstr'u "[")
                let! value' = greater pos (sepBy expression (pstr'u ","))
                do! atLeast pos (pstr'u "]")
                return Expression.List value' }
         
    and private var =
        attempt (betweenParentheses operator ""
                |>> fun op -> QualName([], op)) <|>
        (qualIdentifier idP_lowered)
        |>> fun qid -> Var qid

    and private objMethodCall =
        pipe3
            (betweenParenthesesExpression)
            (pstr'u ".")
            (qualIdentifier idP_lowered)
            (fun ex _ ms -> ObjMethodCall(ex, ms))

    and private app =
        chainl1 value' (ws1 >>% fun e1 e2 -> App(e1, e2)) // The `ws1` allows application spacing 

    and private module' =
        parse { let! pos = getPosition <?> "module"
                do! exact pos (keyword "module")
                let! name = atLeast pos (simpleIdentifierP idP_capitalized)
                let! exposing = exposing pos
                let! body = greater pos document
                return Expression.Module(name, exposing, body) }

    and private exposing pos =
        attempt (greater pos (ws >>? keyword "exposing" >>? ws >>?
            greater pos (betweenParentheses (sepBy
                (bws ((simpleIdentifierP idP_nospecified) <|>
                      (betweenParentheses operator "")))
                (pstr'u ","))
            "to expose" |>> Exposing))) <|>
        (ws >>% Exposing [])

    and private record =
        parse { let! pos = getPosition <?> "record value"
                do! exact pos (pstr'u "{") .>>? ws
                let! record = recordValue pos
                do! ws >>? greater pos (pstr'u "}")
                return Expression.Record record }

    and private recordValue pos =
        atLeast pos
            (sepBy1 (atLeast pos recordItemValue) (pstr'u ","))

    and private recordItemValue =
        attempt (pipe2
            (ws >>? (simpleIdentifierP idP_lowered))
            (ws >>? (pstr'u "=" >>? ws >>? expression))
            (fun id ex -> (id, Some ex))) <|>
        (ws >>? (simpleIdentifierP idP_lowered) |>> fun id -> (id, None))

    and private lambda1 =
        parse { let! pos = getPosition <?> "lambda function"
                do! exact pos (pstr'u "\\")
                let! arg = greater pos lambdaArg
                do! greater pos (pstr'u "->")
                let! expr = greater pos expression
                return Expression.Lambda(arg, expr) }

    and private lambda2 =
        parse { let! pos = getPosition <?> "lambda function"
                do! exact pos (pstr'u "λ")
                let! arg = greater pos lambdaArg
                do! pstr'u "."
                let! expr = greater pos expression
                return Expression.Lambda(arg, expr) }

    and private lambda1'sugar =
        parse { let! pos = getPosition <?> "lambda function"
                do! exact pos (pstr'u "\\")
                let! args = suiteOf1 lambdaArg
                do! greater pos (pstr'u "->")
                let! expr = greater pos expression
                return makeLambdas args expr }

    and private lambdaArg =
        attempt (simpleIdentifierP idP_lowered) |>> LambdaId.Arg <|>
                (pstr'u "_" >>% LambdaId.Wildcard)

    and private lambda =
        attempt lambda1'sugar <|>
        attempt lambda1       <|>
                lambda2

    and private ifElifsElse =
        parse { let! pos = getPosition <?> "flow structure"
                do! exact pos (keyword "if")
                let! if' = greater pos expression
                do! atLeast pos (keyword "then")
                let! then' = greater pos expression
                let! elifs = attempt (blockOf elif') <|> (ws >>% [])
                do! atLeast pos (keyword "else")
                let! else' = greater pos expression
                return Expression.IfElifsElse((if', then'), elifs, else') }

    and private ifElifs =
        parse { let! pos = getPosition <?> ""
                do! exact pos (keyword "if")
                let! if' = greater pos expression
                do! atLeast pos (keyword "then")
                let! then' = greater pos expression
                let! elifs = attempt (blockOf elif') <|> (ws >>% [])
                return Expression.IfElifs((if', then'), elifs) }

    and private elif' = 
        parse { let! pos = getPosition <?> ""
                do! exact pos (keyword "elif")
                let! elif' = greater pos expression
                do! atLeast pos (keyword "then")
                let! then' = greater pos expression
                return (elif', then') }

    and private if' =
        parse { let! pos = getPosition
                do! exact pos (pstr'u "if")
                let! conds = blockOf if'cond
                return Expression.IfElifs(conds.Head, remove 0 conds) }

    and private if'cond =
        pipe2
            (ws >>? pstr'u "|" >>? expression)
            (ws >>? pstr'u "->" >>? expression)
            (fun cond then' -> (cond, then'))

    and private match' =
        parse { let! pos = getPosition <?> ""
                do! exact pos (keyword "match")
                let! toMatch = greater pos (sepBy expression (pstr'u ","))
                let nbrOfMatched = toMatch.Length
                do! atLeast pos (keyword "with")
                let! cases = blockOf (match'pattern nbrOfMatched)
                return Expression.Match(toMatch, cases) }

    and private match'pattern nbrOfMatched =
        pipe2
            (attempt
                (pstr'u "|") <|>
                (ws >>% ())  >>?
                (sepBy (bws matchingCaseExtendedToMatchKword) (pstr'u ",")))
            (pstr'u "->" >>? expression)
            (fun case expr -> (case, expr))
        >>= fun case ->

                // Corresponds to elements separated by a comma that correspond to the cases of
                // the respective elements to be matched
                let nbrOfMatcher = match case with (matchers, _) -> matchers

                if nbrOfMatcher.Length <> nbrOfMatched then
                    fail (sprintf "There are %d elements to match, but some case(s) has %d"
                            nbrOfMatched
                            nbrOfMatcher.Length)
                else preturn case

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
                let! id = exact pos (pstr's !eqid)
                let! cases = greater pos (suiteOf1 matchingCase)
                do! ws >>? atLeast pos (pstr'u "=")
                let! value = greater pos expression
                return (id, cases, value) }

    and private equationalMatching =
        attempt (pipe2
                    (equationalMatching')
                    (blockOf equationalMatching'')
                    (fun x xs -> x :: xs)) |>> Expression.EquationalMatching <|>
                (equationalMatching' |>> fun eq -> Expression.EquationalMatching [eq])
                    
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
        |>> Expression.EquationalMatching

    and private matchingCase =
        attempt (pstr'u "_" >>% Case.Wildcard) <|>
        attempt ((simpleIdentifierP idP_lowered) |>> Case.Variable) <|>
                (matchingCaseValue |>> Case.Case)

    and private matchingCaseExtendedToMatchKword =
        attempt ctorCall |>> Case.Case <|>
                matchingCase

    and private tupleMatch =
        parse { let! pos = getPosition
                do! exact pos (pstr'u "(")
                let! uplets = greater pos tupleMatchUplets
                do! greater pos (pstr'u ")")
                if List.isEmpty uplets
                then return ZeroObject
                else return TupleMatch uplets }

    and private tupleMatchUplets =
        sepBy
            (bws ctorCallParameter)
            (pstr'u ",")

    and private matchingCaseValue =
        attempt betweenParenthesesExpression <|>
        attempt literal <|>
        attempt tupleMatch <|>
        attempt tuple <|>
        attempt record <|>
        attempt firstClassType <|>
        //attempt (((simpleIdentifierP idP_capitalized)
        //        |>> fun id -> CtorCall(id, []))) <|>
        attempt ctorCall <|>
        (betweenParentheses ctorCall "")

    and private newEffect =
        parse { let! pos = getPosition <?> "new effect"
                do! exact pos (keyword "effect")
                let! id = atLeast pos (attempt (simpleIdentifierP idP_capitalized) <|> pstr's "it")
                let! varTypes = greater pos (suiteOf variableType)
                do! greater pos (pstr'u "=") // do with 'with'
                let! functions = newTypeMethods pos
                return Expression.NewEffect(id, VariablesTypes varTypes, Methods.Methods functions) }

    // Variable types with handler ?
    and private handler =
        parse { let! pos = getPosition <?> "handler"
                do! exact pos (keyword "handler")
                let! id = greater pos (simpleIdentifierP idP_capitalized)
                let! tys = greater pos (suiteOf variableType) |>> VariablesTypes
                do! atLeast pos (pstr'u "=")
                let! ty = greater pos type'
                return Expression.Handler(id, tys, ty) }

    and private useEffect =
        parse { let! pos = getPosition <?> "use effect"
                do! exact pos (keyword "use")
                let! id = greater pos (qualIdentifier idP_nospecified)
                let! tys = greater pos (suiteOf type')
                do! atLeast pos (keyword "with")
                let! usingEffectValue = ws >>? expression
                return Expression.UseEffect(id, tys, usingEffectValue) }

    and private handleEffect =
        parse { let! pos = getPosition <?> "handle effect"
                do! exact pos (keyword "handle")
                let! effectExprToHandle = greater pos expression
                do! atLeast pos (keyword "with")
                let! cases = blockOf effectHandlerPattern
                return Expression.HandleEffect(effectExprToHandle, cases) }

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
                return Expression.Class(id, vargs, functions) }

    and private newType =
        parse { let! pos = getPosition <?> "new type"
                let! id = exact pos (simpleIdentifierP idP_capitalized)
                let! varTypes = greater pos (suiteOf variableType)
                do! greater pos (pstr'u "=")
                let! dataType = atLeast pos dataType
                let! as' = atLeast pos newTypeAs
                let! methods = atLeast pos (newTypeMembers pos)
                return Expression.NewType(id, VariablesTypes varTypes, dataType, as', Methods.Methods methods) }

    and private newTypeAs =
        attempt (ws >>? keyword "as" >>?
                    ws >>? (simpleIdentifierP idP_capitalized)
                    |>> fun x -> Some (As x)) <|>
                (ws >>% None)

    and private newTypeMembers pos =
        attempt (ws >>? keyword "with" >>? newTypeMethods pos) <|>
                (ws >>% [])

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
        attempt varType <|>
        attempt dependentType <|>
        attempt typename <|>
        attempt typeConstructor <|>
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
        attempt infixLet' <|>
        attempt paddOperator <|>
        attempt signature <|>
        attempt let' <|>
        attempt let'sugar <|>
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

    and private ctorCall'' = 
        parse { let! pos = getPosition <?> ""
                let! ctor = exact pos (simpleIdentifierP idP_capitalized)
                let! parameters = greater pos (suiteOf ctorCallParameter)
                return CtorCall(ctor, parameters) }

    and private ctorCall' =
        ((simpleIdentifierP idP_capitalized)
        |>> fun id -> CtorCall(id, []))

    and private ctorCall =

        attempt ctorCall'' <|>
                ctorCall'

    and private ctorCallParameter =
        attempt (pstr'u "_" >>% CtorParam.Wildcard) <|>
        attempt ((simpleIdentifierP idP_lowered) |>> CtorParam.Variable) <|>
                (ctorCallParameterValue |>> CtorParam.Case)

    and private signature'function =
        parse { let! pos = getPosition
                let! id = exact pos (simpleIdentifierP idP_lowered)
                do! atLeast pos (pstr'u "::")
                let! ty = greater pos type'
                return Expression.Signature(id, ty) }

    and private signature'operator =
        parse { let! pos = getPosition
                let! id = exact pos (betweenParentheses operator "")
                do! atLeast pos (pstr'u "::")
                let! ty = greater pos type'
                return returnByAddingOp id (Expression.Signature(id, ty)) }

    and private signature =
        (attempt signature'function <|>
                 signature'operator)
        <?> "signature"

    and private let'function =
        parse { let! pos = getPosition
                let! id = exact pos (simpleIdentifierP idP_lowered)
                do! greater pos (pstr'u "=")
                let! value = greater pos expression
                return Let(id, value) }

    and private let'operator =
        parse { let! pos = getPosition
                let! op = exact pos (betweenParentheses operator "")
                do! greater pos (pstr'u "=")
                let! value = greater pos expression
                return returnByAddingOp op (Let(op, value))}

    and private let' =
        (attempt let'function <|>
                 let'operator)
        <?> "function"

    and private infixLet' =
        parse { let! pos = getPosition
                let! larg = exact pos (simpleIdentifierP idP_lowered) |>> Arg
                let! op = greater pos operator
                let! rarg = greater pos (simpleIdentifierP idP_lowered) |>> Arg
                do! atLeast pos (pstr'u "=")
                let! value = greater pos expression
                return returnByAddingOp op (Let(op, makeLambda larg (makeLambda rarg value))) }

    and private let'sugar'function =
        parse { let! pos = getPosition
                let! id = exact pos (simpleIdentifierP idP_lowered)
                let! args = greater pos (suiteOf1 ((simpleIdentifierP idP_lowered) |>> Arg))
                do! greater pos (pstr'u "=")
                let! value = greater pos expression
                return Let(id, makeLambdas args value) }

    and private let'sugar'operator =
        parse { let! pos = getPosition
                let! op = exact pos operator
                let! args = greater pos (suiteOf1 ((simpleIdentifierP idP_lowered) |>> Arg))
                do! greater pos (pstr'u "=")
                let! value = greater pos expression
                return returnByAddingOp op (Let(op, makeLambdas args value)) }

    and private let'sugar =
        (attempt let'sugar'operator <|>
                 let'sugar'function)
        <?> "function"


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
                return Expression.Infix(factor, "*", product) }
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

    /// Used outside an expression, the output of a condition will not impact the program if it is not managed
    and private topLevelCondition =
        attempt ifElifsElse <|>
        attempt ifElifs <|>
                if'

    /// Used as an expression, a condition must be able to give a result according to at least two possibilities:
    /// true or false, and not just one of them
    and private exprCondition = ifElifsElse

    and private objectReference =
        keyword "it" <?> "object reference"
        >>% ObjectReference

    and private hole =
        pstr'u "?" >>? (simpleIdentifierP idP_lowered) <?> "hole"
        |>> Expression.Hole 

    and private exclaimer =
        pstr'u "!" >>? expression <?> "exclaimer"
        |>> Expression.Exclaimer

    and private external =
        parse { let! pos = getPosition
                do! exact pos (keyword "external")
                let! name = greater pos (qualIdentifier idP_lowered)
                do! atLeast pos (pstr'u "::")
                let! ty = greater pos type'
                return Expression.External(name, ty) }

    and private include =
        parse { let! pos = getPosition
                do! exact pos (keyword "include")
                let! file = greater pos includeFile
                do! atLeast pos (keyword "as")
                let! as' = greater pos (simpleIdentifierP idP_capitalized)
                return Expression.Include(file, as') }

    and private includeFile =
        between
            (pstr'u "\"")
            (pstr'u "\"")
            (manyChars (satisfy (fun c -> c <> '\\' && c <> '"')))
            ("file name")

    and private import =
        parse { let! pos = getPosition
                do! exact pos (keyword "import")
                let! file = greater pos furi
                return Expression.Import (file + ".arl") }

    and private furi : Parser<string, IndentState<Unit>> =
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
                do! exact pos (keyword "using")
                let! modules = _use'modules pos
                return Expression.Using modules }
    and private _use'modules pos =
        sepBy1 (greater pos (qualIdentifier idP_capitalized)) (pstr'u ",")

    and private firstClassType =
            bws (pstr'u "@" >>? (
                attempt (bws arrowType) <|>
                attempt typeConstructor <|>
                attempt typeType <|>
                attempt flagType <|>
                attempt dependentType <|>
                attempt exclaimerType <|>
                attempt betweenParenthesesType <|>
                attempt tupleType <|>
                attempt recordType <|>
                refinedType))
            |>> FirstClassType
            <?> "first class type"

    /// Any expression that can be used as a value
    and private value' =
        ws >>? (attempt objMethodCall <|>
                attempt betweenParenthesesExpression <|>
                attempt implicitMulExp <|>
                attempt implicitMul <|>
                attempt implicitExp <|>
                attempt ctorCall <|>
                attempt var <|>
                attempt literal <|>
                attempt tuple <|>
                attempt list <|>
                attempt hole <|>
                attempt exclaimer <|>
                attempt record <|>
                attempt exprCondition <|>
                attempt match' <|>
                attempt lambda <|>
                attempt doNotation <|>
                attempt useEffect <|>
                attempt handleEffect <|>
                attempt objectReference <|>
                attempt firstClassType)

    and private statement =
        bws (*ws >>?*)
               (attempt import <|>
                attempt include <|>
                attempt using <|>
                attempt external <|>
                attempt newEffect <|>
                attempt handler <|>
                attempt class' <|>
                attempt infixLet' <|>
                attempt signature <|>
                attempt paddOperator <|>
                attempt let' <|>
                attempt let'sugar <|>
                attempt equationalMatching <|>
                attempt equationalOperatorMatching <|>
                attempt topLevelCondition <|>
                attempt newType <|>
                module')
        <?> "statement"

    and private document =
        bws (attempt (blockOf (attempt statement <|>
                      expression)) <|> (ws >>% []))

    and private value =

        attempt opp.ExpressionParser

    expressionImpl.Value <-
        bws value <?> "expression"

    opp.TermParser <-
        attempt (bws app) <|>
        (value' .>>? ws)

    let private program =
        document |>> Program

// -------------------------------------------------------------------------------

    let parse code =

        let parser = program .>>? (ws >>. eof)

        match runParser (bws parser) () code with
        | Success(result, _, _) -> result
        | Failure(msg, _, _) ->
            printfn "%s" msg
            Program []

