open AST;
open FunStack;
(*Note that for details about datatypes kindly look in datatype.sml and other structures*)
datatype stackuse = ITEConc of (stackuse list) * (stackuse list) * (stackuse list) * string
                    | WHConc of (stackuse list)* (stackuse list) * string
                    | String of string;

exception ASTError
(* This is the part of code for connecting ML-Lex with ML-Yacc *)
structure WhileLrVals = WhileLrValsFun(structure Token = LrParser.Token)
structure WhileLex = WhileLexFun(structure Tokens = WhileLrVals.Tokens);
structure WhileParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = WhileLrVals.ParserData
     	       structure Lex = WhileLex)
     
open AST;
(* For capturing the lex tokens we will use this function *)
fun capture(tokens) =
    	     	let fun print_error (s,pos:int,_) =
		    	( TextIO.output(TextIO.stdOut, "Syntax Error:"^Int.toString(pos)^":"^Int.toString(pos)^":"^s) ; raise ASTError )
		in
		    WhileParser.parse(0,tokens,print_error,())
		end

(* This function will be used for lexing the string *)
fun lexing(str) =
    let val done = ref false
    	val lexer=  WhileParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
(* This function will be used for parsing *)
fun parse (lexer) =
    let val dummyEOF = WhileLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = capture lexer
	val (nextToken, lexer) = WhileParser.Stream.get lexer
    in
        if WhileParser.sameToken(nextToken, dummyEOF) then (result)
 		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
		
    end

fun readFile (infile:string) =
   let 
        val instream = TextIO.openIn infile
	fun loop instream =
	    String.implode(String.explode(TextIO.inputAll instream))

    in
	    loop instream before TextIO.closeIn instream
    end

(* This is our main parser function defined using the other helper function and *)
(* inside this function (parser) we will give our input arguments of filename *)
fun parser(file)  = parse(lexing(readFile(file)));



(*Now we will implement the evaluation of the VMC machine*)

(*This function will extract the variable declaration from the ast*)
fun getVariables(s: string) = 
    let val x = parser s;
        val (PROG(_,BLK(variableList,_))) = x;
    in 
        variableList
    end
(*This function will be used to get all variables with their types and initial values*)
(*Which will be used to initialize our symbol table*)
fun addElement(x : declaration) : ((string * typ * int) list) =
    let val (DEC (variables,types)) = x;
        fun addTuple(variables,size,result) = if length(variables) = size then result
            else addTuple(variables,size + 1, result @ [(List.nth(variables,size),types,0)]);
    in
        addTuple(variables,0,[])
    end
(*This function will create the symboltable of the program of type*)
(*(string * typ * int) list*)
fun symbolTable(s: string) = 
    let val variableList = getVariables(s);
        fun append(function, lis,index) = if length(lis) = index then [] 
            else function(List.nth(lis,index)) @ append(function,lis,index+1);
        fun addIndex(lis,i,result) =if length(lis) = i then result else 
                    (let val (x,y,index) = List.nth(lis,i)
                    in addIndex(lis,i+1,result @ [(x,y,i)])
                    end )
    in 
        addIndex(append(addElement,variableList,0),0,[])
    end;

(*This function will apply the func on elements of list and give concatination*)
fun apply(func, lis) : stackuse list= let fun concat([]) = []
                        | concat(x:: y) = func(x) @ concat(y)
                    in 
                    concat(lis)
                    end;

(*This function will fetch the command sequence*)
fun fetchCommand(s: string) = 
    let 
        val x = parser s
        val (PROG(_,BLK(_,commands))) = x
    in 
        commands
    end;

 (* This will be the list of stackuse datatype*)
fun postorder(s : string) : stackuse list = 
    let val commands = fetchCommand(s)
        fun postVariable(x) = [String(x)];
        fun post(Factor(x)) = [String(x)]
            |post(GT(x,y)) = post(x) @ post(y) @ [String("GT")]
            |post(GEQ(x,y)) = post(x) @ post(y) @ [String("GEQ")]
            |post(EQ(x,y)) = post(x) @ post(y) @ [String("EQ")]
            |post(NEQ(x,y)) = post(x) @ post(y) @ [String("NEQ")]
            |post(LEQ(x,y)) = post(x) @ post(y) @ [String("LEQ")]
            |post(LT(x,y)) = post(x) @ post(y) @ [String("LT")]
            |post(NEG(x)) = post(x) @ [String("NEG")]
            |post(OR(x,y)) = post(x) @ post(y) @ [String("OR")]
            |post(AND(x,y)) = post(x) @ post(y) @ [String("AND")]
            |post(NOT(x)) = post(x) @ [String("NOT")]
            |post(PLUS(x,y)) = post(x) @ post(y) @ [String("PLUS")]
            |post(MINUS(x,y)) = post(x) @ post(y) @ [String("MINUS")]
            |post(TIMES(x,y)) = post(x) @ post(y) @ [String("TIMES")]
            |post(DIV(x,y)) = post(x) @ post(y) @ [String("DIV")]
            |post(MOD(x,y)) = post(x) @ post(y) @ [String("MOD")]

        fun postCommand(ITE(x,SEQ(y),SEQ(z))) = [ITEConc(post(x),apply(postCommand,y) @ [String "SEQ"],apply(postCommand,z) @ [String "SEQ"],"ITE")]
            (* |post(SEQ(y))  = (map post y) *)
            |postCommand(Read(x)) = postVariable(x) @ [String("READ")]
            |postCommand(Write(x)) = post(x) @ [String("WRITE")]
            |postCommand(SET(x,y)) = postVariable(x) @ post(y) @[String("SET")]
            |postCommand(WH(x,SEQ(y))) = [WHConc(post(x),apply(postCommand,y) @ [String "SEQ"],"WH")]
        fun postSeq(SEQ(y)) = apply(postCommand,y) @ [String "SEQ"];
        in
            postSeq(commands)
        end 
(* now we just have to find postfix expression and store it in stack which would be C  *)
(* after that just apply the semantic rule *)

(*First of all we will make a function to identify if a string is a identifier or not*)
fun intPart("1") = true| intPart("2") = true | intPart("3") = true | intPart("4") = true
    |intPart("5") = true|intPart("6") = true | intPart("7") = true | intPart("8") = true
    |intPart("9") = true| intPart("0") = true| intPart("+") = true| intPart("~") = true
    |intPart(x) = false
fun notIdentifier(strVal) = let fun check(strVal,i) = if size(strVal) = i then true else 
            (if intPart(str(String.sub(strVal,i))) then check(strVal,i+1) else false)
            in check(strVal,0)
            end;
fun giveBoolVal("tt") = 1| giveBoolVal("ff") = 0;
exception NOTBool
fun funcAND(a,b) = if a = 1 andalso b = 1 then 1 else 
                if (a >1 orelse a < 0 orelse b >1 orelse b < 0) then raise NOTBool else 0  ;
fun funcOR(a,b) = if a = 0 andalso b = 0 then 0 else 
                if (a >1 orelse a < 0 orelse b >1 orelse b < 0) then raise NOTBool else 1;
(*This will give the boolean result of the stack*)
fun boolresult("LT",a,b) = if a < b then "tt" else "ff"
    |boolresult("GT",a,b) = if a > b then "tt" else "ff"
    |boolresult("GEQ",a,b) = if a >= b then "tt" else "ff"
    |boolresult("LEQ",a,b) = if a <= b then "tt" else "ff"
    |boolresult("EQ",a,b) = if a = b then "tt" else "ff"
    |boolresult("NEQ",a,b) = if a <> b then "tt" else "ff"
(*Here M is our global *)
signature VMC = 
sig
    val M : int array
    val updateVariables : (string * typ * int) list -> unit
    val findSymbol : string * ((string * typ * int) list) -> int
    val findType : string * ((string * typ * int) list) -> typ
    exception IdentifierNotFound
    exception WrongType
    val postfix : string -> stackuse Stack
    val changeStack1 : (string Stack) * (stackuse Stack) * ((string * typ * int) list) -> (string Stack) * (stackuse Stack) 
    val changeStack2 : (string Stack) * (stackuse Stack) * ((string * typ * int) list) -> (string Stack) * (stackuse Stack) 
    val changeStack3 : (string Stack) * (stackuse Stack) -> (string Stack) * (stackuse Stack)
    val setting :  (string Stack) * (stackuse Stack) * ((string * typ * int) list) -> unit
    val changeStack4 : (string Stack) * (stackuse Stack) * ((string * typ * int) list) -> (string Stack) * (stackuse Stack) 
    val changeStackIF : (string Stack) * (stackuse Stack) * ((string * typ * int) list) -> (string Stack) * (stackuse Stack) 
    val changeStackWhile : (string Stack) * (stackuse Stack) * ((string * typ * int) list) -> (string Stack) * (stackuse Stack)
    val changeStackRead : (string Stack) * (stackuse Stack) * ((string * typ * int) list) -> (string Stack) * (stackuse Stack)
    val changeStackWrite : (string Stack) * (stackuse Stack) * ((string * typ * int) list) -> (string Stack) * (stackuse Stack)
    val expEval : (string Stack) * (stackuse Stack) * ((string * typ * int) list) -> string 
    val rules : (string Stack) * (stackuse Stack) * ((string * typ * int) list) -> (string Stack) * (stackuse Stack) 
    val evaluating : (string Stack) * (stackuse Stack) * ((string * typ * int) list) -> (string Stack) * (stackuse Stack) 
    val execute : string -> (string Stack) * (int array) * (stackuse Stack)
    val stackPost : stackuse list -> string list
    val toString : (string Stack) * (stackuse Stack) -> (string list) * (int array) * (string list)
end
structure Vmc :> VMC = 
struct
    (*VMC are the parts of our VMC machine*)
    val M = Array.array(101,0)
    exception IdentifierNotFound
    exception WrongType
    (*This function will initialize memory with variables initial value which is 0*)
    fun updateVariables(symbols) = 
            let fun getIndex(index) = if length(symbols) = index then
                () else (let val x = Array.update(M,index,0) in getIndex(index + 1) end)
            in getIndex(0)
            end 
    (*This function will find the identifier index in memory from the symbol table*)
    fun findSymbol(identifier, symbols) = let fun find(iden,sym,index) = if index < length(symbols) then
                    (let val (x,y,z) = List.nth(sym,index)
                    in (if iden = x then index else find(iden,sym,index + 1))
                    end)
                    else raise IdentifierNotFound
                    in find(identifier,symbols,0)
                    end
    (*This function will find the type of the identifier from the symbol table*)
    fun findType(identifier,symbols) = let fun find(iden,sym,index) = if index < length(symbols) then
                    (let val (x,y,z) = List.nth(sym,index)
                    in (if iden = x then y else find(iden,sym,index + 1))
                    end)
                    else raise IdentifierNotFound
                    in find(identifier,symbols,0)
                    end
    (*This will give us the postfix stack from filename*)
    fun postfix(filename) = let val x = rev(postorder(filename))
                            in list2Stack(x)
                            end
    (*This is the case when the top of controlStack is binary operator of intExp*)
    fun changeStack1(valueStack,controlStack,symbols) = 
                        let val x = top(controlStack)
                        fun change1(String ("PLUS")) = 
                                let val first = top(valueStack)
                                    val second = top(pop(valueStack))
                                fun operation(a,b) = if (a = "tt" orelse a = "tt" orelse b = "ff" orelse b = "tt") then
                                raise WrongType 
                                else if notIdentifier(a) andalso notIdentifier(b) 
                                then valOf(Int.fromString(a)) + valOf(Int.fromString(b))
                                else if  not(notIdentifier(a)) andalso notIdentifier(b) then
                                (if findType(a,symbols) = INT then Array.sub(M,findSymbol(a,symbols)) + valOf(Int.fromString(b))
                                else raise WrongType) else if notIdentifier(a) andalso not(notIdentifier(b)) then 
                                (if findType(b,symbols) = INT then Array.sub(M,findSymbol(b,symbols)) + valOf(Int.fromString(a))
                                else raise WrongType) else (if findType(a,symbols) = INT andalso findType(b,symbols) = INT
                                then Array.sub(M,findSymbol(a,symbols)) + Array.sub(M,findSymbol(b,symbols))
                                else raise WrongType)
                                in operation(second,first)
                                end
                            |change1(String ("MINUS")) = 
                                let val first = top(valueStack)
                                    val second = top(pop(valueStack))
                                fun operation(a,b) = if (a = "tt" orelse a = "tt" orelse b = "ff" orelse b = "tt") then
                                raise WrongType 
                                else if notIdentifier(a) andalso notIdentifier(b) 
                                then valOf(Int.fromString(a)) - valOf(Int.fromString(b))
                                else if  not(notIdentifier(a)) andalso notIdentifier(b) then
                                (if findType(a,symbols) = INT then Array.sub(M,findSymbol(a,symbols)) - valOf(Int.fromString(b))
                                else raise WrongType) else if notIdentifier(a) andalso not(notIdentifier(b)) then 
                                (if findType(b,symbols) = INT then valOf(Int.fromString(a)) - Array.sub(M,findSymbol(b,symbols)) 
                                else raise WrongType) else (if findType(a,symbols) = INT andalso findType(b,symbols) = INT
                                then Array.sub(M,findSymbol(a,symbols)) - Array.sub(M,findSymbol(b,symbols))
                                else raise WrongType)
                                in operation(second,first)
                                end
                            |change1(String ("TIMES")) = 
                                let val first = top(valueStack)
                                    val second = top(pop(valueStack))
                                fun operation(a,b) = if (a = "tt" orelse a = "tt" orelse b = "ff" orelse b = "tt") then
                                raise WrongType 
                                else if notIdentifier(a) andalso notIdentifier(b) 
                                then valOf(Int.fromString(a)) * valOf(Int.fromString(b))
                                else if  not(notIdentifier(a)) andalso notIdentifier(b) then
                                (if findType(a,symbols) = INT then Array.sub(M,findSymbol(a,symbols)) * valOf(Int.fromString(b))
                                else raise WrongType) else if notIdentifier(a) andalso not(notIdentifier(b)) then 
                                (if findType(b,symbols) = INT then valOf(Int.fromString(a)) * Array.sub(M,findSymbol(b,symbols)) 
                                else raise WrongType) else (if findType(a,symbols) = INT andalso findType(b,symbols) = INT
                                then Array.sub(M,findSymbol(a,symbols)) * Array.sub(M,findSymbol(b,symbols))
                                else raise WrongType)
                                in operation(second,first)
                                end
                            |change1(String ("DIV")) = 
                                let val first = top(valueStack)
                                    val second = top(pop(valueStack))
                                fun operation(a,b) = if (a = "tt" orelse a = "tt" orelse b = "ff" orelse b = "tt") then
                                raise WrongType 
                                else if notIdentifier(a) andalso notIdentifier(b) 
                                then valOf(Int.fromString(a)) div valOf(Int.fromString(b))
                                else if  not(notIdentifier(a)) andalso notIdentifier(b) then
                                (if findType(a,symbols) = INT then Array.sub(M,findSymbol(a,symbols)) div valOf(Int.fromString(b))
                                else raise WrongType) else if notIdentifier(a) andalso not(notIdentifier(b)) then 
                                (if findType(b,symbols) = INT then valOf(Int.fromString(a)) div Array.sub(M,findSymbol(b,symbols)) 
                                else raise WrongType) else (if findType(a,symbols) = INT andalso findType(b,symbols) = INT
                                then Array.sub(M,findSymbol(a,symbols)) div Array.sub(M,findSymbol(b,symbols))
                                else raise WrongType)
                                in operation(second,first)
                                end
                            |change1(String ("MOD")) = 
                                let val first = top(valueStack)
                                    val second = top(pop(valueStack))
                                fun operation(a,b) = if (a = "tt" orelse a = "tt" orelse b = "ff" orelse b = "tt") then
                                raise WrongType 
                                else if notIdentifier(a) andalso notIdentifier(b) 
                                then valOf(Int.fromString(a)) mod valOf(Int.fromString(b))
                                else if  not(notIdentifier(a)) andalso notIdentifier(b) then
                                (if findType(a,symbols) = INT then Array.sub(M,findSymbol(a,symbols)) mod valOf(Int.fromString(b))
                                else raise WrongType) else if notIdentifier(a) andalso not(notIdentifier(b)) then 
                                (if findType(b,symbols) = INT then valOf(Int.fromString(a)) mod Array.sub(M,findSymbol(b,symbols)) 
                                else raise WrongType) else (if findType(a,symbols) = INT andalso findType(b,symbols) = INT
                                then Array.sub(M,findSymbol(a,symbols)) mod Array.sub(M,findSymbol(b,symbols))
                                else raise WrongType)
                                in operation(second,first)
                                end
                            |change1(String ("NEG")) = 
                                let val first = top(valueStack)
                                fun operation(a) = if (a = "tt" orelse a = "ff" ) then 
                                raise WrongType else if notIdentifier(a) 
                                then  ~1 * valOf(Int.fromString(a))
                                else (if findType(a,symbols) = INT then ~1 * Array.sub(M,findSymbol(a,symbols))
                                else raise WrongType)
                                in operation(first)
                                end
                        in (if x = String ("NEG") then (push(Int.toString(change1(x)),pop(valueStack)),pop(controlStack)) else 
                                    (push(Int.toString(change1(x)),pop(pop(valueStack))),pop(controlStack)))
                        end  
    (*This is the case when the top of controlStack is binary operator of comparisons*)
    (*The output is bool of the inner change1 function*)
    fun changeStack2(valueStack,controlStack,symbols) = 
                        let val x = top(controlStack)
                            fun change1(String ("LT")) = 
                                let val first = top(valueStack)
                                    val second = top(pop(valueStack))
                                fun operation(a,b) = if (a = "tt"  andalso b = "ff") then
                                "ff"  else if (a = "tt" andalso b = "tt") then
                                "ff" else if (a = "ff" andalso b = "ff") then
                                "ff" else if (a = "ff" andalso b = "tt") then
                                "tt" else if not(notIdentifier(a)) andalso (a <> "tt" andalso a <> "ff") andalso findType(a,symbols) = BOOL
                                andalso b = "tt" then (if Array.sub(M,findSymbol(a,symbols)) = 1 then "ff" else "tt") 
                                else if not(notIdentifier(a)) andalso (a <> "tt" andalso a <> "ff") andalso findType(a,symbols) = BOOL
                                andalso b = "ff" then (if Array.sub(M,findSymbol(a,symbols)) = 1 then "ff" else "ff")
                                else if not(notIdentifier(b)) andalso (b <> "tt" andalso   b <> "ff") andalso findType(b,symbols) = BOOL
                                andalso a = "tt" then (if Array.sub(M,findSymbol(b,symbols)) = 1 then "ff" else "ff")
                                else if not(notIdentifier(b)) andalso (b <> "tt" andalso   b <> "ff") andalso findType(b,symbols) = BOOL
                                andalso a = "ff" then (if Array.sub(M,findSymbol(b,symbols)) = 1 then "tt" else "ff")
                                else if not(notIdentifier(a)) andalso not(notIdentifier(b)) andalso 
                                    findType(a,symbols) = BOOL andalso findType(b,symbols) = BOOL then
                                    boolresult("LT",Array.sub(M,findSymbol(a,symbols)),Array.sub(M,findSymbol(b,symbols)))
                                else 
                                if notIdentifier(a) andalso notIdentifier(b) 
                                then boolresult("LT",valOf(Int.fromString(a)), valOf(Int.fromString(b)))
                                else if  not(notIdentifier(a)) andalso notIdentifier(b) then
                                (if findType(a,symbols) = INT then boolresult("LT",Array.sub(M,findSymbol(a,symbols)),valOf(Int.fromString(b)))
                                else raise WrongType) else if notIdentifier(a) andalso not(notIdentifier(b)) then 
                                (if findType(b,symbols) = INT then boolresult("LT",valOf(Int.fromString(a)), Array.sub(M,findSymbol(b,symbols))) 
                                else raise WrongType) else (if findType(a,symbols) = INT andalso findType(b,symbols) = INT
                                then boolresult("LT",Array.sub(M,findSymbol(a,symbols)),Array.sub(M,findSymbol(b,symbols)))
                                else raise WrongType)
                                in operation(second,first)
                                end
                            |change1(String ("GT")) = 
                                let val first = top(valueStack)
                                    val second = top(pop(valueStack))
                                fun operation(a,b) = if (a = "tt"  andalso b = "ff") then
                                "tt"  else if (a = "tt" andalso b = "tt") then
                                "ff" else if (a = "ff" andalso b = "ff") then
                                "ff" else if (a = "ff" andalso b = "tt") then
                                "ff" else if not(notIdentifier(a)) andalso (a <> "tt" andalso   a <> "ff") andalso findType(a,symbols) = BOOL
                                andalso b = "tt" then (if Array.sub(M,findSymbol(a,symbols)) = 1 then "ff" else "ff") 
                                else if not(notIdentifier(a)) andalso (a <> "tt" andalso   a <> "ff") andalso findType(a,symbols) = BOOL
                                andalso b = "ff" then (if Array.sub(M,findSymbol(a,symbols)) = 1 then "tt" else "ff")
                                else if not(notIdentifier(b)) andalso (b <> "tt" andalso   b <> "ff") andalso findType(b,symbols) = BOOL
                                andalso a = "tt" then (if Array.sub(M,findSymbol(b,symbols)) = 1 then "ff" else "tt")
                                else if not(notIdentifier(b)) andalso (b <> "tt" andalso   b <> "ff") andalso findType(b,symbols) = BOOL
                                andalso a = "ff" then (if Array.sub(M,findSymbol(b,symbols)) = 1 then "ff" else "ff")
                                else if not(notIdentifier(a)) andalso not(notIdentifier(b)) andalso 
                                    findType(a,symbols) = BOOL andalso findType(b,symbols) = BOOL then
                                    boolresult("GT",Array.sub(M,findSymbol(a,symbols)),Array.sub(M,findSymbol(b,symbols)))
                                else 
                                
                                if notIdentifier(a) andalso notIdentifier(b) 
                                then boolresult("GT",valOf(Int.fromString(a)), valOf(Int.fromString(b)))
                                else if  not(notIdentifier(a)) andalso notIdentifier(b) then
                                (if findType(a,symbols) = INT then boolresult("GT",Array.sub(M,findSymbol(a,symbols)),valOf(Int.fromString(b)))
                                else raise WrongType) else if notIdentifier(a) andalso not(notIdentifier(b)) then 
                                (if findType(b,symbols) = INT then boolresult("GT",valOf(Int.fromString(a)), Array.sub(M,findSymbol(b,symbols))) 
                                else raise WrongType) else (if findType(a,symbols) = INT andalso findType(b,symbols) = INT
                                then boolresult("GT",Array.sub(M,findSymbol(a,symbols)),Array.sub(M,findSymbol(b,symbols)))
                                else raise WrongType)
                                in operation(second,first)
                                end 
                            |change1(String ("LEQ")) = 
                                let val first = top(valueStack)
                                    val second = top(pop(valueStack))
                                fun operation(a,b) = if (a = "tt"  andalso b = "ff") then
                                "ff"  else if (a = "tt" andalso b = "tt") then
                                "tt" else if (a = "ff" andalso b = "ff") then
                                "tt" else if (a = "ff" andalso b = "tt") then
                                "tt" else if not(notIdentifier(a)) andalso (a <> "tt" andalso   a <> "ff") andalso findType(a,symbols) = BOOL
                                andalso b = "tt" then (if Array.sub(M,findSymbol(a,symbols)) = 1 then "tt" else "tt") 
                                else if not(notIdentifier(a)) andalso (a <> "tt" andalso   a <> "ff") andalso findType(a,symbols) = BOOL
                                andalso b = "ff" then (if Array.sub(M,findSymbol(a,symbols)) = 1 then "ff" else "tt")
                                else if not(notIdentifier(b)) andalso (b <> "tt" andalso   b <> "ff") andalso findType(b,symbols) = BOOL
                                andalso a = "tt" then (if Array.sub(M,findSymbol(b,symbols)) = 1 then "tt" else "ff")
                                else if not(notIdentifier(b)) andalso (b <> "tt" andalso   b <> "ff") andalso findType(b,symbols) = BOOL
                                andalso a = "ff" then (if Array.sub(M,findSymbol(b,symbols)) = 1 then "tt" else "tt")
                                else if not(notIdentifier(a)) andalso not(notIdentifier(b)) andalso 
                                    findType(a,symbols) = BOOL andalso findType(b,symbols) = BOOL then
                                    boolresult("LEQ",Array.sub(M,findSymbol(a,symbols)),Array.sub(M,findSymbol(b,symbols)))
                                else 
                                
                                 if notIdentifier(a) andalso notIdentifier(b) 
                                then boolresult("LEQ",valOf(Int.fromString(a)), valOf(Int.fromString(b)))
                                else if  not(notIdentifier(a)) andalso notIdentifier(b) then
                                (if findType(a,symbols) = INT then boolresult("LEQ",Array.sub(M,findSymbol(a,symbols)),valOf(Int.fromString(b)))
                                else raise WrongType) else if notIdentifier(a) andalso not(notIdentifier(b)) then 
                                (if findType(b,symbols) = INT then boolresult("LEQ",valOf(Int.fromString(a)), Array.sub(M,findSymbol(b,symbols))) 
                                else raise WrongType) else (if findType(a,symbols) = INT andalso findType(b,symbols) = INT
                                then boolresult("LEQ",Array.sub(M,findSymbol(a,symbols)),Array.sub(M,findSymbol(b,symbols)))
                                else raise WrongType)
                                in operation(second,first)
                                end 
                            |change1(String ("GEQ")) = 
                                let val first = top(valueStack)
                                    val second = top(pop(valueStack))
                                fun operation(a,b) = if (a = "tt"  andalso b = "ff") then
                                "tt"  else if (a = "tt" andalso b = "tt") then
                                "tt" else if (a = "ff" andalso b = "ff") then
                                "tt" else if (a = "ff" andalso b = "tt") then
                                "ff" else if not(notIdentifier(a)) andalso (a <> "tt" andalso   a <> "ff") andalso findType(a,symbols) = BOOL
                                andalso b = "tt" then (if Array.sub(M,findSymbol(a,symbols)) = 1 then "tt" else "ff") 
                                else if not(notIdentifier(a)) andalso (a <> "tt" andalso   a <> "ff") andalso findType(a,symbols) = BOOL
                                andalso b = "ff" then (if Array.sub(M,findSymbol(a,symbols)) = 1 then "tt" else "tt")
                                else if not(notIdentifier(b)) andalso (b <> "tt" andalso   b <> "ff") andalso findType(b,symbols) = BOOL
                                andalso a = "tt" then (if Array.sub(M,findSymbol(b,symbols)) = 1 then "tt" else "tt")
                                else if not(notIdentifier(b)) andalso (b <> "tt" andalso   b <> "ff") andalso findType(b,symbols) = BOOL
                                andalso a = "ff" then (if Array.sub(M,findSymbol(b,symbols)) = 1 then "ff" else "tt")
                                else if not(notIdentifier(a)) andalso not(notIdentifier(b)) andalso 
                                    findType(a,symbols) = BOOL andalso findType(b,symbols) = BOOL then
                                    boolresult("GEQ",Array.sub(M,findSymbol(a,symbols)),Array.sub(M,findSymbol(b,symbols)))
                                else 
                                
                                if notIdentifier(a) andalso notIdentifier(b) 
                                then boolresult("GEQ",valOf(Int.fromString(a)), valOf(Int.fromString(b)))
                                else if  not(notIdentifier(a)) andalso notIdentifier(b) then
                                (if findType(a,symbols) = INT then boolresult("GEQ",Array.sub(M,findSymbol(a,symbols)),valOf(Int.fromString(b)))
                                else raise WrongType) else if notIdentifier(a) andalso not(notIdentifier(b)) then 
                                (if findType(b,symbols) = INT then boolresult("GEQ",valOf(Int.fromString(a)), Array.sub(M,findSymbol(b,symbols))) 
                                else raise WrongType) else (if findType(a,symbols) = INT andalso findType(b,symbols) = INT
                                then boolresult("GEQ",Array.sub(M,findSymbol(a,symbols)),Array.sub(M,findSymbol(b,symbols)))
                                else raise WrongType)
                                in operation(second,first)
                                end
                            |change1(String ("EQ")) = 
                                let val first = top(valueStack)
                                    val second = top(pop(valueStack))
                                fun operation(a,b) = if (a = "tt"  andalso b = "ff") then
                                "ff"  else if (a = "tt" andalso b = "tt") then
                                "tt" else if (a = "ff" andalso b = "ff") then
                                "tt" else if (a = "ff" andalso b = "tt") then
                                "ff" else if not(notIdentifier(a)) andalso (a <> "tt" andalso   a <> "ff") andalso findType(a,symbols) = BOOL
                                andalso b = "tt" then (if Array.sub(M,findSymbol(a,symbols)) = 1 then "tt" else "ff") 
                                else if not(notIdentifier(a)) andalso (a <> "tt" andalso   a <> "ff") andalso findType(a,symbols) = BOOL
                                andalso b = "ff" then (if Array.sub(M,findSymbol(a,symbols)) = 1 then "ff" else "tt")
                                else if not(notIdentifier(b)) andalso (b <> "tt" andalso   b <> "ff") andalso findType(b,symbols) = BOOL
                                andalso a = "tt" then (if Array.sub(M,findSymbol(b,symbols)) = 1 then "tt" else "ff")
                                else if not(notIdentifier(b)) andalso (b <> "tt" andalso   b <> "ff") andalso findType(b,symbols) = BOOL
                                andalso a = "ff" then (if Array.sub(M,findSymbol(b,symbols)) = 1 then "ff" else "tt")
                                else if not(notIdentifier(a)) andalso not(notIdentifier(b)) andalso 
                                    findType(a,symbols) = BOOL andalso findType(b,symbols) = BOOL then
                                    boolresult("EQ",Array.sub(M,findSymbol(a,symbols)),Array.sub(M,findSymbol(b,symbols)))
                                else 
                                
                                if notIdentifier(a) andalso notIdentifier(b) 
                                then boolresult("EQ",valOf(Int.fromString(a)), valOf(Int.fromString(b)))
                                else if  not(notIdentifier(a)) andalso notIdentifier(b) then
                                (if findType(a,symbols) = INT then boolresult("EQ",Array.sub(M,findSymbol(a,symbols)),valOf(Int.fromString(b)))
                                else raise WrongType) else if notIdentifier(a) andalso not(notIdentifier(b)) then 
                                (if findType(b,symbols) = INT then boolresult("EQ",valOf(Int.fromString(a)), Array.sub(M,findSymbol(b,symbols))) 
                                else raise WrongType) else (if findType(a,symbols) = INT andalso findType(b,symbols) = INT
                                then boolresult("EQ",Array.sub(M,findSymbol(a,symbols)),Array.sub(M,findSymbol(b,symbols)))
                                else raise WrongType)
                                in operation(second,first)
                                end
                            |change1(String ("NEQ")) = 
                                let val first = top(valueStack)
                                    val second = top(pop(valueStack))
                                fun operation(a,b) = if (a = "tt"  andalso b = "ff") then
                                "tt"  else if (a = "tt" andalso b = "tt") then
                                "ff" else if (a = "ff" andalso b = "ff") then
                                "ff" else if (a = "ff" andalso b = "tt") then
                                "tt" else if not(notIdentifier(a)) andalso (a <> "tt" andalso   a <> "ff") andalso findType(a,symbols) = BOOL
                                andalso b = "tt" then (if Array.sub(M,findSymbol(a,symbols)) = 1 then "ff" else "tt") 
                                else if not(notIdentifier(a)) andalso (a <> "tt" andalso   a <> "ff") andalso findType(a,symbols) = BOOL
                                andalso b = "ff" then (if Array.sub(M,findSymbol(a,symbols)) = 1 then "tt" else "ff")
                                else if not(notIdentifier(b)) andalso (b <> "tt" andalso   b <> "ff") andalso findType(b,symbols) = BOOL
                                andalso a = "tt" then (if Array.sub(M,findSymbol(b,symbols)) = 1 then "ff" else "tt")
                                else if not(notIdentifier(b)) andalso (b <> "tt" andalso   b <> "ff") andalso findType(b,symbols) = BOOL
                                andalso a = "ff" then (if Array.sub(M,findSymbol(b,symbols)) = 1 then "tt" else "ff")
                                else if not(notIdentifier(a)) andalso not(notIdentifier(b)) andalso 
                                    findType(a,symbols) = BOOL andalso findType(b,symbols) = BOOL then
                                    boolresult("NEQ",Array.sub(M,findSymbol(a,symbols)),Array.sub(M,findSymbol(b,symbols)))
                                else 
                                
                                if notIdentifier(a) andalso notIdentifier(b) 
                                then boolresult("NEQ",valOf(Int.fromString(a)), valOf(Int.fromString(b)))
                                else if  not(notIdentifier(a)) andalso notIdentifier(b) then
                                (if findType(a,symbols) = INT then boolresult("NEQ",Array.sub(M,findSymbol(a,symbols)),valOf(Int.fromString(b)))
                                else raise WrongType) else if notIdentifier(a) andalso not(notIdentifier(b)) then 
                                (if findType(b,symbols) = INT then boolresult("NEQ",valOf(Int.fromString(a)), Array.sub(M,findSymbol(b,symbols))) 
                                else raise WrongType) else (if findType(a,symbols) = INT andalso findType(b,symbols) = INT
                                then boolresult("NEQ",Array.sub(M,findSymbol(a,symbols)),Array.sub(M,findSymbol(b,symbols)))
                                else raise WrongType)
                                in operation(second,first)
                                end 
                             |change1(String ("NOT")) = 
                                let val first = top(valueStack)
                                fun operation(a) = if a = "tt" then "ff" else 
                                    if a = "ff" then "tt" else
                                    if not(notIdentifier(a)) then 
                                    (if findType(a,symbols) = BOOL then 
                                    (if Array.sub(M,findSymbol(a,symbols)) = 1 then "ff" else "tt")
                                    else raise WrongType) else raise WrongType 
                                in operation(first)
                                end
                            |change1(String ("AND")) = 
                                let val first = top(valueStack)
                                    val second = top(pop(valueStack))
                                fun operation(a,b) = if (a = "tt" andalso b = "tt") then
                                "tt" else if (a = "tt" andalso b = "ff") then 
                                "ff" else if (a = "ff" andalso b = "tt") then
                                "ff" else if (a = "ff" andalso b = "ff") then
                                "ff" else if not(notIdentifier(a)) andalso b = "tt" 
                                then (if findType(a,symbols) = BOOL then
                                (if funcAND(Array.sub(M,findSymbol(a,symbols)),1) = 1 then "tt"
                                else "ff") else raise WrongType) 
                                else if not(notIdentifier(a)) andalso b = "ff" 
                                then (if findType(a,symbols) = BOOL then
                                (if funcAND(Array.sub(M,findSymbol(a,symbols)),0) = 1 then "tt"
                                else "ff") else raise WrongType)
                                else if not(notIdentifier(b)) andalso a = "tt" 
                                then (if findType(b,symbols) = BOOL then
                                (if funcAND(Array.sub(M,findSymbol(b,symbols)),1) = 1 then "tt"
                                else "ff") else raise WrongType) 
                                else if not(notIdentifier(b)) andalso a = "ff" 
                                then (if findType(b,symbols) = BOOL then
                                (if funcAND(Array.sub(M,findSymbol(b,symbols)),0) = 1 then "tt"
                                else "ff") else raise WrongType)
                                else if  not(notIdentifier(a)) andalso not(notIdentifier(b)) then
                                (if findType(a,symbols) = BOOL andalso findType(b,symbols) = BOOL then 
                                (if funcAND(Array.sub(M,findSymbol(a,symbols)),Array.sub(M,findSymbol(b,symbols))) = 1 then "tt"
                                else "ff")
                                else raise WrongType) else raise WrongType
                                in operation(second,first)
                                end
                            |change1(String ("OR")) = 
                                let val first = top(valueStack)
                                    val second = top(pop(valueStack))
                                fun operation(a,b) = if (a = "tt" andalso b = "tt") then
                                "tt" else if (a = "tt" andalso b = "ff") then 
                                "tt" else if (a = "ff" andalso b = "tt") then
                                "tt" else if (a = "ff" andalso b = "ff") then
                                "ff" else if not(notIdentifier(a)) andalso b = "tt" 
                                then (if findType(a,symbols) = BOOL then
                                (if funcOR(Array.sub(M,findSymbol(a,symbols)),1) = 1 then "tt"
                                else "ff") else raise WrongType) 
                                else if not(notIdentifier(a)) andalso b = "ff" 
                                then (if findType(a,symbols) = BOOL then
                                (if funcOR(Array.sub(M,findSymbol(a,symbols)),0) = 1 then "tt"
                                else "ff") else raise WrongType)
                                else if not(notIdentifier(b)) andalso a = "tt" 
                                then (if findType(b,symbols) = BOOL then
                                (if funcOR(Array.sub(M,findSymbol(b,symbols)),1) = 1 then "tt"
                                else "ff") else raise WrongType) 
                                else if not(notIdentifier(b)) andalso a = "ff" 
                                then (if findType(b,symbols) = BOOL then
                                (if funcOR(Array.sub(M,findSymbol(b,symbols)),0) = 1 then "tt"
                                else "ff") else raise WrongType)
                                else if  not(notIdentifier(a)) andalso not(notIdentifier(b)) then
                                (if findType(a,symbols) = BOOL andalso findType(b,symbols) = BOOL then 
                                (if funcOR(Array.sub(M,findSymbol(a,symbols)),Array.sub(M,findSymbol(b,symbols))) = 1 then "tt"
                                else "ff")
                                else raise WrongType) else raise WrongType
                                in operation(second,first)
                                end
                        in
                            (if x = String ("NOT") then (push(change1(x),pop(valueStack)),pop(controlStack)) else 
                                    (push(change1(x),pop(pop(valueStack))),pop(controlStack)))
                        end

                            
                (*Just complete for all the binary operator in a similar way then set and while and ite left*)
                         
    
    
    (*This function is the application of the semantic rules*)
    (*This function will just shift the identifier and value to valueStack*)
    fun changeStack3(valueStack,controlStack) = 
                let val String(x) = top(controlStack)
                in (push(x,valueStack),pop(controlStack))
                end 
    (*This function will do the assignment in the stack*)
    fun setting(valueStack,controlStack,symbols) =
        let val first = top(valueStack)
            val variable = top(pop(valueStack))
            val varType = findType(variable,symbols)
        in if notIdentifier(first) then (if varType = INT then Array.update(M,findSymbol(variable,symbols),valOf(Int.fromString(first))) else raise WrongType)
        else if first = "tt" orelse first = "ff" then (if varType = BOOL then Array.update(M,findSymbol(variable,symbols),giveBoolVal(first)) else raise WrongType)
        else (if findType(first,symbols) = varType then Array.update(M,findSymbol(variable,symbols),Array.sub(M,findSymbol(first,symbols))) else raise WrongType
        )
        end 

    (*This function will give the transformed stacks after the assignment*)
    fun changeStack4(valueStack,controlStack,symbols) =
            let val x = setting(valueStack,controlStack,symbols)
            in (pop(pop(valueStack)),pop(controlStack))
            end
    (*This function will evaluate an expression and give either boolean or int result*)
    fun expEval(valueStack,controlStack,symbols) = 
            if empty(controlStack) then top(valueStack) else
                let val x = top(controlStack)
                    fun evalExp(String("PLUS")) = changeStack1(valueStack,controlStack,symbols)
                    |evalExp(String("MINUS")) =  changeStack1(valueStack,controlStack,symbols)
                    |evalExp(String("TIMES")) =  changeStack1(valueStack,controlStack,symbols)
                    |evalExp(String("DIV")) =  changeStack1(valueStack,controlStack,symbols)
                    |evalExp(String("MOD")) =  changeStack1(valueStack,controlStack,symbols)
                    |evalExp(String("NEG")) =  changeStack1(valueStack,controlStack,symbols)
                    |evalExp(String("LT")) =  changeStack2(valueStack,controlStack,symbols)
                    |evalExp(String("GT")) =  changeStack2(valueStack,controlStack,symbols)
                    |evalExp(String("GEQ")) =  changeStack2(valueStack,controlStack,symbols)
                    |evalExp(String("LEQ")) =  changeStack2(valueStack,controlStack,symbols)
                    |evalExp(String("EQ")) =  changeStack2(valueStack,controlStack,symbols)
                    |evalExp(String("NEQ")) =  changeStack2(valueStack,controlStack,symbols)
                    |evalExp(String("NOT")) =  changeStack2(valueStack,controlStack,symbols)
                    |evalExp(String("AND")) =  changeStack2(valueStack,controlStack,symbols)
                    |evalExp(String("OR")) =  changeStack2(valueStack,controlStack,symbols)
                    |evalExp(String(x)) = changeStack3(valueStack,controlStack)
                    in 
                        (let val (v,c) = evalExp(x)
                        in expEval(v,c,symbols) end)
                    end
    
    (*This function will apply the reduction rule on if else statements*)
    fun changeStackIF(valueStack,controlStack,symbols) = 
            let val ITEConc(e,c,d,"ITE") = top(controlStack)
                val boolVal = expEval(create,list2Stack(rev(e)),symbols)
                val checkCond = if boolVal = "tt" then true else if boolVal = "ff" then false else 
                        if findType(boolVal,symbols) = BOOL andalso Array.sub(M,findSymbol(boolVal,symbols)) = 1 then true
                        else if findType(boolVal,symbols) = BOOL andalso Array.sub(M,findSymbol(boolVal,symbols)) = 0 then false
                        else raise WrongType
                val finalStack = if checkCond then list2Stack(rev(c @ rev(Stack2list(pop(controlStack))))) 
                                else list2Stack(rev(d @ rev(Stack2list(pop(controlStack)))))
            in (valueStack,finalStack)
            end

    (*This function will apply the reduction rule on while statement*)
    fun changeStackWhile(valueStack,controlStack,symbols) = 
            let val WHConc(e,c,"WH") = top(controlStack)
                val boolVal = expEval(create,list2Stack(rev(e)),symbols)
                val checkCond = if boolVal = "tt" then true else if boolVal = "ff" then false else 
                        if findType(boolVal,symbols) = BOOL andalso Array.sub(M,findSymbol(boolVal,symbols)) = 1 then true
                        else if findType(boolVal,symbols) = BOOL andalso Array.sub(M,findSymbol(boolVal,symbols)) = 0 then false
                        else raise WrongType
                val finalStack = if checkCond then list2Stack(rev(c @ rev(Stack2list(controlStack)))) 
                                else pop(controlStack)
            in (valueStack,finalStack)
            end

    (*These semantics will be applied when read is to be done*)
    fun changeStackRead(valueStack,controlStack,symbols) = 
            let val valueElement = top(valueStack)
                val y = valOf(TextIO.inputLine TextIO.stdIn)
                val typeVar = findType(valueElement,symbols)
                val index = findSymbol(valueElement,symbols)
                val valueElement = top(valueStack)
                val assignm = if typeVar = BOOL then (if y = "tt\n" then Array.update(M,index,1) else 
                                if y = "ff\n" then Array.update(M,index,0) else raise WrongType) 
                            else Array.update(M,index,valOf(Int.fromString(y)))
            in 
                (pop(valueStack),pop(controlStack))
            end
    
    (*This function will apply semantics for write*)
    fun changeStackWrite(valueStack,controlStack,symbols) = 
            let val x = top(valueStack)
                val y = if x = "tt" orelse x = "ff" then x else 
                        if not(notIdentifier(x)) then Int.toString(Array.sub(M,findSymbol(x,symbols))) else x
                val pr = print(y ^ "\n")
            in 
                (pop(valueStack),pop(controlStack))
            end

    (*These are our semantic rules *)
    fun rules(valueStack,controlStack,symbols) = 
            let val x  = top(controlStack)
                fun change(String("PLUS")) = changeStack1(valueStack,controlStack,symbols)
                    |change(String("MINUS")) =  changeStack1(valueStack,controlStack,symbols)
                    |change(String("TIMES")) =  changeStack1(valueStack,controlStack,symbols)
                    |change(String("DIV")) =  changeStack1(valueStack,controlStack,symbols)
                    |change(String("MOD")) =  changeStack1(valueStack,controlStack,symbols)
                    |change(String("NEG")) =  changeStack1(valueStack,controlStack,symbols)
                    |change(String("LT")) =  changeStack2(valueStack,controlStack,symbols)
                    |change(String("GT")) =  changeStack2(valueStack,controlStack,symbols)
                    |change(String("GEQ")) =  changeStack2(valueStack,controlStack,symbols)
                    |change(String("LEQ")) =  changeStack2(valueStack,controlStack,symbols)
                    |change(String("EQ")) =  changeStack2(valueStack,controlStack,symbols)
                    |change(String("NEQ")) =  changeStack2(valueStack,controlStack,symbols)
                    |change(String("NOT")) =  changeStack2(valueStack,controlStack,symbols)
                    |change(String("AND")) =  changeStack2(valueStack,controlStack,symbols)
                    |change(String("OR")) =  changeStack2(valueStack,controlStack,symbols)
                    |change(String("SET")) = changeStack4(valueStack,controlStack,symbols)
                    |change(String ("SEQ")) = (valueStack,pop(controlStack))
                    |change(ITEConc(x,y,z,"ITE")) = changeStackIF(valueStack,controlStack,symbols)
                    |change(WHConc(x,y,"WH")) = changeStackWhile(valueStack,controlStack,symbols)
                    |change(String ("READ")) = changeStackRead(valueStack,controlStack,symbols)
                    |change(String ("WRITE")) = changeStackWrite(valueStack,controlStack,symbols)
                    |change(String (x))  = changeStack3(valueStack,controlStack)
            in 
                change(x)
            end
    
    fun evaluating (valueStack,controlStack,symbols) = 
        if empty(controlStack) then (valueStack,controlStack)
        else 
            (let val (v,c) = rules(valueStack,controlStack,symbols)
            in evaluating(v,c,symbols)
            end)
    
    (*This function will evaluate and give the configuration*)
    fun execute(filename) = 
        let val symbols = symbolTable(filename)
            val V = create
            val C = postfix(filename)
            val (v,c) = evaluating(V,C,symbols)
        in 
            (v,M,c)
        end
    fun stackPost(c) = 
        if length(c) = 0 then [] else (
        let fun getString(String x) = [x]
            |getString(ITEConc(x,y,z,w)) = stackPost(x) @ stackPost(y) @ stackPost(z) @ [w]
            |getString(WHConc(x,y,w)) = stackPost(x) @ stackPost(y) @ [w]
        in getString(hd(c)) @ stackPost(tl(c))
        end)


    fun toString(v,c) =
        let  val control = rev(Stack2list(c))
            val controlNew = stackPost(control)
            val memoryView = []
        in (rev(Stack2list(v)),M,controlNew)
        end
end 
open Vmc;

