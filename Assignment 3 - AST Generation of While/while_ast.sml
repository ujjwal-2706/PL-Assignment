(*Here I have defined the appropriate constructors and datatypes according to the assignment*)
structure AST =  
struct
   exception Error
   type variable = string
   type term =  string
   datatype typ = INT|BOOL
   and AST = PROG of string  * block
   and block = BLK of (declaration list) * comseq
   and declaration = DEC of (variable list) * typ
   and comseq = SEQ of  command list 
   and command = SET of variable * express | Read of variable | Write of express | ITE of express * comseq * comseq
                | WH of express * comseq 
   and express = PLUS of express * express | MINUS of express * express| TIMES of express * express
        | DIV of express * express | MOD of express * express| NEG of express |NOT of express
        | AND of express * express | OR of express * express | Factor of term
        | LT of express * express| LEQ of express * express | EQ of express * express 
        | GT of express * express| GEQ of express * express | NEQ of express * express
   
end

exception ASTError
(* This is the part of code for connecting ML-Lex with ML-Yacc *)
structure WhileLrVals = WhileLrValsFun(structure Token = LrParser.Token)
structure WhileLex = WhileLexFun(structure Tokens = WhileLrVals.Tokens);
structure WhileParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = WhileLrVals.ParserData
     	       structure Lex = WhileLex)
     

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
fun parser(file) = parse(lexing(readFile(file)));