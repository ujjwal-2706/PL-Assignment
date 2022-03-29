structure Token= Tokens
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token
  exception IllegalCharacter
  val pos = ref 1
  val col = ref 1
  val eof = fn () => (Tokens.EOF(!pos, !pos))
  fun error(err, col: int, line : int) = TextIO.output(TextIO.stdOut,"Unknown Token:"^Int.toString(line)^":"^Int.toString(col)^":"^err)
  
%%
%header (functor WhileLexFun(structure Tokens: While_TOKENS));

INT = "int";
BOOL = "bool";
READ = "read";
WRITE = "write";
ENDIF = "endif";
WHILE = "while";
DO = "do";
IF = "if";
THEN = "then";
ELSE = "else";
ENDWH = "endwh";
VAR = "var";
Program = "program";
True = "tt";
False = "ff";
Identifier = [a-zA-Z][a-zA-Z0-9]*;
Numeral = \+[0-9][0-9]*|[0-9][0-9]*|"~"[0-9][0-9]*;
wasteSpace = [\ \t\n\b];
%%
";" => (col := !col + 1; Token.SemiColon(!pos,!pos));
"{" =>  (col := !col +1; Token.LC(!pos,!pos));
"}" =>  (col := !col +1; Token.RC(!pos,!pos));
"," =>  (col := !col +1; Token.Comma(!pos,!pos));
":=" => (col := !col +2; Token.Assign(!pos,!pos));
"::" => (col := !col +2; Token.DoubleColon(!pos,!pos));
":" =>  (col := !col +1; Token.SingleColon(!pos,!pos));
"(" =>  (col := !col +1; Token.LR(!pos,!pos));
")" =>  (col := !col +1; Token.RR(!pos,!pos));
"||" => (col := !col +2; Token.OR(!pos,!pos));
"!" =>  (col := !col +1; Token.NOT(!pos,!pos));
"&&" => (col := !col +2; Token.AND(!pos,!pos));
"<=" => (col := !col +2; Token.LEQ(!pos,!pos));
"<" =>  (col := !col +1; Token.LT(!pos,!pos));
">=" => (col := !col +2; Token.GEQ(!pos,!pos));
">" =>  (col := !col +1; Token.GT(!pos,!pos));
"<>" => (col := !col +2; Token.NEQ(!pos,!pos));
"=" =>  (col := !col +1; Token.EQ(!pos,!pos));
"+" =>  (col := !col +1; Token.Add(!pos,!pos));
"-" =>  (col := !col +1; Token.Sub(!pos,!pos));
"*" =>  (col := !col +1; Token.Mul(!pos,!pos));
"/" =>  (col := !col +1; Token.Div(!pos,!pos));
"%" =>  (col := !col +1; Token.Mod(!pos,!pos));
"~" =>  (col := !col +1; Token.NEG(!pos,!pos));
{INT}=> (col := !col +3; Token.INT(!pos,!pos));
{BOOL}=> (col := !col +4; Token.BOOL(!pos,!pos));
{READ}=> (col := !col +4; Token.READ(!pos,!pos));
{WRITE}=> (col := !col +5; Token.WRITE(!pos,!pos));
{ENDIF}=> (col := !col +5; Token.ENDIF(!pos,!pos));
{WHILE}=> (col := !col +5; Token.WHILE(!pos,!pos));
{DO}=> (col := !col +2; Token.DO(!pos,!pos));
{IF}=> (col := !col +2; Token.IF(!pos,!pos));
{THEN}=> (col := !col +4; Token.THEN(!pos,!pos));
{ELSE}=> (col := !col +4; Token.ELSE(!pos,!pos));
{ENDWH}=> (col := !col +5; Token.ENDWH(!pos,!pos));
{VAR}=> (col := !col +3; Token.VAR(!pos,!pos));
{Program}=> (col := !col +7; Token.ProgramStart(!pos,!pos));
{True}=> (col := !col +2; Token.True(yytext,!pos,!pos));
{False}=> (col := !col +2; Token.False(yytext,!pos,!pos));
{Identifier}=> (col := !col + size(yytext); Token.Identifier(yytext,!pos,!pos));
{Numeral}=> (col := !col +size(yytext); Token.Numeral(yytext,!pos,!pos));
{wasteSpace} => (col := !col +1; lex());
. => (error(yytext,!col,!pos); col := 1; pos := 1; raise IllegalCharacter;lex());