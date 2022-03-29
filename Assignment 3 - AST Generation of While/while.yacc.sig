signature While_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val ENDWH:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val ENDIF:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val WRITE:  'a * 'a -> (svalue,'a) token
val READ:  'a * 'a -> (svalue,'a) token
val ProgramStart:  'a * 'a -> (svalue,'a) token
val SingleColon:  'a * 'a -> (svalue,'a) token
val DoubleColon:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
val INT:  'a * 'a -> (svalue,'a) token
val BOOL:  'a * 'a -> (svalue,'a) token
val Comma:  'a * 'a -> (svalue,'a) token
val Assign:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val False: (string) *  'a * 'a -> (svalue,'a) token
val True: (string) *  'a * 'a -> (svalue,'a) token
val NEG:  'a * 'a -> (svalue,'a) token
val Mod:  'a * 'a -> (svalue,'a) token
val Div:  'a * 'a -> (svalue,'a) token
val Mul:  'a * 'a -> (svalue,'a) token
val Sub:  'a * 'a -> (svalue,'a) token
val Add:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val NEQ:  'a * 'a -> (svalue,'a) token
val GEQ:  'a * 'a -> (svalue,'a) token
val LEQ:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val SemiColon:  'a * 'a -> (svalue,'a) token
val RR:  'a * 'a -> (svalue,'a) token
val LR:  'a * 'a -> (svalue,'a) token
val RC:  'a * 'a -> (svalue,'a) token
val LC:  'a * 'a -> (svalue,'a) token
val Numeral: (string) *  'a * 'a -> (svalue,'a) token
val Identifier: (string) *  'a * 'a -> (svalue,'a) token
end
signature While_LRVALS=
sig
structure Tokens : While_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
