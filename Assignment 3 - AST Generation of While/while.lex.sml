functor WhileLexFun(structure Tokens: While_TOKENS)=
   struct
    structure UserDeclarations =
      struct
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
  
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\086\086\087\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\086\085\003\003\003\084\082\003\081\080\079\077\076\075\003\074\
\\072\072\072\072\072\072\072\072\072\072\069\068\065\064\062\003\
\\003\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\003\003\003\003\003\
\\003\010\058\010\056\046\044\010\010\040\010\010\010\010\010\010\
\\033\010\029\010\024\010\021\012\010\010\010\009\007\006\004\003\
\\003"
),
 (4, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
\\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\017\011\011\011\011\011\011\011\
\\011\011\013\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\014\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\015\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (15, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\016\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (17, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\018\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\019\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (19, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\020\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (21, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\022\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (22, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\023\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (24, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\026\011\011\011\011\011\011\011\
\\011\011\011\011\025\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (26, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\027\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (27, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\028\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (29, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\030\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (30, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\031\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (31, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\032\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (33, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\034\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (34, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\035\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (35, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\036\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (36, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\037\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (37, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\038\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (38, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\039\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (40, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\043\011\011\011\011\011\011\011\041\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (41, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\042\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (44, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\045\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (46, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\053\011\047\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (47, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\048\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (48, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\051\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\049\011\011\011\000\000\000\000\000\
\\000"
),
 (49, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\050\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (51, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\052\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (53, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\054\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (54, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\055\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (56, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\057\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (58, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\059\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (59, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\060\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (60, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\061\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (62, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\063\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (65, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\067\066\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (69, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\071\000\000\070\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (72, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\073\073\073\073\073\073\073\073\073\073\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (77, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\078\078\078\078\078\078\078\078\078\078\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (82, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\083\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 142)], trans = 0},
{fin = [(N 54),(N 142)], trans = 4},
{fin = [(N 138)], trans = 4},
{fin = [(N 5),(N 142)], trans = 0},
{fin = [(N 142)], trans = 7},
{fin = [(N 22)], trans = 0},
{fin = [(N 3),(N 142)], trans = 0},
{fin = [(N 129),(N 142)], trans = 10},
{fin = [(N 129)], trans = 10},
{fin = [(N 129),(N 142)], trans = 12},
{fin = [(N 129)], trans = 13},
{fin = [(N 129)], trans = 14},
{fin = [(N 129)], trans = 15},
{fin = [(N 74),(N 129)], trans = 10},
{fin = [(N 129)], trans = 17},
{fin = [(N 129)], trans = 18},
{fin = [(N 129)], trans = 19},
{fin = [(N 86),(N 129)], trans = 10},
{fin = [(N 129),(N 142)], trans = 21},
{fin = [(N 129)], trans = 22},
{fin = [(N 112),(N 129)], trans = 10},
{fin = [(N 129),(N 142)], trans = 24},
{fin = [(N 123),(N 129)], trans = 10},
{fin = [(N 129)], trans = 26},
{fin = [(N 129)], trans = 27},
{fin = [(N 97),(N 129)], trans = 10},
{fin = [(N 129),(N 142)], trans = 29},
{fin = [(N 129)], trans = 30},
{fin = [(N 129)], trans = 31},
{fin = [(N 68),(N 129)], trans = 10},
{fin = [(N 129),(N 142)], trans = 33},
{fin = [(N 129)], trans = 34},
{fin = [(N 129)], trans = 35},
{fin = [(N 129)], trans = 36},
{fin = [(N 129)], trans = 37},
{fin = [(N 129)], trans = 38},
{fin = [(N 120),(N 129)], trans = 10},
{fin = [(N 129),(N 142)], trans = 40},
{fin = [(N 129)], trans = 41},
{fin = [(N 58),(N 129)], trans = 10},
{fin = [(N 92),(N 129)], trans = 10},
{fin = [(N 129),(N 142)], trans = 44},
{fin = [(N 126),(N 129)], trans = 10},
{fin = [(N 129),(N 142)], trans = 46},
{fin = [(N 129)], trans = 47},
{fin = [(N 129)], trans = 48},
{fin = [(N 129)], trans = 49},
{fin = [(N 108),(N 129)], trans = 10},
{fin = [(N 129)], trans = 51},
{fin = [(N 80),(N 129)], trans = 10},
{fin = [(N 129)], trans = 53},
{fin = [(N 129)], trans = 54},
{fin = [(N 102),(N 129)], trans = 10},
{fin = [(N 129),(N 142)], trans = 56},
{fin = [(N 89),(N 129)], trans = 10},
{fin = [(N 129),(N 142)], trans = 58},
{fin = [(N 129)], trans = 59},
{fin = [(N 129)], trans = 60},
{fin = [(N 63),(N 129)], trans = 10},
{fin = [(N 37),(N 142)], trans = 62},
{fin = [(N 35)], trans = 0},
{fin = [(N 42),(N 142)], trans = 0},
{fin = [(N 32),(N 142)], trans = 65},
{fin = [(N 40)], trans = 0},
{fin = [(N 30)], trans = 0},
{fin = [(N 1),(N 142)], trans = 0},
{fin = [(N 15),(N 142)], trans = 69},
{fin = [(N 10)], trans = 0},
{fin = [(N 13)], trans = 0},
{fin = [(N 138),(N 142)], trans = 72},
{fin = [(N 138)], trans = 72},
{fin = [(N 50),(N 142)], trans = 0},
{fin = [(N 46),(N 142)], trans = 0},
{fin = [(N 7),(N 142)], trans = 0},
{fin = [(N 44),(N 142)], trans = 77},
{fin = [(N 138)], trans = 77},
{fin = [(N 48),(N 142)], trans = 0},
{fin = [(N 19),(N 142)], trans = 0},
{fin = [(N 17),(N 142)], trans = 0},
{fin = [(N 142)], trans = 82},
{fin = [(N 27)], trans = 0},
{fin = [(N 52),(N 142)], trans = 0},
{fin = [(N 24),(N 142)], trans = 0},
{fin = [(N 140),(N 142)], trans = 0},
{fin = [(N 140)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => (col := !col + 1; Token.SemiColon(!pos,!pos))
| 10 => (col := !col +2; Token.Assign(!pos,!pos))
| 102 => (col := !col +4; Token.ELSE(!pos,!pos))
| 108 => (col := !col +5; Token.ENDWH(!pos,!pos))
| 112 => (col := !col +3; Token.VAR(!pos,!pos))
| 120 => (col := !col +7; Token.ProgramStart(!pos,!pos))
| 123 => let val yytext=yymktext() in col := !col +2; Token.True(yytext,!pos,!pos) end
| 126 => let val yytext=yymktext() in col := !col +2; Token.False(yytext,!pos,!pos) end
| 129 => let val yytext=yymktext() in col := !col + size(yytext); Token.Identifier(yytext,!pos,!pos) end
| 13 => (col := !col +2; Token.DoubleColon(!pos,!pos))
| 138 => let val yytext=yymktext() in col := !col +size(yytext); Token.Numeral(yytext,!pos,!pos) end
| 140 => (col := !col +1; lex())
| 142 => let val yytext=yymktext() in error(yytext,!col,!pos); col := 1; pos := 1; raise IllegalCharacter;lex() end
| 15 => (col := !col +1; Token.SingleColon(!pos,!pos))
| 17 => (col := !col +1; Token.LR(!pos,!pos))
| 19 => (col := !col +1; Token.RR(!pos,!pos))
| 22 => (col := !col +2; Token.OR(!pos,!pos))
| 24 => (col := !col +1; Token.NOT(!pos,!pos))
| 27 => (col := !col +2; Token.AND(!pos,!pos))
| 3 => (col := !col +1; Token.LC(!pos,!pos))
| 30 => (col := !col +2; Token.LEQ(!pos,!pos))
| 32 => (col := !col +1; Token.LT(!pos,!pos))
| 35 => (col := !col +2; Token.GEQ(!pos,!pos))
| 37 => (col := !col +1; Token.GT(!pos,!pos))
| 40 => (col := !col +2; Token.NEQ(!pos,!pos))
| 42 => (col := !col +1; Token.EQ(!pos,!pos))
| 44 => (col := !col +1; Token.Add(!pos,!pos))
| 46 => (col := !col +1; Token.Sub(!pos,!pos))
| 48 => (col := !col +1; Token.Mul(!pos,!pos))
| 5 => (col := !col +1; Token.RC(!pos,!pos))
| 50 => (col := !col +1; Token.Div(!pos,!pos))
| 52 => (col := !col +1; Token.Mod(!pos,!pos))
| 54 => (col := !col +1; Token.NEG(!pos,!pos))
| 58 => (col := !col +3; Token.INT(!pos,!pos))
| 63 => (col := !col +4; Token.BOOL(!pos,!pos))
| 68 => (col := !col +4; Token.READ(!pos,!pos))
| 7 => (col := !col +1; Token.Comma(!pos,!pos))
| 74 => (col := !col +5; Token.WRITE(!pos,!pos))
| 80 => (col := !col +5; Token.ENDIF(!pos,!pos))
| 86 => (col := !col +5; Token.WHILE(!pos,!pos))
| 89 => (col := !col +2; Token.DO(!pos,!pos))
| 92 => (col := !col +2; Token.IF(!pos,!pos))
| 97 => (col := !col +4; Token.THEN(!pos,!pos))
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end