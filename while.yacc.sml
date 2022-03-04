functor WhileLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : While_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*User declaration Region*)
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

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\005\000\000\000\
\\001\000\001\000\016\000\000\000\
\\001\000\001\000\016\000\002\000\037\000\005\000\036\000\019\000\035\000\
\\020\000\034\000\021\000\033\000\024\000\032\000\000\000\
\\001\000\003\000\013\000\000\000\
\\001\000\004\000\026\000\000\000\
\\001\000\006\000\080\000\008\000\060\000\009\000\059\000\010\000\058\000\
\\011\000\057\000\012\000\056\000\013\000\055\000\014\000\054\000\
\\015\000\053\000\016\000\052\000\017\000\051\000\018\000\050\000\
\\022\000\049\000\023\000\048\000\000\000\
\\001\000\007\000\027\000\000\000\
\\001\000\007\000\065\000\000\000\
\\001\000\008\000\060\000\009\000\059\000\010\000\058\000\011\000\057\000\
\\012\000\056\000\013\000\055\000\014\000\054\000\015\000\053\000\
\\016\000\052\000\017\000\051\000\018\000\050\000\022\000\049\000\
\\023\000\048\000\036\000\064\000\000\000\
\\001\000\008\000\060\000\009\000\059\000\010\000\058\000\011\000\057\000\
\\012\000\056\000\013\000\055\000\014\000\054\000\015\000\053\000\
\\016\000\052\000\017\000\051\000\018\000\050\000\022\000\049\000\
\\023\000\048\000\040\000\047\000\000\000\
\\001\000\025\000\028\000\000\000\
\\001\000\027\000\044\000\028\000\043\000\000\000\
\\001\000\030\000\006\000\000\000\
\\001\000\031\000\025\000\000\000\
\\001\000\032\000\004\000\000\000\
\\001\000\037\000\083\000\000\000\
\\001\000\038\000\085\000\000\000\
\\001\000\041\000\082\000\000\000\
\\001\000\042\000\000\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\029\000\010\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\026\000\024\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\001\000\016\000\033\000\023\000\034\000\022\000\035\000\021\000\
\\039\000\020\000\000\000\
\\101\000\008\000\060\000\009\000\059\000\010\000\058\000\011\000\057\000\
\\012\000\056\000\013\000\055\000\014\000\054\000\015\000\053\000\
\\016\000\052\000\017\000\051\000\018\000\050\000\022\000\049\000\
\\023\000\048\000\000\000\
\\102\000\000\000\
\\103\000\008\000\060\000\009\000\059\000\010\000\058\000\011\000\057\000\
\\012\000\056\000\013\000\055\000\014\000\054\000\015\000\053\000\
\\016\000\052\000\017\000\051\000\018\000\050\000\022\000\049\000\
\\023\000\048\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\016\000\052\000\017\000\051\000\018\000\050\000\000\000\
\\107\000\016\000\052\000\017\000\051\000\018\000\050\000\000\000\
\\108\000\017\000\051\000\018\000\050\000\000\000\
\\109\000\000\000\
\\110\000\017\000\051\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\014\000\054\000\015\000\053\000\016\000\052\000\017\000\051\000\
\\018\000\050\000\000\000\
\\114\000\014\000\054\000\015\000\053\000\016\000\052\000\017\000\051\000\
\\018\000\050\000\000\000\
\\115\000\014\000\054\000\015\000\053\000\016\000\052\000\017\000\051\000\
\\018\000\050\000\000\000\
\\116\000\014\000\054\000\015\000\053\000\016\000\052\000\017\000\051\000\
\\018\000\050\000\022\000\049\000\023\000\048\000\000\000\
\\117\000\014\000\054\000\015\000\053\000\016\000\052\000\017\000\051\000\
\\018\000\050\000\022\000\049\000\023\000\048\000\000\000\
\\118\000\014\000\054\000\015\000\053\000\016\000\052\000\017\000\051\000\
\\018\000\050\000\022\000\049\000\023\000\048\000\000\000\
\\119\000\014\000\054\000\015\000\053\000\016\000\052\000\017\000\051\000\
\\018\000\050\000\022\000\049\000\023\000\048\000\000\000\
\\120\000\014\000\054\000\015\000\053\000\016\000\052\000\017\000\051\000\
\\018\000\050\000\022\000\049\000\023\000\048\000\000\000\
\\121\000\014\000\054\000\015\000\053\000\016\000\052\000\017\000\051\000\
\\018\000\050\000\022\000\049\000\023\000\048\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\"
val actionRowNumbers =
"\014\000\019\000\000\000\012\000\
\\023\000\023\000\003\000\020\000\
\\001\000\022\000\021\000\032\000\
\\028\000\013\000\029\000\004\000\
\\006\000\010\000\002\000\002\000\
\\002\000\001\000\001\000\011\000\
\\030\000\032\000\002\000\054\000\
\\009\000\056\000\002\000\058\000\
\\057\000\002\000\002\000\055\000\
\\008\000\035\000\034\000\027\000\
\\007\000\025\000\026\000\031\000\
\\033\000\003\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\047\000\
\\044\000\005\000\003\000\024\000\
\\017\000\046\000\045\000\042\000\
\\041\000\040\000\039\000\038\000\
\\052\000\053\000\051\000\049\000\
\\050\000\048\000\043\000\015\000\
\\037\000\003\000\016\000\036\000\
\\018\000"
val gotoT =
"\
\\001\000\084\000\002\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\007\000\004\000\006\000\006\000\005\000\000\000\
\\004\000\009\000\006\000\005\000\000\000\
\\005\000\010\000\000\000\
\\000\000\
\\008\000\013\000\009\000\012\000\000\000\
\\000\000\
\\000\000\
\\009\000\017\000\010\000\016\000\011\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\029\000\012\000\028\000\013\000\027\000\000\000\
\\009\000\029\000\012\000\036\000\013\000\027\000\000\000\
\\009\000\029\000\012\000\037\000\013\000\027\000\000\000\
\\009\000\038\000\000\000\
\\008\000\039\000\009\000\012\000\000\000\
\\007\000\040\000\000\000\
\\000\000\
\\009\000\017\000\010\000\016\000\011\000\043\000\000\000\
\\009\000\029\000\012\000\044\000\013\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\029\000\012\000\059\000\013\000\027\000\000\000\
\\000\000\
\\000\000\
\\009\000\029\000\012\000\060\000\013\000\027\000\000\000\
\\009\000\029\000\012\000\061\000\013\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\064\000\000\000\
\\009\000\029\000\012\000\065\000\013\000\027\000\000\000\
\\009\000\029\000\012\000\066\000\013\000\027\000\000\000\
\\009\000\029\000\012\000\067\000\013\000\027\000\000\000\
\\009\000\029\000\012\000\068\000\013\000\027\000\000\000\
\\009\000\029\000\012\000\069\000\013\000\027\000\000\000\
\\009\000\029\000\012\000\070\000\013\000\027\000\000\000\
\\009\000\029\000\012\000\071\000\013\000\027\000\000\000\
\\009\000\029\000\012\000\072\000\013\000\027\000\000\000\
\\009\000\029\000\012\000\073\000\013\000\027\000\000\000\
\\009\000\029\000\012\000\074\000\013\000\027\000\000\000\
\\009\000\029\000\012\000\075\000\013\000\027\000\000\000\
\\009\000\029\000\012\000\076\000\013\000\027\000\000\000\
\\009\000\029\000\012\000\077\000\013\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\079\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\082\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 85
val numrules = 40
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | False of unit ->  (string) | True of unit ->  (string)
 | Numeral of unit ->  (string) | Identifier of unit ->  (string)
 | ExpTerm of unit ->  (AST.term) | Exp of unit ->  (AST.express)
 | CommandContinue of unit ->  ( ( AST.command list ) )
 | Command of unit ->  (AST.command)
 | Variable of unit ->  (AST.variable)
 | VariableList of unit ->  ( ( AST.variable list ) )
 | Type of unit ->  (AST.typ)
 | Declaration of unit ->  (AST.declaration)
 | ComSeq of unit ->  (AST.comseq)
 | DecSeq of unit ->  ( ( AST.declaration list ) )
 | Block of unit ->  (AST.block) | Program of unit ->  (AST.AST)
 | Start of unit ->  (AST.AST)
end
type svalue = MlyValue.svalue
type result = AST.AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 41) => true | _ => false
val showTerminal =
fn (T 0) => "Identifier"
  | (T 1) => "Numeral"
  | (T 2) => "LC"
  | (T 3) => "RC"
  | (T 4) => "LR"
  | (T 5) => "RR"
  | (T 6) => "SemiColon"
  | (T 7) => "LT"
  | (T 8) => "GT"
  | (T 9) => "LEQ"
  | (T 10) => "GEQ"
  | (T 11) => "NEQ"
  | (T 12) => "EQ"
  | (T 13) => "Add"
  | (T 14) => "Sub"
  | (T 15) => "Mul"
  | (T 16) => "Div"
  | (T 17) => "Mod"
  | (T 18) => "NEG"
  | (T 19) => "True"
  | (T 20) => "False"
  | (T 21) => "OR"
  | (T 22) => "AND"
  | (T 23) => "NOT"
  | (T 24) => "Assign"
  | (T 25) => "Comma"
  | (T 26) => "BOOL"
  | (T 27) => "INT"
  | (T 28) => "VAR"
  | (T 29) => "DoubleColon"
  | (T 30) => "SingleColon"
  | (T 31) => "ProgramStart"
  | (T 32) => "READ"
  | (T 33) => "WRITE"
  | (T 34) => "IF"
  | (T 35) => "THEN"
  | (T 36) => "ELSE"
  | (T 37) => "ENDIF"
  | (T 38) => "WHILE"
  | (T 39) => "DO"
  | (T 40) => "ENDWH"
  | (T 41) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35)
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Program Program1, Program1left, 
Program1right)) :: rest671)) => let val  result = MlyValue.Start (fn _
 => let val  (Program as Program1) = Program1 ()
 in (Program)
end)
 in ( LrTable.NT 0, ( result, Program1left, Program1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.Block Block1, _, Block1right)) :: _ :: ( _, 
( MlyValue.Identifier Identifier1, _, _)) :: ( _, ( _, 
ProgramStart1left, _)) :: rest671)) => let val  result = 
MlyValue.Program (fn _ => let val  (Identifier as Identifier1) = 
Identifier1 ()
 val  (Block as Block1) = Block1 ()
 in (AST.PROG (Identifier,Block))
end)
 in ( LrTable.NT 1, ( result, ProgramStart1left, Block1right), rest671
)
end
|  ( 2, ( ( _, ( MlyValue.ComSeq ComSeq1, _, ComSeq1right)) :: ( _, ( 
MlyValue.DecSeq DecSeq1, DecSeq1left, _)) :: rest671)) => let val  
result = MlyValue.Block (fn _ => let val  (DecSeq as DecSeq1) = 
DecSeq1 ()
 val  (ComSeq as ComSeq1) = ComSeq1 ()
 in (AST.BLK(DecSeq,ComSeq))
end)
 in ( LrTable.NT 2, ( result, DecSeq1left, ComSeq1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.DecSeq DecSeq1, _, DecSeq1right)) :: ( _, ( 
MlyValue.Declaration Declaration1, Declaration1left, _)) :: rest671))
 => let val  result = MlyValue.DecSeq (fn _ => let val  (Declaration
 as Declaration1) = Declaration1 ()
 val  (DecSeq as DecSeq1) = DecSeq1 ()
 in (Declaration :: DecSeq)
end)
 in ( LrTable.NT 3, ( result, Declaration1left, DecSeq1right), rest671
)
end
|  ( 4, ( rest671)) => let val  result = MlyValue.DecSeq (fn _ => ([])
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 5, ( ( _, ( _, _, SemiColon1right)) :: ( _, ( MlyValue.Type Type1
, _, _)) :: _ :: ( _, ( MlyValue.VariableList VariableList1, _, _)) ::
 ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Declaration (fn _ => let val  (VariableList as VariableList1)
 = VariableList1 ()
 val  (Type as Type1) = Type1 ()
 in (AST.DEC(VariableList,Type))
end)
 in ( LrTable.NT 5, ( result, VAR1left, SemiColon1right), rest671)
end
|  ( 6, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (AST.INT))
 in ( LrTable.NT 6, ( result, INT1left, INT1right), rest671)
end
|  ( 7, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (AST.BOOL))
 in ( LrTable.NT 6, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.VariableList VariableList1, _, 
VariableList1right)) :: _ :: ( _, ( MlyValue.Variable Variable1, 
Variable1left, _)) :: rest671)) => let val  result = 
MlyValue.VariableList (fn _ => let val  (Variable as Variable1) = 
Variable1 ()
 val  (VariableList as VariableList1) = VariableList1 ()
 in (Variable :: VariableList)
end)
 in ( LrTable.NT 7, ( result, Variable1left, VariableList1right), 
rest671)
end
|  ( 9, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = 
MlyValue.VariableList (fn _ => let val  (Variable as Variable1) = 
Variable1 ()
 in ([Variable])
end)
 in ( LrTable.NT 7, ( result, Variable1left, Variable1right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.Identifier Identifier1, Identifier1left, 
Identifier1right)) :: rest671)) => let val  result = MlyValue.Variable
 (fn _ => let val  (Identifier as Identifier1) = Identifier1 ()
 in (Identifier)
end)
 in ( LrTable.NT 8, ( result, Identifier1left, Identifier1right), 
rest671)
end
|  ( 11, ( ( _, ( _, _, RC1right)) :: ( _, ( MlyValue.CommandContinue 
CommandContinue1, _, _)) :: ( _, ( _, LC1left, _)) :: rest671)) => let
 val  result = MlyValue.ComSeq (fn _ => let val  (CommandContinue as 
CommandContinue1) = CommandContinue1 ()
 in (AST.SEQ(CommandContinue))
end)
 in ( LrTable.NT 4, ( result, LC1left, RC1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.CommandContinue CommandContinue1, _, 
CommandContinue1right)) :: _ :: ( _, ( MlyValue.Command Command1, 
Command1left, _)) :: rest671)) => let val  result = 
MlyValue.CommandContinue (fn _ => let val  (Command as Command1) = 
Command1 ()
 val  (CommandContinue as CommandContinue1) = CommandContinue1 ()
 in (Command ::CommandContinue)
end)
 in ( LrTable.NT 10, ( result, Command1left, CommandContinue1right), 
rest671)
end
|  ( 13, ( rest671)) => let val  result = MlyValue.CommandContinue (fn
 _ => ([]))
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 14, ( ( _, ( MlyValue.Exp Exp1, _, Exp1right)) :: _ :: ( _, ( 
MlyValue.Variable Variable1, Variable1left, _)) :: rest671)) => let
 val  result = MlyValue.Command (fn _ => let val  (Variable as 
Variable1) = Variable1 ()
 val  (Exp as Exp1) = Exp1 ()
 in (AST.SET(Variable,Exp))
end)
 in ( LrTable.NT 9, ( result, Variable1left, Exp1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.Variable Variable1, _, Variable1right)) :: 
( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Variable as Variable1) = Variable1
 ()
 in (AST.Read(Variable))
end)
 in ( LrTable.NT 9, ( result, READ1left, Variable1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.Exp Exp1, _, Exp1right)) :: ( _, ( _, 
WRITE1left, _)) :: rest671)) => let val  result = MlyValue.Command (fn
 _ => let val  (Exp as Exp1) = Exp1 ()
 in (AST.Write(Exp))
end)
 in ( LrTable.NT 9, ( result, WRITE1left, Exp1right), rest671)
end
|  ( 17, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.ComSeq 
ComSeq2, _, _)) :: _ :: ( _, ( MlyValue.ComSeq ComSeq1, _, _)) :: _ ::
 ( _, ( MlyValue.Exp Exp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671
)) => let val  result = MlyValue.Command (fn _ => let val  (Exp as 
Exp1) = Exp1 ()
 val  ComSeq1 = ComSeq1 ()
 val  ComSeq2 = ComSeq2 ()
 in (AST.ITE(Exp,ComSeq1,ComSeq2))
end)
 in ( LrTable.NT 9, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 18, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.ComSeq 
ComSeq1, _, _)) :: _ :: ( _, ( MlyValue.Exp Exp1, _, _)) :: ( _, ( _, 
WHILE1left, _)) :: rest671)) => let val  result = MlyValue.Command (fn
 _ => let val  (Exp as Exp1) = Exp1 ()
 val  (ComSeq as ComSeq1) = ComSeq1 ()
 in (AST.WH(Exp,ComSeq))
end)
 in ( LrTable.NT 9, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( 
MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  Exp1 = Exp1 ()
 val  Exp2 = Exp2 ()
 in (AST.PLUS(Exp1,Exp2))
end)
 in ( LrTable.NT 11, ( result, Exp1left, Exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( 
MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  Exp1 = Exp1 ()
 val  Exp2 = Exp2 ()
 in (AST.MINUS(Exp1,Exp2))
end)
 in ( LrTable.NT 11, ( result, Exp1left, Exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( 
MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  Exp1 = Exp1 ()
 val  Exp2 = Exp2 ()
 in (AST.TIMES(Exp1,Exp2))
end)
 in ( LrTable.NT 11, ( result, Exp1left, Exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( 
MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  Exp1 = Exp1 ()
 val  Exp2 = Exp2 ()
 in (AST.DIV(Exp1,Exp2))
end)
 in ( LrTable.NT 11, ( result, Exp1left, Exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( 
MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  Exp1 = Exp1 ()
 val  Exp2 = Exp2 ()
 in (AST.MOD(Exp1,Exp2))
end)
 in ( LrTable.NT 11, ( result, Exp1left, Exp2right), rest671)
end
|  ( 24, ( ( _, ( _, _, RR1right)) :: ( _, ( MlyValue.Exp Exp1, _, _))
 :: ( _, ( _, LR1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  (Exp as Exp1) = Exp1 ()
 in (Exp)
end)
 in ( LrTable.NT 11, ( result, LR1left, RR1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.Exp Exp1, _, Exp1right)) :: ( _, ( _, 
NEG1left, _)) :: rest671)) => let val  result = MlyValue.Exp (fn _ =>
 let val  (Exp as Exp1) = Exp1 ()
 in (AST.NEG(Exp))
end)
 in ( LrTable.NT 11, ( result, NEG1left, Exp1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( 
MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  Exp1 = Exp1 ()
 val  Exp2 = Exp2 ()
 in (AST.OR(Exp1,Exp2))
end)
 in ( LrTable.NT 11, ( result, Exp1left, Exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( 
MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  Exp1 = Exp1 ()
 val  Exp2 = Exp2 ()
 in (AST.AND(Exp1,Exp2))
end)
 in ( LrTable.NT 11, ( result, Exp1left, Exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.Exp Exp1, _, Exp1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.Exp (fn _ =>
 let val  (Exp as Exp1) = Exp1 ()
 in (AST.NOT(Exp))
end)
 in ( LrTable.NT 11, ( result, NOT1left, Exp1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( 
MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  Exp1 = Exp1 ()
 val  Exp2 = Exp2 ()
 in (AST.LT(Exp1,Exp2))
end)
 in ( LrTable.NT 11, ( result, Exp1left, Exp2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( 
MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  Exp1 = Exp1 ()
 val  Exp2 = Exp2 ()
 in (AST.LEQ(Exp1,Exp2))
end)
 in ( LrTable.NT 11, ( result, Exp1left, Exp2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( 
MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  Exp1 = Exp1 ()
 val  Exp2 = Exp2 ()
 in (AST.GT(Exp1,Exp2))
end)
 in ( LrTable.NT 11, ( result, Exp1left, Exp2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( 
MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  Exp1 = Exp1 ()
 val  Exp2 = Exp2 ()
 in (AST.GEQ(Exp1,Exp2))
end)
 in ( LrTable.NT 11, ( result, Exp1left, Exp2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( 
MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  Exp1 = Exp1 ()
 val  Exp2 = Exp2 ()
 in (AST.EQ(Exp1,Exp2))
end)
 in ( LrTable.NT 11, ( result, Exp1left, Exp2right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( 
MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  Exp1 = Exp1 ()
 val  Exp2 = Exp2 ()
 in (AST.NEQ(Exp1,Exp2))
end)
 in ( LrTable.NT 11, ( result, Exp1left, Exp2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.ExpTerm ExpTerm1, ExpTerm1left, 
ExpTerm1right)) :: rest671)) => let val  result = MlyValue.Exp (fn _
 => let val  (ExpTerm as ExpTerm1) = ExpTerm1 ()
 in (AST.Factor(ExpTerm))
end)
 in ( LrTable.NT 11, ( result, ExpTerm1left, ExpTerm1right), rest671)

end
|  ( 36, ( ( _, ( MlyValue.Numeral Numeral1, Numeral1left, 
Numeral1right)) :: rest671)) => let val  result = MlyValue.ExpTerm (fn
 _ => let val  (Numeral as Numeral1) = Numeral1 ()
 in (Numeral)
end)
 in ( LrTable.NT 12, ( result, Numeral1left, Numeral1right), rest671)

end
|  ( 37, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = MlyValue.ExpTerm
 (fn _ => let val  (Variable as Variable1) = Variable1 ()
 in (Variable)
end)
 in ( LrTable.NT 12, ( result, Variable1left, Variable1right), rest671
)
end
|  ( 38, ( ( _, ( MlyValue.True True1, True1left, True1right)) :: 
rest671)) => let val  result = MlyValue.ExpTerm (fn _ => let val  (
True as True1) = True1 ()
 in (True)
end)
 in ( LrTable.NT 12, ( result, True1left, True1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.False False1, False1left, False1right)) :: 
rest671)) => let val  result = MlyValue.ExpTerm (fn _ => let val  (
False as False1) = False1 ()
 in (False)
end)
 in ( LrTable.NT 12, ( result, False1left, False1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : While_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun Identifier (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.Identifier (fn () => i),p1,p2))
fun Numeral (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.Numeral (fn () => i),p1,p2))
fun LC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun RC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun LR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun RR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SemiColon (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun Add (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun Sub (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun Mul (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun Div (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun Mod (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun True (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.True (fn () => i),p1,p2))
fun False (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.False (fn () => i),p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun Assign (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun Comma (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun DoubleColon (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun SingleColon (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ProgramStart (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
