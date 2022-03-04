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
%%
%name While

%term
    Identifier of string | Numeral of string | LC| RC| LR| RR| SemiColon
    | LT | GT | LEQ | GEQ | NEQ | EQ | Add | Sub | Mul | Div| Mod | NEG
    |True of string| False of string  | OR | AND | NOT | Assign | Comma | BOOL | INT 
    |VAR | DoubleColon | SingleColon | ProgramStart | READ | WRITE | IF | THEN | ELSE 
    |ENDIF | WHILE | DO | ENDWH | EOF  

%nonterm 
    Start of AST.AST | Program of AST.AST | Block of AST.block  | DecSeq of (AST.declaration list)
    |ComSeq of AST.comseq | Declaration of  AST.declaration| Type of AST.typ | VariableList of (AST.variable list)
    |Variable of AST.variable | Command of AST.command | CommandContinue of (AST.command list) | Exp of AST.express
    | ExpTerm of AST.term 

%pos int
%eop EOF
%noshift EOF
%left LT GT LEQ GEQ EQ NEQ  
%left NOT OR AND 
%left Add Sub
%left Mul
%left Mod
%left Div 
%left NEG 
%start Start
%verbose
%%

Start : Program         (Program)
Program : ProgramStart Identifier DoubleColon Block     (AST.PROG (Identifier,Block))
Block : DecSeq ComSeq   (AST.BLK(DecSeq,ComSeq))
DecSeq : Declaration DecSeq     (Declaration :: DecSeq)
        | ([])
Declaration : VAR VariableList SingleColon Type SemiColon       (AST.DEC(VariableList,Type))
Type : INT      (AST.INT) 
        | BOOL  (AST.BOOL)
VariableList : Variable Comma VariableList      (Variable :: VariableList)
                | Variable      ([Variable])
Variable : Identifier   (Identifier)
ComSeq :  LC  CommandContinue RC       (AST.SEQ(CommandContinue))
CommandContinue : Command SemiColon CommandContinue     (Command ::CommandContinue)
                | ([])
Command : Variable Assign Exp (AST.SET(Variable,Exp))
        | READ Variable (AST.Read(Variable))
        | WRITE Exp  (AST.Write(Exp))
        | IF Exp THEN ComSeq ELSE ComSeq ENDIF  (AST.ITE(Exp,ComSeq1,ComSeq2))
        | WHILE Exp DO ComSeq ENDWH     (AST.WH(Exp,ComSeq))

Exp : Exp Add Exp       (AST.PLUS(Exp1,Exp2))
        | Exp Sub Exp   (AST.MINUS(Exp1,Exp2))
        |Exp Mul Exp    (AST.TIMES(Exp1,Exp2))
        |Exp Div Exp    (AST.DIV(Exp1,Exp2))
        |Exp Mod Exp    (AST.MOD(Exp1,Exp2))
        |LR Exp RR      (Exp)
        |NEG Exp        (AST.NEG(Exp))
        | Exp OR Exp    (AST.OR(Exp1,Exp2))
        | Exp AND Exp   (AST.AND(Exp1,Exp2))
        | NOT Exp       (AST.NOT(Exp))
        |Exp LT Exp (AST.LT(Exp1,Exp2))
        | Exp LEQ Exp   (AST.LEQ(Exp1,Exp2))
        | Exp GT Exp    (AST.GT(Exp1,Exp2))
        | Exp GEQ Exp   (AST.GEQ(Exp1,Exp2))
        | Exp EQ Exp    (AST.EQ(Exp1,Exp2))
        | Exp NEQ Exp   (AST.NEQ(Exp1,Exp2))
        | ExpTerm       (AST.Factor(ExpTerm))
ExpTerm : Numeral        (Numeral)
        | Variable      (Variable)
        | True          (True)
        | False         (False)
