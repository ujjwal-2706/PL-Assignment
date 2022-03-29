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
