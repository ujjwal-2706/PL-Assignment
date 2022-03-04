# COL226 Assignment 3 : Abstract Syntax Trees (AST) for WHILE
- Name : Ujjwal Mehta
- Entry No. - 2020CS10401
### Overview of File Structure
- For This assignment I have created while_ast.sml file(This is our main file in which the datatypes definations, constructors as well as lexer and parser our 
connected), while.lex file(This file contains the ml-lex code to create tokens), while.yacc file(This file contains our implemented grammar production rules as well as the semantics), loader.sml (This will directly open the terminal and load our sml files once make all command is run), makefile ( for compiling our files to create lexer and yacc files).

### Running the Parser
- First Place while_ast.sml , while.yacc, while.lex, makefile, loader.sml in the same directory along with the testfiles containing the while language code
- open the terminal and go that directory where the files are placed
- run make all command ( This will automatically enter the sml terminal)
- use "parser(filename);" and hit enter to create the AST of the program in file filename

## Context-free grammar
- In this section I will define the grammar production rules which I have used in my while.yacc file to generate the abstract syntax tree
- The Terminals of this grammar are the ones in the string given below in the productions as well as Identifier and Numeral
- Rest of the symbols are non terminals of the grammar
- It should be noted that the ambiguity of this grammar has been taken care of by using %left attribute of ml-yacc and that's why the grammar is defined in this ambiguous fashion (the Idea was taken from C language grammar which I have mentioned in the Acknowledgements section)
- The production rules are as follows :-
- Start -> Program 
- Program -> "program" Identifier "::" Block
- Block -> DeclarationSequence CommandSequence
- DeclarationSequence -> Declaration DeclarationSequence | epsilon
- Declaration -> "var" VariableList ":" Type ";"
- Type -> "int"| "bool"
- VariableList -> Variable "," VariableList | Variable
- Variable -> Identifier
- CommandSequence -> "{" CommandContinue "}"
- CommandContinue -> Command ";" CommandContinue | epsilon
- Command -> Variable ":=" Exp | "read" Variable | "write" Exp | "if" Exp "then" CommandSequence "else" CommandSequence "endif" | "while" Exp "do" CommandSequence "endwh"
- Exp -> Exp "+" Exp | Exp "-" Exp |Exp "*" Exp |Exp "/" Exp |Exp "%" Exp |"(" Exp ")" | "~" Exp| Exp "||" Exp| Exp "&&" Exp| "!" Exp |Exp "<" Exp| Exp "<=" Exp | Exp ">" Exp| Exp ">=" Exp| Exp "=" Exp| Exp "<>" Exp| ExpTerm
- ExpTerm -> Numeral| Variable| "tt"| "ff"

## AST datatype definition
- The defination of the datatypes along with the constructor defination are specified clearly in the while_ast.sml file in the first few lines
- The defination are as follows :- 
- type variable = string
- type term =  string
- datatype typ = INT|BOOL
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
- These datatypes have been used to generate AST by passing arguments in the constructor (on the left side of "of" keyword like PLUS) and will give types to the non terminals of our yacc file and these are related as :- 
- AST -> this is the type of Start and Program non terminals
- block -> this is the type of Block non termnial
- declaration list -> this is the type of DeclarationSequence non termnial
- comseq -> this is the type of CommandSequence non termnial
- declaration -> this is the type of Declaration non termnial
- typ -> this is the type of Type non termnial
- variable list-> this is the type of VariableList non termnial
- variable -> this is the type of Variable non termnial
- command -> this is the type of Command non termnial
- command list -> this is the type of CommandContinue non terminal
- express -> this is the type of the Exp non terminal
- term -> this is the type of the ExpTerm non terminal

## Syntax-directed translation
- In this section I will explain the semantics which I used in my while.yacc file to create the AST by sending the attributes via the constructor of the data type
- We will think of our semantics in a recursive fashion hence we will define the rules in top to bottom manner
- Start is there initially as the only non terminal in our AST hence it will take the value of attribute from its chidren Program
- then Program will use the constructor PROG to create a tree structure with Identifier and Block as its child nodes
- In a similar way we will break Block into DeclarationSequence and CommandSequence( Here it should be noted that we will be maintaining our DeclarationSequence and CommandSequence as a list (which we defined earlier in our AST datatype definations) and these list will be the children of Block which will be made via the BLK constructor of the AST structure)
- Similarly then we will fetch the attributes of Declaration by maintaining the variables in form of a list then storing this list as a child of Declaration and the Type as the other child which we will call using the DEC constructor
- In a similar fashion will maintain the list of commands as a child of our CommandSequence(though non terminals will not be shown in the AST, it will be used to send the attributes to the parent) in our AST
- Finally each Command will form its own subtree the attribute of which will depend on the type of statement used in the command ( which can If then else , while do , assignment,etc) and then we will call that particular constructor for the attributes( like SET, ITE , etc)
- The attributes of the Exp are simply sent to top of AST by calling that particular constructor (like PLUS, MINUS,etc)

## Auxiliary functions and Data
- I have used 1 auxiliary datatype constructor inside my while.yacc file in order to properly assign attributes like FACTOR constructor will simply make the node containing Indentifier or Numeral or "tt" or "ff".

## Other Design Decisions (Important)
- It should be noted that while designing my parser I have taken the Numeral token to be either something like +23 or ~17 or only 12 as well hence if we are using "+" in an expression of the program then there should be space between "+" and numeral in order to treat "+" as an operator else it would be treated as a single token along with the Numeral.

# Acknowledgements
- In this particular section I am mentioning the sources from where I got the ideas of both implementation as well as designing of this parser along with the resources I used in order to understand and implement ml-lex and ml-yacc to create a parser.
- https://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/manual.html
- http://rogerprice.org/ug/ug.pdf
- http://cs.wellesley.edu/~cs235/fall08/lectures/35_YACC_revised.pdf
- http://marvin.cs.uidaho.edu/Teaching/CS445/c-Grammar.pdf
- https://whynottrythis.wordpress.com/2011/01/24/sml-and-ml-lex-tutorial/
- https://www.smlnj.org/doc/ML-Lex/manual.html
