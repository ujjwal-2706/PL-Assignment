## COL226 Assignment 4 : The VMC machine for AST of While

## Name : Ujjwal Mehta

## Entry No. : 2020CS10401

### Overview of the assignment 

- In this assignment we have created the evaluator of any program of the while language whose AST was produced in the previous assignment
- In order to evaluate the AST we first find the postfix expression of the AST which is found out using the postorder traversal of the AST that was produced using the parser function inside "while_ast.sml" file
- We also implemented all the functions inside the FunStack structure which are present inside the "stack.sml" file
- Finally we defined the VMC signature which contains our 4 main important functions that are **rules** , **execute** , **postfix** and **toString** the role of which is given in the assignment
- We defined the memory as an array of 101 int elements with values initialized to 0 defined **globally** inside the structure **Vmc** named as **M** whereas the stacks **V** and **C** will be defined **locally** whenever a file is called to execute function
- It should be also noted that I have defined a function named **symbolTable** which will take the file name containing the while program and a list containg tuples of identifiers, their types and their value index to the memory M hence in my design the typechecking will happen during the run time
- I have also defined my own datatype named **stackuse** which will be used to create the control stack C whereas the V stack would be the stack of strings
- In order to define the rules function I have defined several functions named as changeStackx which will apply the semantics depending on whats at the top of the control stack
- Finally execute will recursively call rules until the control stack is empty in order to run the postfix expression

### Design Caution to be kept while writing the while program

- It should be noted that while designing my parser I have taken the Numeral token to be either something like +23 or ~17 or only 12 as well hence if we are using "+" in an expression of the program then there should be space between "+" and numeral in order to treat "+" as an operator else it would be treated as a single token along with the Numeral

### Input and Output of rules, execute, postfix, toString

- The input of **rules ** is value stack , control stack and the symbol table and it outputs the reduced value stack, control stack
- The input of **execute** is the filename containing the while program and it outputs the tuple of final value stack, final memory array and final control stack
- The input of **postfix** is the filename and it outputs the control stack for that file
- The input of **toString** is the value stack and the control stack and it outputs the tuple of string list of value stack, int array of memory(where the values are in the order in which variables were defined) and string list of the control stack

### FileStructure in Submission as well as running the program

- Our main signature of **VMC**  and the structure **Vmc** are implemented inside the "while_ast.sml" file
- The signature **Stack** and the structure **FunStack** are implemented inside the "stack.sml" file
- The datatypes of AST are defined inside the "datatype.sml" file
- The other files are the lex and yacc file, loader.sml and makefile

#### Running the files

- In order to run the files, put the while program files inside the directory containing the above files
- Open the terminal and use the **"make all"** command to compile all the files, this will open the sml terminal
- Then use the appropriate function defined above as per the convenience 