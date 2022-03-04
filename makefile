all:
	ml-lex while.lex
	ml-yacc while.yacc
	rlwrap sml loader.sml
clean:
	rm while.lex.sml while.yacc.desc while.yacc.sig while.yacc.sml