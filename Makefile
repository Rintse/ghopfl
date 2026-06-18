GRAMMER_FILES = Abs.hs ErrM.hs Lex.x Par.y Print.hs Skel.hs
UNNEEDED = Test.hs Doc.txt *.bak

exp_files = $(addprefix src/Syntax/Exp/,$(GRAMMER_FILES))
types_files = $(addprefix src/Syntax/Types/,$(GRAMMER_FILES))

exp_unneeded = $(addprefix src/Syntax/Exp/,$(UNNEEDED))
types_unneeded = $(addprefix src/Syntax/Types/,$(UNNEEDED))

grammar: $(exp_files) $(types_files)

$(exp_files): src/exp.bnf
	bnfc -p Syntax -o src -d $<
	rm -f $(exp_unneeded)

$(types_files): src/types.bnf
	bnfc -p Syntax -o src -d $<
	rm -f $(types_unneeded)

clean:
	rm -f $(exp_files) $(types_files)
