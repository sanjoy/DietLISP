.PHONY: all clean

dlisp: src/DietLISP.hs src/Builtins.hs src/Tokenizer.hs src/Utils.hs src/Domain.hs src/Parser.hs src/Semantics.hs
	ghc -isrc -outputdir .objs -o $@ --make $<

all:: dlisp

clean:
	$(RM) -r .objs
	$(RM) dlisp
