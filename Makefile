.PHONY: all clean

dlisp: src/DietLISP.hs src/Origin.hs src/Parser.hs src/DLTokenizer.hs src/Semantics.hs src/REPL.hs src/Utils.hs
	ghc -isrc -outputdir .objs -o $@ --make $<

all:: dlisp

clean:
	$(RM) -r .objs
	$(RM) dlisp
