all:
	happy -gca ParCPP.y
	alex -g LexCPP.x
	ghc --make TestCPP.hs TypeCheck.hs -o TestCPP -fwarn-incomplete-patterns

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocCPP.ps

distclean: clean
	-rm -f DocCPP.* LexCPP.* ParCPP.* LayoutCPP.* SkelCPP.* PrintCPP.* TestCPP.* AbsCPP.* TestCPP ErrM.* SharedString.* ComposOp.* CPP.dtd XMLCPP.* Makefile*
	

