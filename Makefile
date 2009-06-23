CC = ghc
FLAGS = 
PROF = #-prof -auto-all
PACKAGES = -package binary

all: MUB-Search



ExtRat: ExtRat.hs
	$(CC) -c ExtRat.hs $(FLAGS) $(PACKAGES) $(PROF)

ExtCpx: ExtCpx.hs
	$(CC) -c ExtCpx.hs $(FLAGS) $(PACKAGES) $(PROF)

Perms: Perms.hs
	$(CC) -c Perms.hs $(FLAGS) $(PACKAGES) $(PROF)



GenVectorTable12: GenVectorTable12.hs ExtRat ExtCpx
	$(CC) -c GenVectorTable12.hs $(FLAGS) $(PACKAGES) $(PROF)

GenVectorTable24: GenVectorTable24.hs ExtRat ExtCpx
	$(CC) -c GenVectorTable24.hs $(FLAGS) $(PACKAGES) $(PROF)

GenOrthGraph: GenOrthGraph.hs
	$(CC) -c GenOrthGraph.hs $(FLAGS) $(PACKAGES) $(PROF)

FindCliques: FindCliques.hs
	$(CC) -c FindCliques.hs $(FLAGS) $(PACKAGES) $(PROF)



MUB-Search: MUB-Search.hs GenVectorTable12 GenVectorTable24 GenOrthGraph FindCliques
	$(CC) -o MUB-Search MUB-Search.hs GenVectorTable12.o GenVectorTable24.o GenOrthGraph.o FindCliques.o ExtRat.o ExtCpx.o $(FLAGS) $(PACKAGES) $(PROF)



clean:
	rm -f MUB-Search *.hi *.o *.prof

