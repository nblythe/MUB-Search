CC = ghc
FLAGS = 
PROF = -prof -auto-all
PACKAGES = -package binary

all: MUB-Table-exact MUB-Table-approx



ExtRat: ExtRat.hs
	$(CC) -c ExtRat.hs $(FLAGS) $(PACKAGES) $(PROF)

ExtCpx: ExtCpx.hs
	$(CC) -c ExtCpx.hs $(FLAGS) $(PACKAGES) $(PROF)

Perms: Perms.hs
	$(CC) -c Perms.hs $(FLAGS) $(PACKAGES) $(PROF)



GenVectorTable12: GenVectorTable12.hs ExtRat ExtCpx
	$(CC) -o GenVectorTable12 GenVectorTable12.hs ExtRat.o ExtCpx.o $(FLAGS) $(PACKAGES) $(PROF)

GenVectorTable24: GenVectorTable24.hs ExtRat ExtCpx
	$(CC) -o GenVectorTable24 GenVectorTable24.hs ExtRat.o ExtCpx.o $(FLAGS) $(PACKAGES) $(PROF)

GenOrthGraph: GenOrthGraph.hs
	$(CC) -o GenOrthGraph GenOrthGraph.hs $(FLAGS) $(PACKAGES) $(PROF)

FindCliques: FindCliques.hs
	$(CC) -o FindCliques FindCliques.hs $(FLAGS) $(PACKAGES) $(PROF)



clean:
	rm GenVectorTable12 GenVectorTable24 GenOrthGraph FindCliques *.hi *.o *.prof

