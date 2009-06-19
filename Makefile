CC = ghc
FLAGS = 
PROF = #-prof -auto-all
PACKAGES = -package binary -package fgl

all: MUB-Table-exact MUB-Table-approx

ExtRat: ExtRat.hs
	$(CC) -c ExtRat.hs $(FLAGS) $(PACKAGES) $(PROF)

ExtCpx: ExtCpx.hs
	$(CC) -c ExtCpx.hs $(FLAGS) $(PACKAGES) $(PROF)

Perms: Perms.hs
	$(CC) -c Perms.hs $(FLAGS) $(PACKAGES) $(PROF)

ListGen: ListGen.hs
	$(CC) -c ListGen.hs $(FLAGS) $(PACKAGES) $(PROF)


BSet: BSet.hs ExtRat ExtCpx Perms ListGen
	$(CC) -o BSet BSet.hs ExtRat.o ExtCpx.o Perms.o ListGen.o $(FLAGS) $(PACKAGES) $(PROF)


MUB-Table-exact: MUB-Table-exact.hs ExtRat ExtCpx
	$(CC) -o MUB-Table-exact MUB-Table-exact.hs ExtRat.o ExtCpx.o $(FLAGS) $(PACKAGES) $(PROF)

MUB-Table-approx: MUB-Table-approx.hs
	$(CC) -o MUB-Table-approx MUB-Table-approx.hs $(FLAGS) $(PACKAGES) $(PROF)



MUB-Search: MUB-Search.hs
	$(CC) -o MUB-Search MUB-Search.hs $(FLAGS)

clean:
	rm MUB-Table-exact MUB-Table-approx MUB-Search BSet *.hi *.o *.prof
