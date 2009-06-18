CC = ghc
FLAGS = -prof -auto-all -package binary

all: MUB-Table-exact MUB-Table-approx

ExtRat: ExtRat.hs
	$(CC) -c ExtRat.hs $(FLAGS)

ExtCpx: ExtCpx.hs
	$(CC) -c ExtCpx.hs $(FLAGS)

Perms: Perms.hs
	$(CC) -c Perms.hs $(FLAGS)

ListGen: ListGen.hs
	$(CC) -c ListGen.hs $(FLAGS)


BSet: BSet.hs ExtRat ExtCpx Perms ListGen
	$(CC) -o BSet BSet.hs ExtRat.o ExtCpx.o Perms.o ListGen.o $(FLAGS)


MUB-Table-exact: MUB-Table-exact.hs ExtRat ExtCpx
	$(CC) -o MUB-Table-exact MUB-Table-exact.hs ExtRat.o ExtCpx.o $(FLAGS)

MUB-Table-approx: MUB-Table-approx.hs
	$(CC) -o MUB-Table-approx MUB-Table-approx.hs $(FLAGS)

MUB-Search: MUB-Search.hs
	$(CC) -o MUB-Search MUB-Search.hs $(FLAGS)

clean:
	rm MUB-Table-exact MUB-Table-approx MUB-Search BSet *.hi *.o *.prof
