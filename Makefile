CC = ghc
FLAGS =

all: MUB-Table-exact MUB-Table-approx MUB-Search

ExtRat: ExtRat.hs
	$(CC) -c ExtRat.hs $(FLAGS)

ExtCpx: ExtCpx.hs
	$(CC) -c ExtCpx.hs $(FLAGS)

MUB-Table-exact: MUB-Table-exact.hs ExtRat ExtCpx
	$(CC) -o MUB-Table-exact MUB-Table-exact.hs ExtRat.o ExtCpx.o $(FLAGS)

MUB-Table-approx: MUB-Table-approx.hs
	$(CC) -o MUB-Table-approx MUB-Table-approx.hs $(FLAGS)

MUB-Search: MUB-Search.hs
	$(CC) -o MUB-Search MUB-Search.hs $(FLAGS)

clean:
	rm MUB-Table-exact MUB-Table-approx MUB-Search *.hi *.o
