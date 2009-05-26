CC = ghc
FLAGS =

all: MUB-Table-exact MUB-Table-approx MUB-Search

MUB-Table-exact: MUB-Table-exact.hs
	$(CC) -o MUB-Table-exact MUB-Table-exact.hs $(FLAGS)

MUB-Table-approx: MUB-Table-approx.hs
	$(CC) -o MUB-Table-approx MUB-Table-approx.hs $(FLAGS)

MUB-Search: MUB-Search.hs
	$(CC) -o MUB-Search MUB-Search.hs $(FLAGS)

clean:
	rm MUB-Table-exact MUB-Table-approx MUB-Search *.hi *.o
