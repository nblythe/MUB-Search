CC = ghc
FLAGS =

MUB-Table-exact: MUB-Table-exact.hs
	$(CC) -o MUB-Table-exact MUB-Table-exact.hs $(FLAGS)

MUB-Table-approx: MUB-Table-approx.hs
	$(CC) -o MUB-Table-approx MUB-Table-approx.hs $(FLAGS)

clean:
	rm MUB-Table-exact MUB-Table-approx *.hi *.o

