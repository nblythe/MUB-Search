# MUB-Search
#
# 2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)


# Flags.
#
# CC is the Haskell compiler to use.
# FLAGS are any additional command line flags passed to each compilation.
# PROF is for any profiling information.
# PACKAGES is for compiling against packages that need explicit reference.
#
CC = ghc
FLAGS = -static -optl-static -optl-pthread
PROF = #-prof -auto-all
PACKAGES = -package binary



# Operations.
#
all: FundamentalNeighbors Bases CombBases CheckFourierFamily MUBs2LaTeX MUB-Search MakeMagic HNSS

clean:
	rm -f FundamentalNeighbors Bases CombBases CheckFourierFamily MUBs2LaTeX MUB-Search MakeMagic HNSS *.hi *.o *.prof *.aux *.hp *.ps

push:
	git push git@github.com:nblythe/MUB-Search.git master



# Modules that provide specific encapsulated functionalities.
#
Cyclotomic: Cyclotomic.hs
	$(CC) -c Cyclotomic.hs $(FLAGS) $(PACKAGES) $(PROF)

Perms: Perms.hs
	$(CC) -c Perms.hs $(FLAGS) $(PACKAGES) $(PROF)

Combinadics: Combinadics.hs
	$(CC) -c Combinadics.hs $(FLAGS) $(PACKAGES) $(PROF)

SublistPred: SublistPred.hs Combinadics
	$(CC) -c SublistPred.hs $(FLAGS) $(PACKAGES) $(PROF)

Magic: Magic.hs
	$(CC) -c Magic.hs $(FLAGS) $(PACKAGES) $(PROF)

Magic2: Magic2.hs
	$(CC) -c Magic2.hs $(FLAGS) $(PACKAGES) $(PROF)

Graph: Graph.hs Magic
	$(CC) -c Graph.hs $(FLAGS) $(PACKAGES) $(PROF)

Graph2: Graph2.hs Magic2
	$(CC) -c Graph2.hs $(FLAGS) $(PACKAGES) $(PROF)

Polynomial: Polynomial.hs
	$(CC) -c Polynomial.hs $(FLAGS) $(PACKAGES) $(PROF)


# Top-level modules that produce executables.
#
DumpBinaryAdjacencies: DumpBinaryAdjacencies.hs
	$(CC) -o DumpBinaryAdjacencies DumpBinaryAdjacencies.hs $(FLAGS) $(PACKAGES) $(PROF)

Uniquify: Uniquify.hs Magic2 Combinadics
	$(CC) -o Uniquify Uniquify.hs Magic2.o Combinadics.o $(FLAGS) $(PACKAGES) $(PROF)

FundamentalNeighbors: FundamentalNeighbors.hs Cyclotomic SublistPred Combinadics Magic Perms
	$(CC) -o FundamentalNeighbors FundamentalNeighbors.hs Cyclotomic.o SublistPred.o Combinadics.o Magic.o Perms.o $(FLAGS) $(PACKAGES) $(PROF)

Bases: Bases.hs Graph Magic
	$(CC) -o Bases Bases.hs Graph.o Magic.o $(FLAGS) $(PACKAGES) $(PROF)

Bases2: Bases2.hs Graph2 Magic2
	$(CC) -o Bases2 Bases2.hs Graph2.o Magic2.o $(FLAGS) $(PACKAGES) $(PROF)

CombBases: CombBases.hs Magic
	$(CC) -o CombBases CombBases.hs Magic.o $(FLAGS) $(PACKAGES) $(PROF)

CheckFourierFamily: CheckFourierFamily.hs Magic Perms
	$(CC) -o CheckFourierFamily CheckFourierFamily.hs Magic.o Perms.o $(FLAGS) $(PACKAGES) $(PROF)

MUBs2LaTeX: MUBs2LaTeX.hs Magic
	$(CC) -o MUBs2LaTeX MUBs2LaTeX.hs Magic.o $(FLAGS) $(PACKAGES) $(PROF)

MUB-Search: MUB-Search.hs Graph Magic
	$(CC) -o MUB-Search MUB-Search.hs Graph.o Magic.o $(FLAGS) $(PACKAGES) $(PROF)

MakeMagic: MakeMagic.hs Magic
	$(CC) -o MakeMagic MakeMagic.hs Magic.o $(FLAGS) $(PACKAGES) $(PROF)

HNSS: HNSS.hs Polynomial
	$(CC) -o HNSS HNSS.hs Polynomial.o $(FLAGS) $(PACKAGES) $(PROF)


