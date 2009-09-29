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
FLAGS = #-static -optl-static -optl-pthread
PROF = -prof -auto-all
PACKAGES = -package binary



# Operations.
#
all: FundamentalNeighbors Bases CombBases CheckFourierFamily MUBs2LaTeX MUB-Search MakeMagic

clean:
	rm -f FundamentalNeighbors Bases CombBases CheckFourierFamily MUBs2LaTeX MUB-Search *.hi *.o *.prof

push:
	git push git@github.com:nblythe/MUB-Search.git master


# Modules used to construct and manipulate roots of unity.
#
Cyclotomic: Cyclotomic.hs
	$(CC) -c Cyclotomic.hs $(FLAGS) $(PACKAGES) $(PROF)

Cyclotomic24: Cyclotomic24.hs
	$(CC) -c Cyclotomic24.hs $(FLAGS) $(PACKAGES) $(PROF)

Roots12: Roots12.hs Cyclotomic24
	$(CC) -c Roots12.hs $(FLAGS) $(PACKAGES) $(PROF)

Roots24: Roots24.hs Cyclotomic24
	$(CC) -c Roots24.hs $(FLAGS) $(PACKAGES) $(PROF)



# Modules used to manipulate vectors, matrices, and graphs.
#
Perms: Perms.hs
	$(CC) -c Perms.hs $(FLAGS) $(PACKAGES) $(PROF)

SublistPred: SublistPred.hs
	$(CC) -c SublistPred.hs $(FLAGS) $(PACKAGES) $(PROF)

Magic: Magic.hs
	$(CC) -c Magic.hs $(FLAGS) $(PACKAGES) $(PROF)

Graph: Graph.hs Magic
	$(CC) -c Graph.hs $(FLAGS) $(PACKAGES) $(PROF)



# Top-level modules that produce executables.
#
FundamentalNeighbors: FundamentalNeighbors.hs Cyclotomic SublistPred Magic
	$(CC) -o FundamentalNeighbors FundamentalNeighbors.hs Cyclotomic.o SublistPred.o Magic.o $(FLAGS) $(PACKAGES) $(PROF)

Bases: Bases.hs Graph Magic
	$(CC) -o Bases Bases.hs Graph.o Magic.o $(FLAGS) $(PACKAGES) $(PROF)

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

