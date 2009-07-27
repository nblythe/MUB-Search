# MUB-Search
#
# 2009 Nathan Blythe, Dr. Oscar Boykin


# Flags.
#
# CC is the Haskell compiler to use.
# FLAGS are any additional command line flags passed to each compilation.
# PROF is for any profiling information.
# PACKAGES is for compiling against packages that need explicit reference.
#
CC = ghc
FLAGS = 
PROF = #-prof -auto-all
PACKAGES = -package binary



# Operations.
#
all: Gen-AdjFunctions Gen-Bases MUB-Search

clean:
	rm -f Gen-AdjFunctions Gen-Bases MUB-Search *.hi *.o *.prof



# Modules used to construct and manipulate roots of unity.
#
Cyclotomic24: Cyclotomic24.hs
	$(CC) -c Cyclotomic24.hs $(FLAGS) $(PACKAGES) $(PROF)

Roots12: Roots12.hs Cyclotomic24
	$(CC) -c Roots12.hs $(FLAGS) $(PACKAGES) $(PROF)

Roots24: Roots24.hs Cyclotomic24
	$(CC) -c Roots24.hs $(FLAGS) $(PACKAGES) $(PROF)



# Modules used to manipulate vectors, lists of vectors, and graphs of vectors.
#
SublistPred: SublistPred.hs
	$(CC) -c SublistPred.hs $(FLAGS) $(PACKAGES) $(PROF)

Magic: Magic.hs
	$(CC) -c Magic.hs $(FLAGS) $(PACKAGES) $(PROF)

Graph: Graph.hs Magic
	$(CC) -c Graph.hs $(FLAGS) $(PACKAGES) $(PROF)



# Top-level modules that produce executables.
#
Gen-AdjFunctions: Gen-AdjFunctions.hs Cyclotomic24 Roots12 Roots24 SublistPred Magic
	$(CC) -o Gen-AdjFunctions Gen-AdjFunctions.hs Cyclotomic24.o Roots12.o Roots24.o SublistPred.o Magic.o $(FLAGS) $(PACKAGES) $(PROF)

Gen-Bases: Gen-Bases.hs Graph Magic
	$(CC) -o Gen-Bases Gen-Bases.hs Graph.o Magic.o $(FLAGS) $(PACKAGES) $(PROF)

MUB-Search: MUB-Search.hs Graph Magic
	$(CC) -o MUB-Search MUB-Search.hs Graph.o Magic.o $(FLAGS) $(PACKAGES) $(PROF)



# To be pruned.
#
Perms: Perms.hs
	$(CC) -c Perms.hs $(FLAGS) $(PACKAGES) $(PROF)

Analyze-Bases: Analyze-Bases.hs Perms
	$(CC) -o Analyze-Bases Analyze-Bases.hs Perms.o $(FLAGS) $(PACKAGES) $(PROF)

Validate-Bases: Validate-Bases.hs Graph Magic Perms
	$(CC) -o Validate-Bases Validate-Bases.hs Graph.o Magic.o Perms.o $(FLAGS) $(PACKAGES) $(PROF)

