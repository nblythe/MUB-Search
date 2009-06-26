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
PROF = -prof -auto-all
PACKAGES = -package binary



# Operations.
#
all: Analyze-Bases Gen-VecTables MUB-Search Old-Search

clean:
	rm -f Analyze-Bases Gen-VecTables MUB-Search Old-Search *.hi *.o *.prof



# Modules used to construct and manipulate roots of unity.
#
ExtRat: ExtRat.hs
	$(CC) -c ExtRat.hs $(FLAGS) $(PACKAGES) $(PROF)

ExtCpx: ExtCpx.hs
	$(CC) -c ExtCpx.hs $(FLAGS) $(PACKAGES) $(PROF)

Roots12: Roots12.hs ExtRat ExtCpx
	$(CC) -c Roots12.hs $(FLAGS) $(PACKAGES) $(PROF)

Roots24: Roots24.hs ExtRat ExtCpx
	$(CC) -c Roots24.hs $(FLAGS) $(PACKAGES) $(PROF)



# Modules used to manipulate vectors and lists of vectors.
#
SubsetPred: SubsetPred.hs
	$(CC) -c SubsetPred.hs $(FLAGS) $(PACKAGES) $(PROF)

Magic: Magic.hs
	$(CC) -c Magic.hs $(FLAGS) $(PACKAGES) $(PROF)

VecTables: VecTables.hs Roots12 Roots24 SubsetPred Magic
	$(CC) -c VecTables.hs $(FLAGS) $(PACKAGES) $(PROF)

Perms: Perms.hs
	$(CC) -c Perms.hs $(FLAGS) $(PACKAGES) $(PROF)



# Modules used to manipulate graphs.
#
GenOrthGraph: GenOrthGraph.hs
	$(CC) -c GenOrthGraph.hs $(FLAGS) $(PACKAGES) $(PROF)

FindCliques: FindCliques.hs
	$(CC) -c FindCliques.hs $(FLAGS) $(PACKAGES) $(PROF)



# Top-level modules that produce executables.
#
Analyze-Bases: Analyze-Bases.hs Perms
	$(CC) -o Analyze-Bases Analyze-Bases.hs Perms.o $(FLAGS) $(PACKAGES) $(PROF)

Gen-VecTables: Gen-VecTables.hs VecTables
	$(CC) -o Gen-VecTables Gen-VecTables.hs VecTables.o ExtRat.o ExtCpx.o Roots12.o Roots24.o SubsetPred.o Magic.o $(FLAGS) $(PACKAGES) $(PROF)

MUB-Search: MUB-Search.hs GenVectorTable12 GenVectorTable24 GenOrthGraph FindCliques
	$(CC) -o MUB-Search MUB-Search.hs GenVectorTable12.o GenVectorTable24.o GenOrthGraph.o FindCliques.o ExtRat.o ExtCpx.o Roots12.o Roots24.o $(FLAGS) $(PACKAGES) $(PROF)

Old-Search: Old-Search.hs Magic
	$(CC) -o Old-Search Old-Search.hs Magic.o $(FLAGS) $(PACKAGES) $(PROF)

