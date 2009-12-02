# MUB-Search
#
# 2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)


# Flags.
#
# CC is the Haskell compiler to use.
# FLAGS are any additional command line flags passed to each compilation.
# PROF is for any profiling information.
#
CC = ghc
FLAGS = -XFlexibleInstances -XFlexibleContexts -static -optl-static -optl-pthread
PROF = -prof -auto-all


# Operations.
#
all: utilities major

utilities: MUBs2LaTeX CheckFourierFamily EquivBases
clean_utilities:
	rm -f MUBs2LaTeX CheckFourierFamily EquivBases

major: FundamentalNeighbors Simplify MUB-Search HNSS
clean_major:
	rm -f FundamentalNeighbors Simplify MUB-Search HNSS

clean: clean_utilities clean_major
	rm -f *.hi *.o *.prof *.aux *.hp *.ps

commit:
	git commit -a

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

Polynomial: Polynomial.hs
	$(CC) -c Polynomial.hs $(FLAGS) $(PACKAGES) $(PROF)

Cliques: Cliques.hs
	$(CC) -c Cliques.hs $(FLAGS) $(PACKAGES) $(PROF)


# Small utilities.
#
MUBs2LaTeX: MUBs2LaTeX.hs
	$(CC) -o MUBs2LaTeX MUBs2LaTeX.hs $(FLAGS) $(PACKAGES) $(PROF)

CheckFourierFamily: CheckFourierFamily.hs Perms
	$(CC) -o CheckFourierFamily CheckFourierFamily.hs Perms.o $(FLAGS) $(PACKAGES) $(PROF)

EquivBases: EquivBases.hs Perms
	$(CC) -o EquivBases EquivBases.hs Perms.o $(FLAGS) $(PACKAGES) $(PROF)


# The big boys.
#
FundamentalNeighbors: FundamentalNeighbors.hs Cyclotomic SublistPred Combinadics Perms
	$(CC) -o FundamentalNeighbors FundamentalNeighbors.hs Cyclotomic.o SublistPred.o Combinadics.o Perms.o $(FLAGS) $(PACKAGES) $(PROF)

Simplify: Simplify.hs
	$(CC) -o Simplify Simplify.hs $(FLAGS) $(PACKAGES) $(PROF)

MUB-Search: MUB-Search.hs Cliques
	$(CC) -o MUB-Search MUB-Search.hs Cliques.o $(FLAGS) $(PACKAGES) $(PROF)

HNSS: HNSS.hs Polynomial
	$(CC) -o HNSS HNSS.hs Polynomial.o $(FLAGS) $(PACKAGES) $(PROF)

