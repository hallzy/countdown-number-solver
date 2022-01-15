all:
	ghc -O2 Numbers.hs -o numbers
	ghc -O2 Letters.hs -o letters
clean:
	rm *.hi *.o
