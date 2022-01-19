all: words.txt
	ghc -O2 Numbers.hs -o numbers
	ghc -O2 Letters.hs -o letters
words.txt:
	wget -O words.txt https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt
	dos2unix words.txt
clean:
	rm *.hi *.o
