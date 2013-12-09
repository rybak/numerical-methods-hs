HC=ghc -O9

main :
	$(HC) --make main.hs -o main
RM=rm -f

rebuild : clean main

clean :
	$(RM) *.hi
	$(RM) *.o
	$(RM) main
