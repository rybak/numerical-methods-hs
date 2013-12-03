HC=ghc -O9
RM=rm -f

build : main

main : main.hs
	$(HC) main.hs -o main

rebuild : clean build

#fusion : Fusion.hs
#	$(HC) Fusion.hs -c

clean :
	$(RM) *.hi
	$(RM) main

#fresh :
#	ls | grep -E "\\.[0-9][0-9]$$" | xargs -t -I{} rm {}
