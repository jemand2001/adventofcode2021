
%: %.hs
	ghc $@

run-%: Day% inputs/%.txt
	./$< < $(word 2, $^)

test-%: Day% inputs/%-test.txt
	./$< < $(word 2, $^)
