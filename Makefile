
%: %.hs
	ghc $@

run-%: Day% inputs/%.txt
	./$< < $(word 2, $^)
