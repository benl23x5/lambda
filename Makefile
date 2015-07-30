

.PHONY : bin/reduce
bin/reduce : 
	ghc -Wall -Werror -O --make Main.hs -o bin/lambda

.PHONY : clean
clean  :
	@rm -f bin/reduce
	@find . 	-name "*.hi"  \
		-o 	-name "*.hi" \
		-o 	-name "*.o" \
		-o 	-name "*~" \
		| xargs rm -f