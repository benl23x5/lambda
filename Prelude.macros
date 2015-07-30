
-- Natural Numbers ------------------------------------------------------------
#zero		= \s. \z. z
#succ		= \n. \s. \z. s (n s z)

#one		= #succ #zero
#two		= #succ #one
#three		= #succ #two
#four		= #succ #three


-- Logic ----------------------------------------------------------------------
#true		= \x. \y. x
#false 		= \x. \y. y
#and 		= \x. \y. #if x y #false
#or 		= \x. \y. #if x #true y


-- Control --------------------------------------------------------------------
#if 		= \a. \b. \c. a b c
#isZero 	= \n. n (\x. #false) #true
#Y		= \f. (\x. f (x x)) (\x. f (x x))


-- Pairs ----------------------------------------------------------------------
#pair		= \m. \n. \b. #if b m n
#fst		= \p. p #true
#snd		= \p. p #false

-- pairs of numbers, used to define #pred
#pairZero	= #pair #zero #zero
#pairSucc	= \p. #pair (#snd p) (#succ (#snd p))


-- Arithmetic -----------------------------------------------------------------
#pred 		= \n. #fst (n #pairSucc #pairZero)
#add 		= \m. \n. \s. \z. m s (n s z)
#sub		= \m. \n. n #pred m
#mul		= \m. \n. \z. n (m z)
#exp		= \m. \n. n m

-- we have to do two subtractions here 
--	because we don't have negative numbers
#equals		= \m. \n. #and (#isZero (#sub m n)) (#isZero (#sub n m))

-- factorial. eg: fac 3 = 1 * 2 * 3
#fac		= #Y (\fac. \n. #if (#isZero n) #one (#mul n (fac (#pred n))))


-- Lists ----------------------------------------------------------------------
#nil 		= #pair #true #true
#isNil		= #fst
#cons		= \h. \t. #pair #false (#pair h t)
#head		= \l. #fst (#snd l)
#tail		= \l. #snd (#snd l)

#rangeDown	= #Y (\rangeDown. \n. #if (#isZero n) #nil (#cons n (rangeDown (#pred n))))
#rangeUp	= #Y (\rangeUp.   \n. #cons n (rangeUp (#succ n)))

#length		= #Y (\length.    \l. #if (#isNil l) #zero (#succ (#length (#tail l)))) 
#sum		= #Y (\sum.       \l. #if (#isNil l) #zero (#add (#head l) (sum (#tail l))))