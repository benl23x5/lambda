
-- | A simple LL(k) parser for lambda expressions.
module Lambda.Parser 
        (parse)
where
import Lambda.Exp 
import Lambda.Lexer


-- | Parse a lambda expression, 
--   or `Nothing` if there was a parse error.
parse :: [Token] -> Maybe Exp
parse ks
 = case parseSome ks of
        -- We completely parsed the tokens.
        Just (xx, [])   -> Just xx

        -- We parsed some prefix of the tokens,
        -- but there was junk at the end that isn't part of the expression.
        Just (_, _)     -> Nothing

        -- Some other parse error.
        Nothing         -> Nothing
        

-- | Try to parse some tokens as an expression.
--
--   The standard grammar for lambda calculus is left recursive in the 
--   production for applications, which doesn't work with top-down LL
--   parsers. We get around this by using the standard trick of parsing
--   each component of the application independently then building
--   the application structure as a post process.
--   
parseSome :: [Token] -> Maybe (Exp, [Token])
parseSome ks
 = case parseBits ks of
        (x : xs, ks')   -> Just (buildApp x xs, ks')
        ([],     _)     -> Nothing

 
-- | Parse a sequence of non-application expressions.
parseBits :: [Token] -> ([Exp], [Token])
parseBits ks
 = case parseBit ks of
        Nothing         
         -> ([], ks)

        Just (e, ks')   
         -> let (es, rest)      = parseBits ks'
            in  (e : es, rest)


-- | Parse some non-application expressions.
parseBit :: [Token] -> Maybe (Exp, [Token])

parseBit (KVar str : ks)        
        = Just (XVar (V str), ks)

parseBit (KLam : KVar str : KDot : ks)
        | Just (e, ks')         <- parseSome ks
        = Just (XAbs (V str) e, ks')

parseBit (KBra : ks)
        | Just (e, KKet : ks')  <- parseSome ks
        = Just (e, ks')

parseBit (KMacro str : ks)      
        = Just (XMacro (M str), ks)

parseBit _
        | otherwise
        = Nothing


-- | Create some left-associated applications from a
--   list of expressions.
--   
--    Eg: buildApp [x1, x2, x3] => (x1 x2) x3
--
buildApp :: Exp -> [Exp] -> Exp
buildApp x0 xx0
 = buildApp' $ reverse (x0 : xx0)
 where 
       buildApp' xx
        = case xx of
           []           -> error "buildApp': list should be non-empty"
           x : []       -> x
           x : xs       -> XApp (buildApp' xs) x

        