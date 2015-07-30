-- | Pretty printing of lambda expressions
module Lambda.Pretty 
        (pretty)
where
import Lambda.Exp


-- | Pretty print an expression.
pretty :: Exp -> String
pretty xx
 = case xx of
        XVar (V var)    -> var
        XAbs (V var) e  -> "\\" ++ var ++ ". " ++ pretty e
        XApp e1 e2      -> prettyLeft e1 ++ " " ++ prettyRight e2
        XMacro (M str)  -> "#" ++ str


-- | Pretty print an expression on the left of an application.
prettyLeft :: Exp -> String
prettyLeft xx
 = case xx of
        XVar{}          -> pretty xx
        XAbs{}          -> parens (pretty xx)
        XApp{}          -> pretty xx
        XMacro{}        -> pretty xx


-- | Pretty print an expression on the right of an application.
prettyRight :: Exp -> String
prettyRight xx
 = case xx of
        XVar{}          -> pretty xx
        XAbs{}          -> parens (pretty xx)
        XApp{}          -> parens (pretty xx)
        XMacro{}        -> pretty xx


-- | Wrap a string in parenthesis.
parens :: String -> String
parens ss
 = "(" ++ ss ++ ")"

