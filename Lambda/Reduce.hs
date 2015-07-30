module Lambda.Reduce where
import Lambda.Exp
import Lambda.Substitute


-- | Perform a single step reduction.
--   Left-to-right evaluation order, reducing under lambdas.
--
--   The only runtime error we can encounter is discovering a macro with
--   no definition. If this happens we return the offending macro name.
-- 
reduce :: [(Macro, Exp)] -> Exp -> Either Macro Exp
reduce macros xx
 = case xx of
        -- Variables and macro names are already in normal form.
        XVar _            -> Right xx
        XMacro _          -> Right xx

        -- Reduce under lambda abstractions.
        XAbs v x1       
         -> case reduce macros x1 of
                Left err  -> Left err
                Right x1' -> Right $ XAbs v x1' 

        -- When the left of an application is an abstraction 
        -- we can perform a (capture avoiding) substitution.
        XApp (XAbs v x11) x2    
         -> Right $ subNoCap v x2 x11

        -- If the left of an application is a macro,
        -- then expand the macro in a separate step.
        XApp (XMacro m) x2    
         -> case lookup m macros of
                Nothing  -> Left m
                Just x1' -> Right $ XApp x1' x2 

        -- Try and reduce expressions deep with applications,
        -- but still only one-at-a-time.
        XApp x1 x2      
         |  not $ isNormal x1 
         -> case reduce macros x1 of
                Left err  -> Left err
                Right x1' -> Right $ XApp x1' x2

         |  otherwise     
         -> case reduce macros x2 of
                Left err  -> Left err
                Right x2' -> Right $ XApp x1 x2'
