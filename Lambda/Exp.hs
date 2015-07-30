
-- | Lambda Calculus Expressions.
module Lambda.Exp 
        ( Var   (..)
        , Macro (..)
        , Exp   (..)
        , isXVar
        , isXMacro
        , isNormal
        , allVars
        , freeVars)
where
import Data.Set                 (Set)
import qualified Data.Set       as Set


-- | A variable name represented as a String.
data Var        
        = V String
        deriving (Show, Eq, Ord)


-- | A macro name represented as a String.
data Macro
        = M String
        deriving (Show, Eq, Ord)


-- | Expressions abstract syntax.
data Exp
        -- | Variables.
        = XVar Var

        -- | Lambda abstractions.
        | XAbs Var Exp 

        -- | Function applications.
        | XApp Exp Exp

        -- | A macro expansion.
        | XMacro Macro
        deriving (Show, Eq)


-- | Check if an expression is a variable.
isXVar :: Exp -> Bool
isXVar xx
 = case xx of
        XVar _          -> True
        _               -> False


-- | Check if an expression is a macro name.
isXMacro :: Exp -> Bool
isXMacro xx
 = case xx of
        XMacro _        -> True
        _               -> False


-- | Check if an expression is in normal form, 
--   meaning that it contains no redexes.
isNormal :: Exp -> Bool
isNormal xx
 = case xx of
        XVar _          -> True
        XMacro _        -> True
        XAbs _ x1       -> isNormal x1

        XApp XAbs{}   _ -> False
        XApp XMacro{} _ -> False
        XApp x1 x2      -> isNormal x1 && isNormal x2


-- | Get all the variables mentioned in an expression,
--   both free and bount.
allVars  :: Exp -> Set Var
allVars xx
 = case xx of
        XVar v          -> Set.singleton v
        XAbs v  x1      -> Set.insert v (allVars x1)
        XApp x1 x2      -> Set.union (allVars x1) (allVars x2)
        XMacro{}        -> Set.empty


-- | Get a set of free variables in this expression.
freeVars :: Exp -> Set Var
freeVars xx
 = case xx of
        XVar v          -> Set.singleton v
        XAbs v x1       -> Set.delete v (freeVars x1)
        XApp x1 x2      -> Set.union (freeVars x1) (freeVars x2)
        XMacro{}        -> Set.empty


