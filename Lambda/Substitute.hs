
module Lambda.Substitute where
import Lambda.Exp
import Data.Set         (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set


-- | Do a capture avoiding substitution,
--   alpha converting expressions as need be.
subNoCap :: Var -> Exp -> Exp -> Exp
subNoCap vSub xSub xx0
 = down Map.empty xx0
 where
        freeX = freeVars xSub

        down rw xx
         = case xx of
            XVar v  
             |  Just vRewrite <- Map.lookup v rw
             -> XVar vRewrite

             | v == vSub      -> xSub
             | otherwise      -> xx

            XAbs v x1
             | v == vSub      -> xx

             | Set.member v freeX
             -> let newVar = chooseNewVar 
                           $ Set.unions 
                                [ Set.singleton vSub, allVars xSub
                                , Set.singleton v,    allVars x1
                                , Set.fromList $ Map.keys  rw
                                , Set.fromList $ Map.elems rw ]

                    rw'    = Map.insert v newVar rw
                in XAbs newVar (down rw' x1)

             | otherwise
             -> XAbs v (down rw x1)

            XApp x1 x2
             -> XApp (down rw x1) (down rw x2)

            XMacro _ -> xx


-- | Choose a new variable that isn't in the given set.
chooseNewVar :: Set Var -> Var
chooseNewVar notVs
        = head $ filter (\v -> not $ Set.member v notVs) someNewVars

-- | Infinite list of new variables.

someNewVars :: [Var]
someNewVars     
        =  (map (\c -> V [c]) ['A' .. 'Z'])
        ++ [V ("A" ++ show n) | n <- [0 :: Int ..]]

