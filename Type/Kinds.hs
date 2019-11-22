module ComplexKind where

{-# LANGUAGE GADTs #-}

import qualified Data.Map as Map
import Data.List (nub)
import Prelude hiding(map)
import Control.Applicative
import qualified Data.Set as Set

data KindExpr a
  = EConcrete
  | EArr (KindExpr a) (KindExpr a)
  | EVar a
  | EApp (KindExpr a) (KindExpr a)
  deriving(Show,Eq)

data Kind a
  = Concrete
  | Var a
  | Arr (Kind a) (Kind a)
  | App (Kind a) (Kind a)
  deriving(Show,Eq)

data NormaliseError
  = Infinate
  | Indeterminable
  | Incompatible
  deriving(Show,Eq)

data Env k a where
  Env :: (KindForm k) => Map.Map a (k a) -> Env k a

instance (Show a, Show (k a)) => Show (Env k a) where
  show = show . underlying

underlying (Env m) = m

keysDisjoint :: Ord a => Map.Map a b -> Map.Map a c -> Bool
keysDisjoint a b = Set.disjoint (Set.fromList $ Map.keys a) (Set.fromList $ Map.keys b)

unite :: (Ord n) => Env k n -> Env k n -> Maybe (Env k n)
unite (Env env0) (Env env1) = if keysDisjoint env0 env1 then Just $ Env (Map.union env0 env1) else Nothing

genRenamer    :: (Eq n, Ord n) => [n] -> [n] -> (n -> n) -> Map.Map n n
genRenamer _ [] _ = Map.empty
genRenamer using (u:us) fn = Map.insert u (newName u) (genRenamer using us fn)
  where
    newName n = if n `elem` using then newName (fn n) else n

class KindForm k where
  splitArr	:: k n -> Maybe (k n, k n)
  splitApp	:: k n -> Maybe (k n, k n)
  getName	:: k n -> Maybe n
  varWrapper	:: n -> k n
  appWrapper	:: k n -> k n -> k n
  arrWrapper	:: k n -> k n -> k n
  base		:: k n
  isBase	:: k n -> Bool

  collect	:: (Eq n, Ord n) => k n -> Set.Set n
  collect k = case (arr k <|> app k <|> var k) of
    Just x -> x
    Nothing -> Set.empty
    where
      arr k = do
        (a, b) <- splitArr k
        return $ Set.union (collect a) (collect b)
      app k = do
        (a, b) <- splitApp k
        return $ Set.union (collect a) (collect b)
      var k = do
        n <- getName k
        return $ Set.singleton n

  rename	:: (Eq n, Ord n) => Map.Map n n -> k n -> k n
  rename rep k = map name k
    where
      name n = case Map.lookup n rep of
        Just x -> x
        Nothing -> n

  substitute	:: (Eq n, Ord n) => Env k n -> k n -> Maybe (k n)
  substitute env k = if not (Set.disjoint names eKeys) then
      substitute env (subOnce env k)
    else Just k
    where
      names = collect k
      eKeys  = Set.fromList (Map.keys (underlying env))
      arr env k = do
        (a, b) <- splitArr k
        return $ arrWrapper (subOnce env a) (subOnce env b)
      app env k = do
        (a, b) <- splitApp k
        return $ appWrapper (subOnce env a) (subOnce env b)
      var env k = do
        n <- getName k
        Map.lookup n (underlying env)
      subOnce env k = case (arr env k <|> app env k <|> var env k) of
        Just x -> x
        Nothing -> k

  bind		:: (Eq n, Ord n) => k n -> k n -> Maybe (Env k n)
  bind var exp =
        assign
    <|> arrs
    <|> apps
    <|> conc
    where
      conc = if (isBase var) && (isBase exp) then Just $ Env Map.empty else Nothing
      assign = do
        n <- getName var
        return $ Env $ Map.singleton n exp
      arrs = do
        (a, b) <- splitArr var
        (x, y) <- splitArr exp
        first  <- bind a x
        second <- bind b y
        unite first second
      apps = do
        (a, b) <- splitApp var
        (x, y) <- splitApp exp
        first  <- bind a x
        second <- bind b y
        unite first second

  apply		:: (Eq n, Ord n) => k n -> k n -> (n -> n) -> Maybe (k n, Env k n)
  apply fn var namer = (do
      (a, b) <- splitArr fn
      env <- bind a var
      sub <- substitute env b
      return (sub, env))

  map		:: (n -> n) -> k n -> k n
  map f k = case (arr k <|> app k <|> var k) of
    Just x -> x
    Nothing -> k
    where
      arr k = do
        (a, b) <- splitArr k
        return $ arrWrapper (map f a) (map f b)
      app k = do
        (a, b) <- splitApp k
        return $ appWrapper (map f a) (map f b)
      var k = do
        n <- getName k
        return $ varWrapper (f n)

instance KindForm KindExpr where
  varWrapper		= EVar
  appWrapper		= EApp
  arrWrapper		= EArr
  base			= EConcrete
  isBase EConcrete	= True
  isBase _		= False
  splitArr (EArr x y)	= Just (x, y)
  splitArr _		= Nothing
  splitApp (EApp x y)	= Just (x, y)
  splitApp _		= Nothing
  getName (EVar n)	= Just n
  getName _		= Nothing
