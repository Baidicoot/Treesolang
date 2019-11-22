module Type.Simple where

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative
import Prelude hiding(map)

data Env k n where
  Env :: (KindForm k) => Map.Map n (k n) -> Env k n

underlying :: Env k n -> Map.Map n (k n)
underlying (Env x) = x

empty :: (KindForm k) => Env k n
empty = Env Map.empty

instance (Show n, Show (k n)) => Show (Env k n) where
  show = show . underlying

genRenamer :: (Ord n) => [n] -> [n] -> (n -> n) -> Map.Map n n
genRenamer _ [] _ = Map.empty
genRenamer using (n:ns) fn = Map.insert n (newName n) (genRenamer using ns fn)
  where
    newName n = if n `elem` using then newName (fn n) else n

isSubset :: (Eq a, Ord k) => Map.Map k a -> Map.Map k a -> Bool
isSubset a b = listSubset (Map.assocs a) b
  where
    listSubset [] _ = True
    listSubset ((k, a):xs) b = case k `Map.lookup` b of
      Just x -> x == a && (listSubset xs b)
      Nothing -> False

unite :: (Ord n, Eq (k n)) => Env k n -> Env k n -> Maybe (Env k n)
unite (Env env0) (Env env1) =
  if isSubset shared env1 then
    Just (Env $ Map.union env0 env1)
  else
    Nothing
  where
    shared = Map.intersection env0 env1

class (Functor k) => KindForm k where
  splitArr	:: k n -> Maybe (k n, k n)
  getName	:: k n -> Maybe n
  isBase	:: k n -> Bool

  arrWrapper	:: k n -> k n -> k n
  varWrapper	:: n -> k n
  base		:: k n

  collect	:: (Ord n) => k n -> Set.Set n
  collect k = case (do
      (a, b) <- splitArr k
      return $ Set.union (collect a) (collect b))
    <|> (do
      n <- getName k
      return $ Set.singleton n)
    of
      Just x -> x
      Nothing -> Set.empty

  map		:: k n -> (n -> k n) -> k n
  map k fn = case (do
      (a, b) <- splitArr k
      return $ arrWrapper (map a fn) (map b fn))
    <|> (do
      n <- getName k
      return (fn n))
    of
      Just x -> x
      Nothing -> k

  rename	:: (Ord n) => Map.Map n n -> k n -> k n
  rename rep k = fmap name k
    where
      name n = case n `Map.lookup` rep of
        Just x -> x
        Nothing -> n

  substitute	:: (Ord n) => Env k n -> k n -> Maybe (k n)
  substitute env k = if Set.disjoint names keys then Just k else
    substitute env subOnce
    where
      names = collect k
      keys = Set.fromList (Map.keys (underlying env))
      subName n = case n `Map.lookup` (underlying env) of
        Just x -> x
        Nothing -> varWrapper n
      subOnce = map k subName

  bind		:: (Ord n, Eq (k n)) => k n -> k n -> Maybe (Env k n)
  bind var exp = (do
      (a, b) <- splitArr var
      (x, y) <- splitArr exp
      first  <- bind a x
      second <- bind b y
      unite first second)
    <|> (do
      n <- getName exp
      return $ Env $ Map.singleton n var)
    <|> (if (isBase var) && (isBase exp) then Just $ Env Map.empty else Nothing)

  apply		:: (Ord n, Eq (k n)) => k n -> k n -> (n -> n) -> Maybe (k n, k n)
  apply fn var namer = do
    (a, b) <- splitArr fn
    env <- bind a v
    sub <- substitute env b
    return (a, sub)
    where
      renamer = genRenamer (Set.elems $ collect fn) (Set.elems $ collect var) namer
      v = rename renamer var

data Kind n
  = Var n
  | Arr (Kind n) (Kind n)
  | Base
  deriving(Eq)

parensIf :: Bool -> String -> String
parensIf False x = x
parensIf True x = '(':x++")"

pp :: (n -> String) -> Bool -> Kind n -> String
pp fn _ (Var n) = fn n
pp _ _ Base = "*"
pp fn n (Arr x y) = parensIf n $ (pp fn True x) ++ " -> " ++ (pp fn False y)

instance (Show n) => Show (Kind n) where
  show = pp show False

instance {-# OVERLAPPING #-} Show (Kind String) where
  show  = pp id False

instance Functor Kind where
  fmap fn (Var n)	= Var (fn n)
  fmap fn (Arr x y)	= Arr (fmap fn x) (fmap fn y)
  fmap _ Base		= Base

instance KindForm Kind where
  splitArr (Arr a b)	= Just (a, b)
  splitArr _		= Nothing
  getName (Var n)	= Just n
  getName _		= Nothing
  isBase Base		= True
  isBase _		= False
  varWrapper		= Var
  arrWrapper		= Arr
  base			= Base
