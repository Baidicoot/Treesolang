module Eval where

import qualified Syntax as Binding (Parsed(..), Binding(..), Name, allElem, parensIf)
import qualified Data.Map as Map
import Control.Monad
import Control.Applicative

type Env = Map.Map Binding.Name State
type Parsed = Binding.Parsed

unite :: Env -> Env -> Maybe Env
unite env0 env1 = if Map.null (Map.intersection env0 env1) then Just $ Map.union env0 env1 else Nothing

data State
  = App State State
  | Op Binding.Name

splitApp :: State -> Maybe (State, State)
splitApp (App x y) = Just (x, y)
splitApp _ = Nothing

allElem = Binding.allElem
parensIf = Binding.parensIf

showState :: Bool -> State -> String
showState _ (Op n) = parensIf (allElem ":!#$%&*+./<=>?@\\^|-~" n) n
showState b (App x y) = parensIf b $ (showState False x) ++ " " ++ (showState True y)

instance Show State where
  show = showState False

bind :: State -> Binding.Binding -> Maybe Env
bind x (Binding.Var n) = Just $ Map.singleton n x
bind (Op n) (Binding.Op n') = if n == n' then Just Map.empty else Nothing
bind (App a b) (Binding.App x y) = do
  a' <- bind a x
  b' <- bind b y
  unite a' b'
bind _ _ = Nothing

substitute :: Env -> Binding.Binding -> Maybe State
substitute env (Binding.Var n) = n `Map.lookup` env
substitute env (Binding.App x y) = do
  a <- substitute env x
  b <- substitute env y
  return $ App a b
substitute _ (Binding.Op n) = Just $ Op n

applyApp :: Parsed -> State -> Maybe (Bool, State)
applyApp rule (App x y) = Just (bx || by, App x' y')
  where
    (bx, x') = applyRule rule x
    (by, y') = applyRule rule y
applyApp _ _ = Nothing

applyOp :: Parsed -> State -> Maybe (Bool, State)
applyOp rule s = do
  binding <- bind s i
  sub <- substitute binding o
  return (True, sub)
  where
    (Binding.PRule i o) = rule

applyRule :: Parsed -> State -> (Bool, State)
applyRule rule s = case applyOp rule s <|> applyApp rule s of
    Just x -> x
    Nothing -> (False, s)

applyRules :: [Parsed] -> State -> (Bool, State)
applyRules [] s = (False, s)
applyRules (r:rs) s = (bs || br, r')
  where
    (bs, s') = applyRule r s
    (br, r') = applyRules rs s'

eval :: [Parsed] -> State -> State
eval rs s = case applyRules rs s of
  (True, x) -> eval rs x
  (False, x) -> x

runPrg :: [Parsed] -> State
runPrg rs = eval rs (Op "Main")

testRules :: [Parsed]
testRules =
  [Binding.PRule (Binding.Op "Main") (Binding.App (Binding.Op "Fst") (Binding.Op "Snd"))
  ,Binding.PRule (Binding.Op "Snd") (Binding.Op "Sng")]
