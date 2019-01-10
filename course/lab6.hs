
module Lab6 where
import Data.List (nub)
import Data.Maybe (fromJust)

data Fruct = Mar String Bool | Portocala String Int deriving (Eq, Show)

sici = Portocala "Sanguinello" 10
edesicilia :: Fruct -> Bool
edesicilia (Portocala "Sanguinello" _) = True
edesicilia _ = False

-- Logica propozitionala
type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  deriving (Eq, Read)
infixr 2 :|:
infixr 3 :&:

p = (Var "P")
q = (Var "Q")
p1 = (p :|: q) :&: ((Not p :&: q))

instance Show Prop where
    show (Var nume) = nume
    show (F) = "false"
    show (T) = "true"
    show (Not p) = "(~" ++ show p ++ ")"
    show (p :|: q) = "(" ++ show p ++ " \\/ " ++ show q ++ ")"
    show (p :&: q) = "(" ++ show p ++ " /\\ " ++ show q ++ ")"

type Env = [(Nume, Bool)]
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval (Var nume) e = impureLookup nume e
eval (F) _ = False
eval (T) _ = True
eval (Not p) e = not (eval p e)
eval (p :|: q) e = (eval p e) || (eval q e)
eval (p :&: q) e = (eval p e) && (eval q e)

test_eval = eval (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

variable :: Prop -> [Nume]
variable (Var nume) = [nume]
variable (F) = []
variable (T) = []
variable (Not p) = variable p
variable (p :|: q) = nub ((variable p) ++ (variable q))
variable (p :&: q) = nub ((variable p) ++ (variable q))

envs :: [Nume] -> [[(Nume, Bool)]]
envs xs = [ zip xs y | y <- bin (length xs) ]
    where
        bin 1 = [[True], [False]]
        bin n = [ (x : y) | x <- [True, False], y <- (bin (n-1)) ]

satisfiabila :: Prop -> Bool
satisfiabila p = (filter (\e -> eval p e == True) (envs (variable p))) /= []

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False