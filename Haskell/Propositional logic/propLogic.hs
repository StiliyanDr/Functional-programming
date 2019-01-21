module PropositionalLogic where
import Data.List

type Name = String

data PropFormula = Constant Bool
                 | Variable Name
		 | Not PropFormula
		 | And PropFormula PropFormula
		 | Or PropFormula PropFormula
                 | Implies PropFormula PropFormula

infixr 7 `Implies`
infixl 8 `Or`
infixl 9 `And`

psi, fi :: PropFormula
fi = (Variable "x") `And` (Variable "y") `Implies` (Variable "z")
psi = (Variable "x") `Or` (Variable "y") `Implies` (Variable "z") `And` (Variable "t")

instance Show PropFormula where
 show (Constant value) = if value then "T" else "F"
 show (Variable x) = x
 show (Not fi) = '¬' : show fi
 show (And fi psi) = showCompositeFormula fi "&" psi
 show (Or fi psi) = showCompositeFormula fi "v" psi
 show (Implies fi psi) = showCompositeFormula fi "=>" psi

showCompositeFormula :: PropFormula -> String -> PropFormula -> String
showCompositeFormula fi operation psi =
 "(" ++ show fi ++ " " ++ operation ++ " " ++ show psi ++ ")"

instance Eq PropFormula where
 (Constant lhs) == (Constant rhs) = lhs == rhs
 (Variable x) == (Variable y) = x == y
 (Not fi) == (Not psi) = fi == psi
 (And fi1 fi2) == (And psi1 psi2) = fi1 == psi1 && fi2 == psi2
 (Or fi1 fi2) == (Or psi1 psi2) = fi1 == psi1 && fi2 == psi2
 (Implies fi1 fi2) == (Implies psi1 psi2) = fi1 == psi1 && fi2 == psi2
 _ == _ = False

type Environment = [(Name, Bool)]

valueOfIn :: Name -> Environment -> Bool
valueOfIn name ((x, value) : pairs) = if (name == x)
                                      then value
				      else valueOfIn name pairs

allTrue :: Environment
allTrue = [("x", True), ("y", True), ("z", True), ("t", True)]

valueOf :: PropFormula -> Environment -> Bool
valueOf (Constant value) _ = value
valueOf (Variable x) environment = valueOfIn x environment
valueOf (Not fi) environment = not $ valueOf fi environment
valueOf (Or fi psi) environment = valueOfCompositeFormula fi (||) psi environment
valueOf (And fi psi) environment = valueOfCompositeFormula fi (&&) psi environment
valueOf (Implies fi psi) environment =
 valueOfCompositeFormula fi (\a b -> (not a) || b) psi environment

type BooleanOperation = (Bool -> Bool -> Bool)

valueOfCompositeFormula :: PropFormula -> BooleanOperation -> PropFormula -> Environment -> Bool
valueOfCompositeFormula fi operation psi environment =
 valueOf fi environment `operation` valueOf psi environment

uniqueElementsOf :: Eq item => [item] -> [item]
uniqueElementsOf = nub

variablesUnion :: PropFormula -> PropFormula -> [Name]
variablesUnion fi psi = uniqueElementsOf $ variablesOf fi ++ variablesOf psi

variablesOf :: PropFormula -> [Name]
variablesOf (Constant _) = []
variablesOf (Variable x) = [x]
variablesOf (Not fi) = variablesOf fi
variablesOf (Or fi psi) = variablesUnion fi psi
variablesOf (And fi psi) = variablesUnion fi psi
variablesOf (Implies fi psi) = variablesUnion fi psi

