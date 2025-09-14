module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)


-- 7 ---------------------------------------------------------------

type ConstructorRecu a = (Expr -> a -> Expr -> a-> a)

recrExpr :: (Float -> a) -> (Float -> Float -> a) -> ConstructorRecu a -> ConstructorRecu a -> ConstructorRecu a-> ConstructorRecu a -> Expr -> a
recrExpr fConst fRango fSuma fResta fMult fDiv c = case c of
                                                      Const b-> fConst b
                                                      Rango a b -> fRango a b
                                                      Suma a b -> fSuma a (rec a) b (rec b)
                                                      Resta a b -> fResta a (rec a) b (rec b)
                                                      Mult a  b -> fMult a (rec a) b (rec b)
                                                      Div a b -> fDiv a (rec a) b (rec b)
                                                  where
                                                      rec = recrExpr fConst fRango fSuma fResta fMult fDiv

type FuncionRecursiva a = (a -> a -> a)

foldExpr :: (Float -> a) -> (Float -> Float -> a) -> FuncionRecursiva a -> FuncionRecursiva a -> FuncionRecursiva a -> FuncionRecursiva a -> Expr -> a
foldExpr fConst fRango fSuma fResta fMult fDiv c = case c of
                                                      Const b-> fConst b
                                                      Rango a b -> fRango a b
                                                      Suma a b -> fSuma (rec a) (rec b)
                                                      Resta a b -> fResta (rec a) (rec b)
                                                      Mult a  b -> fMult (rec a) (rec b)
                                                      Div a b -> fDiv (rec a) (rec b)
                                                  where
                                                      rec = foldExpr fConst fRango fSuma fResta fMult fDiv

-- 8 ---------------------------------------------------------------

-- | Evaluar expresiones dado un generador de números aleatorios
--eval :: Expr -> G Float
--eval (Const a ) = G a 

-- testsEval :: Test
-- testsEval =
--   test
--     [ fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
--       fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
--       -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
--       fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
--       completar
--     ]

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = error "COMPLETAR EJERCICIO 9"

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = error "COMPLETAR EJERCICIO 10"

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = error "COMPLETAR EJERCICIO 11"

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
