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


type ConstRec a = (a -> a -> a) -- alias para el constructor recursivo

foldExpr :: (Float -> a) -> (Float -> Float -> a) -> ConstRec a -> ConstRec a -> ConstRec a -> ConstRec a -> Expr -> a
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
eval :: Expr -> G Float
eval = foldExpr (\x g -> (x, g))
                (\x y g -> dameUno (x,y) g)
                (evaluar (+))
                (evaluar (-))
                (evaluar (*))
                (evaluar (/))
                  where
                    evaluar f x y g =
                      let
                        primerGenerador = x g
                        segundoGenerador = y (snd primerGenerador)
                      in
                        (f (fst primerGenerador) (fst segundoGenerador), snd segundoGenerador)


-- 9 ---------------------------------------------------------------

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = (histograma m (rango95 (fst tomarMuestra)) (fst tomarMuestra), snd tomarMuestra)
  where tomarMuestra = muestra f n g

--                    
-- 10 ---------------------------------------------------------------

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = armarHistograma m n (eval expr)

-- 11 ---------------------------------------------------------------

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = recrExpr show (\x y -> show x ++ "~" ++ show y)
                        (convertirString (\expr -> elem (constructor expr) [CEMult, CEResta, CEDiv]) " + ")
                        (convertirString (\z -> constructor z /= CEConst) " - ")
                        (convertirString (\z -> elem (constructor z) [CESuma, CEResta, CEDiv]) " * ")
                        (convertirString (\z -> constructor z /= CEConst) " / ")
                      where convertirString p s x rx y ry = maybeParen (p x) rx ++ s ++ maybeParen (p y) ry
-- Usamos maybeParen para evitar los paréntesis de más.


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
