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

--recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
--recr f z _ [] = z
--recr f z (x:xs) = f x xs (recr f z xs)

 --analizando recr, vamos definir una funcion que agarre algo del tipo Expr, le aplique una funcion, y devuelva algo de otro tipo de dato.
 --Como el parametro de entrada sobre el que vamos a aplicar la funcion puede ser uno de 6 distintos tipos de Expr,  necesitamos armar casos para cada uno que las funciones
 -- que se usan para los distintos constructores no necesariamente son las mismas.

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
--Importante notar como en los constructores recursivos, accedemos a la estructura (a y b) al igual que al llamado recursivo de a y b((rec a) y (rec b)), tal cual
-- como se ve en la recursion primitiva que implementa recr.



--Siguiendo con la premisa de la recursion anterior, tomamos una funcion por cada posible constructor recursivo y luego
-- el parametro de tipo Expr

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

--cuando hacemos el fold,  las operaciones se hacen entre datos del tipo a por lo que la FuncionRecursivo va de a en a.

-- 8 ---------------------------------------------------------------

-- | Evaluar expresiones dado un generador de números aleatorios

--como tenemos que hacer aritmetica basica, la idea es agarrar 2 datos y juntarlos atravez del operador acorde al caso.
--Como podemos tener expresiones complejas,  los 2 datos van a ser muchas veces resultados obtenidos atraves de recursiones
--previas como se ve en recursion global.  Por lo que es mas apropiado usar foldExpr.
eval :: Expr -> G Float
eval = foldExpr (\x g -> (x, g))
                (\x y g -> dameUno (x,y) g)
                (\x y g -> evaluar (+) x y g)
                (\x y g -> evaluar (-) x y g)
                (\x y g -> evaluar (*) x y g)
                (\x y g -> evaluar (/) x y g)

evaluar :: (Float -> Float -> Float) -> G Float -> G Float -> Gen -> (Float, Gen)
evaluar f x y g = (f (fst (x g)) (fst (y (snd (x g)))), g)

-- operacion :: (Float -> Float -> Float) -> G Float -> G Float -> Gen -> G Float
-- operacion f x y g = (f ((fst (x g)) (fst (y (snd (x g))))), g)

  --    where operacion f x y g = (\f x y g -> (fst (x g) f fst (y (snd (x g))), g))

-- 
-- muestra :: G a -> Int -> G [a]
-- rango95 :: [Float] -> (Float, Float)

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.

-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
-- histograma :: Int      -> (Float, Float)     -> [Float]     -> Histograma
--        cant casilleros -> rango (intervalos) -> lista elems

-- HISTOGRAMA  :: Int -> Float     -> [Int]
--             inicio -> intervalo -> lista elemsxintervalo

--armarHistograma :: Int     -> Int          -> G Float     -> G Histograma
--                casilleros -> cant muestra -> (Float,Gen) -> 

--armarHistograma :: Int -> Int -> Gen -> (Float, Gen) -> Gen -> (Histograma, Gen)
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = (histograma m (rango95 (fst (muestra f n g  ))) (fst (muestra f n g)), snd (muestra f n g))

-- Gen -> (Float, Gen)

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = armarHistograma m n (eval expr)

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = recrExpr show (\x y -> show x ++ "~" ++ show y)
                        (\x (recx) y (recy) ->maybeParen (constructor x /= CERango && constructor x /= CESuma && constructor x /= CEConst) (recx) ++ " + "++maybeParen (constructor y /= CERango && constructor y /= CESuma && constructor y /= CEConst) (recy))
                        (\x (recx) y (recy) ->maybeParen (constructor x /= CEConst) (recx) ++ " - "++maybeParen (constructor y /= CEConst) (recy))
                        (\x recx y recy -> maybeParen (constructor x /= CERango && constructor x /= CEMult && constructor x /= CEConst) recx ++ " * " ++ maybeParen (constructor y /= CERango && constructor y /= CEMult && constructor y /= CEConst) recy)
                        (\x (recx) y (recy) ->maybeParen (constructor x /= CEConst) (recx) ++ " / "++maybeParen (constructor y /= CEConst) (recy))

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
