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



type ConstRec a = (a -> a -> a) -- alias para el constructor recursivo

foldExpr :: (Float -> a) -> (Float -> Float -> a) -> ConstRec a -> ConstRec a -> ConstRec a -> ConstRec a -> Expr -> a
foldExpr fConst fRango fSuma fResta fMult fDiv c = case c of
    Const b-> fConst b
    a b -> fRango a b
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

data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

eval :: Expr -> G Float
eval = foldExpr (\x g -> (x, g))
                (\x y g -> dameUno (x,y) g)
                (evaluar (+))
                (evaluar (-)) 
                (evaluar (*))
                (evaluar (/))
                  where 
                    evaluar f x y g = (f x y, (y g))
                    --evaluar f x y g = (f (fst (x g)) (fst (y (snd (x g)))), g)
                    
                    
                    
                    --valor = fst (x g)
                    --generador = snd (x g)

--

G Float = Gen -> (Float, Gen)

-- 9 ---------------------------------------------------------------

-- | recibo m=cantidad de casilleros, n=cantidad de muestras de f, g=generador usado en f.
-- La funcion me va a devolver un Generador de Histograma, en la primera componente de la tupla va a tener el histograma
-- con los m casilleros, el rango, y la lista; para el rango llamo a rango9, la lista que se va a pasar como parametro la obtengo quedandome
-- con la primera componente de la tuple devuelta por la funcion muestra, este toma la funcion, el n y el generador, le aplico la funcion n veces a g
-- eso me da un generador con una lista, de ahi solo me quedo con la primer componente, esa
-- lista es la que utiliza rango 95 para armar su tupla, y esta es la que necesita el histograma como rango,
-- y nuevamente genero la lista de numeros que tenia antes con muestra quedandome con la primer componente de la tupla para terminar de pasarle los
-- parametros a histograma.
-- Finalmente, me quedo con la segunda componente de esta tupla del generador obtenido con muestra.


armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = (histograma m (rango95 (fst tomarMuestra)) (fst tomarMuestra), snd tomarMuestra)
  where tomarMuestra = muestra f n g

--                    
-- 10 ---------------------------------------------------------------


-- | llamo a la funcion armarHistograma con m=cantidad de casilleros, n= la cantidad de veces que quiero evaluar y expr=la expresion a evaluar.
-- utilizo la funcion armarHistograma pasandole los casilleros, el n, y de funcion, el resultado de aplicarle eval a la expresion, que va a devolver
-- un generador en forma de tupla dependendo de como se evalue esa expresion. O sea me armo un histograma a partir de la evaluacion de la expresion

evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = armarHistograma m n (eval expr)

-- 11 ---------------------------------------------------------------

-- En definitiva queremos pasar la Expresion dada a lenguaje normal que podamos entender, como dice la funcion.
-- Primero llamo a recrExpr para obtener la expresion a mostrar, luego
-- definimos con notaciones lambda los posibles casos de expresion (suma, resta, cte. , etc), maybeParen esta
-- para evitar los parentesis de mas.
-- 

mostrar :: Expr -> String
mostrar = recrExpr show (\x y -> show x ++ "~" ++ show y)
                        (convertirString (\expr -> elem (constructor expr) [CEMult, CEResta, CEDiv]) " + ")
                        (convertirString (\z -> constructor z /= CEConst) " - ")
                        (convertirString (\z -> elem (constructor z) [CESuma, CEResta, CEDiv]) " * ")
                        (convertirString (\z -> constructor z /= CEConst) " / ")
                      where convertirString p s x rx y ry = maybeParen (p x) rx ++ s ++ maybeParen (p y) ry


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
