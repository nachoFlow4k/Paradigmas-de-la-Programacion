-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.
module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros,
  )
where

import Data.List (zipWith4)
import Util

data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

tamInt :: Float -> Float -> Int -> Float
tamInt a b c = (b - a) / fromIntegral c


-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
vacio n (l, u) = Histograma l (tamInt l u n) [0 | x <- [0 .. n + 1]]
--Histograma l (tamInt l u n) [0 | x <- [0 .. n + 1]]




-- | Agrega un valor al histograma.
agregar :: Float -> Histograma -> Histograma
agregar f (Histograma inicio tamaño_intervalo cs) = Histograma inicio tamaño_intervalo (actualizarLista f inicio tamaño_intervalo cs)



-- actualizarLista es el mecanismo que agarra a la lista y le suma uno al indice adecuado usando actualizar elem
actualizarLista :: Float -> Float -> Float -> [Int] -> [Int]
actualizarLista elem inicio tam lista
  | i < 0 = actualizarElem 0 (+ 1) lista
  | i >= length lista = actualizarElem (length lista - 1) (+ 1) lista
  | otherwise = actualizarElem i (+ 1) lista
  where
    i = floor ((elem - inicio) / tam) + 1 -- calcula el indice en el que caeria un elem dado el inicio del intervalo y el tamaño de cada casilla





-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma num (desde, hasta) xs = foldr (\x rec -> agregar x rec) (vacio num (desde, hasta)) xs


-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero m _ _ _) = m

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ m _ _) = m

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p

-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.
casilleros :: Histograma -> [Casillero]
casilleros (Histograma i t l) = zipWith4 Casillero listaMin listaMax l porcentajes
                        where 
                          listaMin = infinitoNegativo : [ i + (fromIntegral n*t) | n <- [0..(length l - 2)]]
                          listaMax = [ i + (fromIntegral n*t) | n <- [0..(length l - 2)]] ++ [infinitoPositivo]
                          porcentajes = map calcPorcentaje  l  
                          calcPorcentaje x = if sum l == 0 then 0 else fromIntegral x / fromIntegral(sum l) * 100 




