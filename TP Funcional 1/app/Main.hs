module Main (main) where

import App
import Expr
import Expr.Parser
import GHC.Stack (HasCallStack)
import Generador
import Histograma
import Test.HUnit
import Util

main :: IO ()
main = runTestTTAndExit allTests

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      "Ej 7 - Expr.recrExpr" ~: testsRecr,
      "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 6 "hola" ~?= "  hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc"
     -- completar
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem 0 (+ 10) [1, 2, 3] ~?= [11, 2, 3],
      actualizarElem 1 (+ 10) [1, 2, 3] ~?= [1, 12, 3],
      actualizarElem 10 (* 2) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] ~?= [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 22],
      actualizarElem 4 (*(-1)) [1, 2, 3, 4, -5, 6] ~?= [1, 2, 3, 4, 5, 6],
      actualizarElem 10 (+ 10) [1, 2, 3] ~?= [1, 2, 3],
      actualizarElem 1 (+ 10) [] ~?= []
    ]

testsVacio :: Test
testsVacio =
  test
    [ casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ]
   --   completar
    ]

testsAgregar :: Test
testsAgregar =
  let h0 = vacio 3 (0, 6)
   in test
        [ casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores están acá
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores están acá
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ]
        --  completar
        ]

testsHistograma :: Test
testsHistograma =
  test
    [ histograma 4 (1, 5) [1, 2, 3] ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5))))
  --    completar
    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ]
   --   completar
    ]

testsRecr :: Test
testsRecr =
  test
    [ recrExpr id (+) (\ _ ri _ rd -> ri + rd) (\ _ ri _ rd -> ri + rd) (\ _ ri _ rd -> ri + rd) (\ _ ri _ rd -> ri + rd)
      (Const 1) ~?= 1,
      recrExpr id (+) (\ i ri d rd -> if i == d then 5 else ri) (\ i ri d rd -> if i == d then 10 else rd)
      (\ i ri d rd -> if i ==   d then 90 else ri) (\ i ri d rd -> if i == d then 4 else rd)
      (Suma (Const 2) (Resta (Const 3) (Const 3))) ~?= 2,
      recrExpr abs (**) (\ i ri d rd -> if i == d then 0 else ri + rd) (\ i ri d rd -> if i == d then 0 else ri - rd)
      (\ i ri d rd -> if i == d then 0 else ri * rd) (\ i ri d rd -> if i == d then 0 else ri / rd)
      (Mult (Rango 2 4) (Div (Suma (Const (-62)) (Resta (Const 3) (Const (-20)))) (Const 5))) ~?= 144,
      recrExpr abs (**) (\ i ri d rd -> if i == d then 0 else ri + rd) (\ i ri d rd -> if i == d then 0 else ri - rd)
      (\ i ri d rd -> if i == d then 0 else ri * rd) (\ i ri d rd -> if i == d then 0 else ri / rd)
      (Resta (Suma (Mult (Const 1) (Const 1)) (Mult (Const 1) (Const 1))) (Const 5)) ~?= (-5),
      recrExpr (: []) (\ x y -> [x / y]) (\ i ri d rd -> if i == d then [] else ri) (\ i ri d rd -> if i == d then [] else rd)
      (\ i ri d rd -> ri ++ rd) (\ _ _ _ _ -> [])
      (Resta (Suma (Mult (Const 1) (Const 1)) (Mult (Const 1) (Const 1))) (Const 5)) ~?= [5],
      recrExpr (: []) (\ x y -> [x / y]) (\ i ri d rd -> if i == d then [] else ri) (\ i ri d rd -> if i == d then [] else rd)
      (\ i ri d rd -> ri ++ rd) (\ _ _ _ _ -> [])
      (Mult (Rango 6 2) (Div (Suma (Const (-62)) (Resta (Const 3) (Const (-20)))) (Const 5))) ~?= [3],
      recrExpr (: []) (\ x y -> [x / y]) (\ i ri d rd -> if i == d then [] else ri) (\ i ri d rd -> if i == d then [] else rd)
      (\ i ri d rd -> ri ++ rd) (\ _ _ _ _ -> [])
      (Suma (Const 5) (Const 5)) ~?= []
    ]


testsFold :: Test
testsFold =
  test
    [ foldExpr id (+) (\ri rd -> ri + rd) (\ri rd -> ri + rd) (\ri rd -> ri + rd) (\ri rd -> ri + rd) (Const 1) ~?= 1,
    foldExpr id (+) (\ri rd -> ri + rd) (\ri rd -> ri + rd) (\ri rd -> ri + rd) (\ri rd -> ri + rd)
    (Suma (Const 2) (Resta (Const 3) (Const 5))) ~?= 10,
    foldExpr (*10) (\ ri y -> (ri + y)/2) (\ri rd -> ri + rd) (\ri rd -> ri - rd) (\ri rd -> ri * rd) (\ri rd -> ri / rd)
    (Suma (Const 2) (Resta (Mult (Const 2) (Const 3)) (Div (Const 50) (Const 10)))) ~?= 615,
    foldExpr id (\ ri y -> (ri + y)/2) (\ri rd -> ri + rd) (\ri rd -> ri - rd) (\ri rd -> ri * rd) (\ri rd -> ri / rd)
    (Mult (Rango 6 12) (Rango 40 80)) ~?= 540,
    foldExpr (: []) (\ ri y -> [ri + y]) (\ri rd -> ri ++ rd) (\ri rd -> ri ++ rd) (\ri rd -> ri ++ rd)
    (\ri rd -> ri ++ rd) (Suma (Const 2) (Resta (Mult (Const 2) (Const 3)) (Div (Const 50) (Const 10)))) ~?= [2, 2, 3, 50, 10],
    foldExpr (\ri -> Const (ri * (-1))) (\ ri y -> Rango y ri) Resta Suma Div Mult
    (Suma (Const 2) (Resta (Mult (Const (-2)) (Const 3)) (Div (Const 50) (Rango 5 20)))) ~?=
    Resta (Const (-2)) (Suma (Div (Const 2) (Const (-3))) (Mult (Const (-50)) (Rango 20 5)))
    ]

testsEval :: Test
testsEval =
  test
    [ fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,

      -- caso Resta rangos iguales
      fst (eval (Resta (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= -0.32698154,
      -- caso Resta 
      fst (eval (Resta (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 1.7980492,

      -- caso multiplicacion dos rangos iguales 
      fst (eval (Mult (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 8.74398993,
      -- caso multiplicacion 
      fst (eval (Mult (Const 1) (Const 1)) (genNormalConSemilla 0)) ~?= 1,

      --caso division
      fst (eval (Div (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 2.7980492,
      --caso division por 0
--      fst (eval (Div (Rango 1 5) (Const 0)) (genNormalConSemilla 0)) ~?= Infinity,

      --caso suma y resta anidados que den 0
      fst (eval (Resta (Suma (Const 2) (Const 0.7980492)) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 0,
      --caso multiplicacion,suma y division anidados que den 1
      fst (eval (Div (Mult (Suma (Const 2) (Const 0.7980492)) (Const 1)) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 1
    ]

testsArmarHistograma :: Test
testsArmarHistograma =
   test
     [
      casilleros (fst (armarHistograma 3 1 (dameUno (1, 5)) genFijo)) ~?= 
        [Casillero infinitoNegativo 2.0 0 0.0,
            Casillero 2.0 2.6666667 0 0.0,
            Casillero 2.6666667 3.3333335 1 100.0,
            Casillero 3.3333335 4.0 0 0.0,
            Casillero 4.0 infinitoPositivo 0 0.0],
      casilleros (fst (armarHistograma 8 33 (dameUno (10, 34)) genFijo)) ~?= 
        [Casillero infinitoNegativo 21.0 0 0.0,
            Casillero 21.0 21.25 0 0.0,
            Casillero 21.25 21.5 0 0.0,
            Casillero 21.5 21.75 0 0.0,
            Casillero 21.75 22.0 0 0.0,
            Casillero 22.0 22.25 33 100.0,
            Casillero 22.25 22.5 0 0.0,
            Casillero 22.5 22.75 0 0.0,
            Casillero 22.75 23.0 0 0.0,
            Casillero 23.0 infinitoPositivo 0 0.0],
      casilleros (fst (armarHistograma 6 13 (dameUno (6, 10)) (genNormalConSemilla 0))) ~?= 
        [Casillero infinitoNegativo 5.7222404 0 0.0,
            Casillero 5.7222404 6.526694 3 23.076923,
            Casillero 6.526694 7.3311477 1 7.692308,
            Casillero 7.3311477 8.135601 3 23.076923,
            Casillero 8.135601 8.940055 2 15.384616,
            Casillero 8.940055 9.744509 3 23.076923,
            Casillero 9.744509 10.548962 1 7.692308,
            Casillero 10.548962 infinitoPositivo 0 0.0]
     ]

testsEvalHistograma :: Test
testsEvalHistograma =
   test
     [casilleros (fst (evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0))) ~?=
      [Casillero infinitoNegativo 105.454315 0 0.0,
          Casillero 105.454315 105.454315 0 0.0,
          Casillero 105.454315 105.45432 0 0.0,
          Casillero 105.45432 105.45432 0 0.0,
          Casillero 105.45432 105.45432 0 0.0,
          Casillero 105.45432 105.45433 0 0.0,
          Casillero 105.45433 105.45433 0 0.0,
          Casillero 105.45433 105.45434 0 0.0,
          Casillero 105.45434 105.45434 0 0.0,
          Casillero 105.45434 105.45434 10 100.0,
          Casillero 105.45434 105.454346 0 0.0,
          Casillero 105.454346 105.454346 0 0.0,
          Casillero 105.454346 infinitoPositivo 0 0.0],
      casilleros (fst (evalHistograma 5 62 (Suma (Rango 2 80) (Rango 23 55)) (genNormalConSemilla 1))) ~?=
      [Casillero infinitoNegativo 95.025856 0 0.0,
          Casillero 95.025856 95.02586 0 0.0,
          Casillero 95.02586 95.02587 62 100.0,
          Casillero 95.02587 95.02587 0 0.0,
          Casillero 95.02587 95.02588 0 0.0,
          Casillero 95.02588 95.02589 0 0.0,
          Casillero 95.02589 infinitoPositivo 0 0.0],
      casilleros (fst (evalHistograma 7 39 (Suma (Rango 29 80) (Rango 1 1)) genFijo)) ~?=
      [Casillero infinitoNegativo 54.5 0 0.0,
          Casillero 54.5 54.785713 0 0.0,
          Casillero 54.785713 55.07143 0 0.0,
          Casillero 55.07143 55.357143 0 0.0,
          Casillero 55.357143 55.642857 39 100.0,
          Casillero 55.642857 55.92857 0 0.0,
          Casillero 55.92857 56.214287 0 0.0,
          Casillero 56.214287 56.5 0 0.0,
          Casillero 56.5 infinitoPositivo 0 0.0]
     ]

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

testsMostrar :: Test
testsMostrar =
  test
    [ mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
        ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0",
      mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Suma (Const 1) (Const 2)) (Suma (Const 3) (Const 4)))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Mult (Mult (Mult (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Const 1) (Mult (Const 2) (Mult (Const 3) (Const 4))))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Mult (Const 1) (Const 2)) (Mult (Const 3) (Const 4)))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
        ~?= "(1.0 - 2.0) - (3.0 - 4.0)",
      mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 - 2.0) - 3.0) - 4.0",
      mostrar (Suma (Mult (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 + 2.0) * 3.0) + 4.0",
      mostrar (Mult (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "(1.0 + 2.0 + 3.0) * 4.0"
    ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      -- Redondeo
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      -- Infinitos
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]
