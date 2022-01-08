module Tarefa1_2021li1g117_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g117
import Fixtures
import Data.Bool (Bool(False, True))

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    , "Tarefa 1 - Teste Valida Mapa sem porta " ~: validaPotencialMapa [(Caixa,(1,3)),(Bloco,(1,4))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa valido " ~: validaPotencialMapa [(Porta, (0, 2)),(Bloco, (0, 3)), (Bloco, (1, 3)), (Bloco, (2, 3))] ~=? True
    , "Tarefa 1 - Teste Valida Mapa com uma caixa a flutuar" ~: validaPotencialMapa [(Porta,(0,2)),(Bloco,(0,3)),(Bloco, (1,3)),(Bloco,(2,3)),(Caixa,(1,0))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa só com blocos e porta" ~: validaPotencialMapa [(Porta,(0,0)),(Bloco,(0,1)),(Bloco,(1,0)),(Bloco,(1,1))] ~=? False
    ]


