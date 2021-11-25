
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}


{- |
Module      : Tarefa1_2021li1g117
Description : Validação de um potencial mapa
Copyright   : João Pedro da Rocha Rodrigues <a100896@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g117 where

import LI12122

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False 
validaPotencialMapa ((x , (y , z)) : a) = (validaUmaPosicao ((x , (y , z)) : a)) && (validaUmaPorta ((x , (y , z)) : a)) && (validaCaixa ((x , (y , z)) : a)) && (validaVazios ((x , (y , z)) : a)) && (validaChao ((x , (y , z)) : a))


--1. Não haver mais do que uma declaração de peça para a mesma posição.

validaUmaPosicao :: [(Peca, Coordenadas)] -> Bool
validaUmaPosicao [] = True
validaUmaPosicao ((a , b) : []) = True
validaUmaPosicao ((a , b) : (c , d) : e)
    | b == d = False 
    | otherwise = validaUmaPosicao ((c , d) : e)

--2. Declarar exactamente uma porta.

validaUmaPorta :: [(Peca, Coordenadas)] -> Bool
validaUmaPorta [] = False
validaUmaPorta ((x ,(y , z)) : a)
    | portas ((x ,(y , z)) : a) == 1 = True
    | otherwise = False

portas :: [(Peca, Coordenadas)] -> Int
portas [] = 0
portas ((x ,(y , z)) : a)
    | x == Porta = 1 + portas a
    | otherwise = portas a

--3. Todas as caixas devem estar posicionadas em cima de outra caixa ou
--   bloco, i.e. não podem haver caixas a “flutuar”.

validaCaixa :: [(Peca, Coordenadas)] -> Bool
validaCaixa [] = True 
validaCaixa ((x , (y , z)) : a)
    | x == Caixa = (if (Caixa , (y , z + 1)) `elem` a || (Bloco , (y , z + 1)) `elem` a then validaCaixa a else False) 
    | otherwise = validaCaixa a

--4. Devem existir espaços vazios (no mínimo um), i.e. o mapa não pode
--   estar totalmente preenchido por caixas, blocos e porta.

validaVazios :: [(Peca, Coordenadas)] -> Bool
validaVazios [] = True
validaVazios ((x , (y,z)) : a)
    | quantosVazios ((x , (y,z)) : a) >= 1 = True 
    | otherwise = False 

quantosVazios :: [(Peca, Coordenadas)] -> Int 
quantosVazios [] = 0
quantosVazios ((x , (y,z)) : a) = ((xMaximo ((x , (y,z)) : a) 0 ) + 1) * ((yMaximo ((x , (y,z)) : a) 0 ) + 1) - caixasBlocosPortas ((x , (y,z)) : a)

caixasBlocosPortas :: [(Peca, Coordenadas)] -> Int 
caixasBlocosPortas [] = 0
caixasBlocosPortas ((x , (y,z)) : a) = if x == Caixa || x == Bloco || x == Porta then 1 + caixasBlocosPortas a else caixasBlocosPortas a


xMaximo :: [(Peca, Coordenadas)] -> Int -> Int
xMaximo [] b = b
xMaximo ((x , (y,z)) : a) b
    | b >= y = xMaximo a b
    | otherwise = xMaximo a y

yMaximo :: [(Peca, Coordenadas)] -> Int -> Int
yMaximo [] b = b
yMaximo ((x , (y,z)) : a) b
    | b >= z = xMaximo a b
    | otherwise = xMaximo a z


--5. A base do mapa deve ser composta por blocos, i.e. deve existir um
--   chão ao longo do mapa.


validaChao :: [(Peca, Coordenadas)] -> Bool
validaChao [] = True 
validaChao ((x , (y , z)) : a)
    | z == (yMaximo ((x , (y , z)) : a) 0) = (if x == Bloco then validaChao a else False)
    | otherwise = validaChao a


