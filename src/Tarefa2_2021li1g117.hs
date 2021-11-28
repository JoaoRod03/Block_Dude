{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{- |
Module      : Tarefa2_2021li1g117
Description : Construção/Desconstrução do mapa
Copyright   : João Pedro da Rocha Rodrigues <a100896@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g117 where

import LI12122 ( Coordenadas, Peca(Vazio), Mapa )
import Data.List (sortBy)
import Data.Function (on)
import Tarefa1_2021li1g117 (yMaximo, xMaximo)

--funções finais

constroiMapa :: [(Peca,Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa ((x , (y , z)) : a) = constroiMapaAux ((x , (y , z)) : a) (yMaximo ((x , (y , z)) : a) 0)

constroiMapaAux :: [(Peca,Coordenadas)] -> Int -> Mapa
constroiMapaAux [] b = []
constroiMapaAux ((x , (y , z)) : a) b 
  | b >= 0 = constroiMapaAux ((x , (y , z)) : a) (b-1) ++ [listaPecas (compararCoordenadas  (vaziosParaComparar (xMaximo ((x , (y , z)) : a) 0) b) (linhaPorNumero (ordenarCoordenadas ((x , (y , z)) : a)) b))]
  | otherwise = []


desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa [] = []
desconstroiMapa (x : y) = desconstroiMapaAux (x : y) 0

desconstroiMapaAux :: Mapa -> Int -> [(Peca,Coordenadas)]
desconstroiMapaAux [] _ = []
desconstroiMapaAux (x : y) a = ordenarCoordenadas ((desconstroiLinha x 0 a) ++ (desconstroiMapaAux y (a + 1)))

--funções auxiliares

listaPecas :: [(Peca,Coordenadas)] -> [Peca]
listaPecas [] = []
listaPecas ((x , (y , z)) : a) = x : listaPecas a

ordenarCoordenadas :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenarCoordenadas [] = []
ordenarCoordenadas ((x , (y , z)) : a) = sortBy (compare `on` snd) ((x , (y , z)) : a)


--linhaPorNumero cria as linhas 1 a 1 


linhaPorNumero :: [(Peca,Coordenadas)] -> Int -> [(Peca,Coordenadas)]
linhaPorNumero [] b = []
linhaPorNumero ((x,(y,z)):a) b = if b == z then ordenarCoordenadas ((x,(y,z)) : linhaPorNumero a b) else ordenarCoordenadas (linhaPorNumero a b)

vaziosParaComparar :: Int -> Int -> [(Peca,Coordenadas)]
vaziosParaComparar 0 0 = [(Vazio , (0,0))]
vaziosParaComparar 0 y = ordenarCoordenadas((Vazio, (0 , y)) : (vaziosParaComparar 0 (y - 1)))
vaziosParaComparar x y = ordenarCoordenadas((Vazio, (x , y)) : (vaziosParaComparar (x - 1) y ))


compararCoordenadas :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
compararCoordenadas  [] _ = []
compararCoordenadas  ((x,(y,z)):a) [] = ((x,(y,z)):a)
compararCoordenadas  ((x , (y , z)) : t) ((a , (b , c)) : d)
  | (y , z) == (b , c) = (a , (b , c)): compararCoordenadas  t d
  | otherwise = (x , (y , z)) : compararCoordenadas t ((a , (b , c)) : d)



--[ (Porta, (0, 3)),(Bloco, (0, 4)), (Bloco, (1, 4)),(Bloco, (2, 4)),(Bloco, (3, 4)),(Bloco, (4, 4)),(Caixa, (4, 3)),(Bloco, (5, 4)),(Bloco, (6, 4)),(Bloco, (6, 3)),(Bloco, (6, 2)),(Bloco, (6, 1))]


desconstroiLinha :: [Peca] -> Int -> Int -> [(Peca,Coordenadas)]
desconstroiLinha [] x y= []
desconstroiLinha (Vazio : a) x y = desconstroiLinha a (x + 1) y
desconstroiLinha (x : y) a b = (x , (a , b)) : desconstroiLinha y (a + 1) b

