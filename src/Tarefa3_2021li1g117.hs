{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- |
Module      : Tarefa3_2021li1g117
Description : Representação textual do jogo
Copyright   : João Pedro da Rocha Rodrigues <a100896@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g117 where

import LI12122

instance Show Jogo where
  show = undefined

mapaString :: Mapa -> String
mapaString l = juntarStrings (map linhaMapaString l) 
  where linhaMapaString :: [Peca]  -> String
        linhaMapaString [] = ""
        linhaMapaString (x : y)
          | x == Caixa = "C" ++ linhaMapaString y
          | x == Bloco = "X" ++ linhaMapaString y 
          | x == Porta = "P" ++ linhaMapaString y 
          | otherwise = " " ++ linhaMapaString y 

juntarStrings :: [String] -> String
juntarStrings [] = ""
juntarStrings (x : y) = x ++ juntarStrings y 