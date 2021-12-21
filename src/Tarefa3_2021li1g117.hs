{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- |
Module      : Tarefa3_2021li1g117
Description : Representação textual do jogo
Copyright   : João Pedro da Rocha Rodrigues <a100896@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g117 where

import LI12122
import Data.Time.Format.ISO8601 (yearFormat)

instance Show Jogo where
  show = jogoFinal

jogoFinal :: Jogo -> String 
jogoFinal (Jogo l (Jogador (a , b) c d)) = jogoFinalAux l (Jogador (a , b) c d)

jogoFinalAux :: Mapa -> Jogador -> String
jogoFinalAux l (Jogador (a , b) c d) = juntarStrings (map stringLetra (existeCaixa (jogadorMapa (mapaString l) (Jogador (a , b) c d)) (Jogador (a , b) c d)) )

-- Passar tudo para os caracteres pretendidos

stringLetra :: [String]  -> String
stringLetra [] = ""
stringLetra (x : y)
  | x == "Caixa" = "C" ++ stringLetra y
  | x == "Bloco" = "X" ++ stringLetra y
  | x == "Porta" = "P" ++ stringLetra y
  | x == "Vazio" = " " ++ stringLetra y
  | x == "JogadorEste" = ">" ++ stringLetra y
  | otherwise = "<" ++ stringLetra y

juntarStrings :: [String] -> String
juntarStrings [] = ""
juntarStrings (x : y) = x ++ "\n" ++ juntarStrings y

-- Passar o mapa todo para String 

mapaString :: Mapa -> [[String]]
mapaString [] = []
mapaString (x : y) = linhaString x : mapaString y

linhaString :: [Peca] -> [String]
linhaString [] = []
linhaString (x : y) = show x : linhaString y

-- Colocar o jogador no mapa

jogadorMapa :: [[String]] -> Jogador -> [[String]]
jogadorMapa [] _ = []
jogarMapa (x : y) (Jogador (a , b) c d)
  | b == 0 = (jogadorLinha x (Jogador (a , b) c d)) : y
  | otherwise = x : jogadorMapa y (Jogador (a , b - 1) c d)

jogadorLinha :: [String] -> Jogador -> [String]
jogadorLinha [] _ = []
jogadorLinha (x : y) (Jogador (a , b) c d)
  | a == 0 = if c == Este then "JogadorEste" : y else "JogadorOeste" : y
  | otherwise = x : jogadorLinha y (Jogador (a - 1 , b) c d)

-- Colocar caixa caso haja

existeCaixa :: [[String]] -> Jogador -> [[String]]
existeCaixa [] _ = []
existeCaixa (x : y) (Jogador (a , b) c d)
  | d == False = (x : y)
  | otherwise = caixaMapa (x : y) (Jogador (a , b) c d)

caixaMapa :: [[String]] -> Jogador ->  [[String]]
caixaMapa [] _ = []
caixaMapa (x : y) (Jogador (a , b) c d)
  | b - 1 == 0 = (caixaLinha x (Jogador (a , b) c d)) : y
  | otherwise = x : caixaMapa y (Jogador (a , b - 1) c d)

caixaLinha :: [String] -> Jogador -> [String]
caixaLinha [] _ = []
caixaLinha (x : y) (Jogador (a , b) c d)
  | a == 0 = "Caixa" : y
  | otherwise = x : caixaLinha y (Jogador (a - 1 , b) c d)