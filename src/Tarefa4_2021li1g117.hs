{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- |
Module      : Tarefa4_2021li1g117
Description : Movimentação do personagem
Copyright   : João Pedro da Rocha Rodrigues <a100896@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g117 where

import LI12122
import Tarefa2_2021li1g117 
import Tarefa3_2021li1g117


--moveJogador :: Jogo -> Movimento -> Jogo
--moveJogador jogo movimento = undefined

--correrMovimentos :: Jogo -> [Movimento] -> Jogo
--correrMovimentos jogo movimentos = undefined

moveJogador :: Jogo -> Movimento -> Jogo 
moveJogador (Jogo l (Jogador (a , b) c d)) AndarEsquerda = andarEsquerda (Jogo l (Jogador (a , b) c d))
moveJogador (Jogo l (Jogador (a , b) c d)) AndarDireita = andarDireita (Jogo l (Jogador (a , b) c d))

--Funcao para definir onde o jogador cai

cairJogador :: [(Peca , Coordenadas)] -> Jogador -> Jogo  
cairJogador ((x ,(y , z)) : w) (Jogador (a , b) c d)
    | (Bloco , (a , b + 1)) `elem` ((x ,(y , z)) : w) = Jogo (constroiMapa ((x ,(y , z)) : w)) (Jogador (a , b) c d)
    | otherwise = cairJogador ((x ,(y , z)) : w) (Jogador (a , b + 1) c d)

--Funcoes para andar a esquerda

andarEsquerda :: Jogo -> Jogo 
andarEsquerda (Jogo l (Jogador (a , b) c d)) = andarEsquerdaAux (desconstroiMapa l) (Jogador (a , b) c d)

andarEsquerdaAux :: [(Peca , Coordenadas)] -> Jogador -> Jogo
andarEsquerdaAux ((x ,(y , z)) : w) (Jogador (a , b) c d)
    | (Bloco , (a - 1 , b)) `elem` ((x ,(y , z)) : w) 
        || (Caixa , (a - 1 , b)) `elem` ((x ,(y , z)) : w) 
        = Jogo (constroiMapa ((x ,(y , z)) : w)) (Jogador (a , b) Oeste d)
    | d == True = 
        if (Bloco , (a - 1, b - 1)) `elem` ((x ,(y , z)) : w) 
        then Jogo (constroiMapa ((x ,(y , z)) : w)) (Jogador (a , b) Oeste True) 
        else cairJogador ((x ,(y , z)) : w) (Jogador (a - 1, b) Oeste True)
    | otherwise = cairJogador ((x ,(y , z)) : w) (Jogador (a - 1, b) Oeste d)

--Funcoes para andar a direita

andarDireita :: Jogo -> Jogo 
andarDireita (Jogo l (Jogador (a , b) c d)) = andarDireitaAux (desconstroiMapa l) (Jogador (a , b) c d)

andarDireitaAux :: [(Peca , Coordenadas)] -> Jogador -> Jogo
andarDireitaAux ((x ,(y , z)) : w) (Jogador (a , b) c d)
    | (Bloco , (a + 1 , b)) `elem` ((x ,(y , z)) : w) 
        || (Caixa , (a + 1 , b)) `elem` ((x ,(y , z)) : w) 
        = Jogo (constroiMapa ((x ,(y , z)) : w)) (Jogador (a , b) Este d)
    | d == True = 
        if (Bloco , (a + 1, b - 1)) `elem` ((x ,(y , z)) : w) 
        then Jogo (constroiMapa ((x ,(y , z)) : w)) (Jogador (a , b) Este True) 
        else cairJogador ((x ,(y , z)) : w) (Jogador (a + 1, b) Este True)
    | otherwise = cairJogador ((x ,(y , z)) : w) (Jogador (a + 1, b) Este d)

--Funcao para subir caixa ou bloco

