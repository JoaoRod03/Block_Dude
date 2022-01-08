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


correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos (Jogo l (Jogador (a , b) c d)) []
    = Jogo l (Jogador (a , b) c d)
correrMovimentos (Jogo l (Jogador (a , b) c d)) (x : y)
    = correrMovimentos (moveJogador (Jogo l (Jogador (a , b) c d)) x ) y


moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo l (Jogador (a , b) c d)) AndarEsquerda = andarEsquerda (Jogo l (Jogador (a , b) c d))
moveJogador (Jogo l (Jogador (a , b) c d)) AndarDireita = andarDireita (Jogo l (Jogador (a , b) c d))
moveJogador (Jogo l (Jogador (a , b) c d)) Trepar = treparPeca (Jogo l (Jogador (a , b) c d))
moveJogador (Jogo l (Jogador (a , b) c d)) InterageCaixa = interagirCaixa (Jogo l (Jogador (a , b) c d))

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

--Funcao para trepar caixa ou bloco

treparPeca :: Jogo -> Jogo
treparPeca (Jogo l (Jogador (a , b) c d))
    | d == False = treparPecaVazio (desconstroiMapa l) (Jogador (a , b) c d)
    | d == True = treparPecaCaixa (desconstroiMapa l) (Jogador (a , b) c d)

treparPecaVazio :: [(Peca , Coordenadas)] -> Jogador -> Jogo
treparPecaVazio ((x ,(y , z)) : w) (Jogador (a , b) c d)
    | c == Este =
        if ((Bloco , (a + 1 , b)) `elem` ((x ,(y , z)) : w)) || ((Caixa , (a + 1 , b)) `elem` ((x ,(y , z)) : w))
        then  Jogo (constroiMapa ((x ,(y , z)) : w)) (Jogador (a + 1 , b - 1) c d)
        else Jogo  (constroiMapa ((x ,(y , z)) : w)) (Jogador (a , b) c d)
    | c == Oeste =
        if ((Bloco , (a - 1 , b)) `elem` ((x ,(y , z)) : w)) || ((Caixa , (a - 1 , b)) `elem` ((x ,(y , z)) : w))
        then  Jogo (constroiMapa ((x ,(y , z)) : w)) (Jogador (a - 1 , b - 1) c d)
        else Jogo  (constroiMapa ((x ,(y , z)) : w)) (Jogador (a , b) c d)

treparPecaCaixa :: [(Peca , Coordenadas)] -> Jogador -> Jogo
treparPecaCaixa ((x ,(y , z)) : w) (Jogador (a , b) c d)
    | c == Este
        && ((Bloco, (a + 1 , b)) `elem` ((x ,(y , z)) : w) || (Caixa , (a + 1 , b)) `elem` ((x ,(y , z)) : w) ) =
            if (Bloco , (a + 1, b - 2)) `elem` ((x ,(y , z)) : w)
            then Jogo (constroiMapa ((x ,(y , z)) : w)) (Jogador (a , b) c d)
            else  Jogo (constroiMapa ((x ,(y , z)) : w)) (Jogador (a + 1 , b - 1) c d)
    | c == Oeste
        && ((Bloco, (a - 1 , b)) `elem` ((x ,(y , z)) : w) || (Caixa , (a - 1 , b)) `elem` ((x ,(y , z)) : w) ) =
            if (Bloco , (a - 1, b - 2)) `elem` ((x ,(y , z)) : w)
            then Jogo (constroiMapa ((x ,(y , z)) : w)) (Jogador (a , b) c d)
            else Jogo (constroiMapa ((x ,(y , z)) : w)) (Jogador (a - 1 , b - 1) c d)

-- Funcao para interagir com a caixa 

interagirCaixa :: Jogo -> Jogo
interagirCaixa (Jogo l (Jogador (a , b) c d))
    | d == False = pegarCaixa (desconstroiMapa l) (Jogador (a , b) c d)
    | d == True = largarCaixa (desconstroiMapa l) (Jogador (a , b) c d)

pegarCaixa :: [(Peca , Coordenadas)] -> Jogador -> Jogo
pegarCaixa ((x ,(y , z)) : w) (Jogador (a , b) c d)
    | c == Este
        && (Caixa , (a + 1 , b)) `elem` ((x ,(y , z)) : w) =
            if ((Bloco, (a + 1, b - 1)) `elem` ((x ,(y , z)) : w) || (Bloco, (a, b - 1)) `elem` ((x ,(y , z)) : w))
            then Jogo (constroiMapa ((x ,(y , z)) : w)) (Jogador (a , b) c d)
            else Jogo (constroiMapa (tirarCaixaEste ((x ,(y , z)) : w) (Jogador (a , b) c d) )) (Jogador (a , b) c True)
    | c == Oeste
        && (Caixa , (a - 1 , b)) `elem` ((x ,(y , z)) : w) =
            if ((Bloco, (a - 1, b - 1)) `elem` ((x ,(y , z)) : w) || (Bloco, (a, b - 1)) `elem` ((x ,(y , z)) : w))
            then Jogo (constroiMapa ((x ,(y , z)) : w)) (Jogador (a , b) c d)
            else Jogo (constroiMapa (tirarCaixaOeste ((x ,(y , z)) : w) (Jogador (a , b) c d) )) (Jogador (a , b) c True)

tirarCaixaEste :: [(Peca , Coordenadas)] -> Jogador -> [(Peca , Coordenadas)]
tirarCaixaEste [] _ = []
tirarCaixaEste ((x ,(y , z)) : w) (Jogador (a , b) c d)
    | x == Caixa =
        if y == a + 1 && z == b
        then w
        else (x ,(y , z)) : tirarCaixaEste w (Jogador (a , b) c d)
    | otherwise = (x ,(y , z)) : tirarCaixaEste w (Jogador (a , b) c d)

tirarCaixaOeste :: [(Peca , Coordenadas)] -> Jogador -> [(Peca , Coordenadas)]
tirarCaixaOeste [] _ = []
tirarCaixaOeste ((x ,(y , z)) : w) (Jogador (a , b) c d)
    | x == Caixa =
        if y == a - 1 && z == b
        then w
        else (x ,(y , z)) : tirarCaixaOeste w (Jogador (a , b) c d)
    | otherwise = (x ,(y , z)) : tirarCaixaOeste w (Jogador (a , b) c d)

cairCaixa :: (Peca , Coordenadas) -> [(Peca , Coordenadas)] -> [(Peca , Coordenadas)]
cairCaixa (Caixa , (x , y)) ((a , (b , c)) : d)
    | (Bloco , (x , y + 1)) `elem` ((a , (b , c)) : d)
        || (Caixa , (x , y + 1)) `elem` ((a , (b , c)) : d)
        = ordenarCoordenadas ((Caixa , (x , y)) : ((a , (b , c)) : d))
    | otherwise = cairCaixa (Caixa , (x , y + 1)) ((a , (b , c)) : d)

largarCaixa :: [(Peca , Coordenadas)] -> Jogador -> Jogo
largarCaixa ((x ,(y , z)) : w) (Jogador (a , b) c d)
    | c == Oeste = largarCaixaOeste ((x ,(y , z)) : w) (Jogador (a , b) c d)
    | c == Este = largarCaixaEste ((x ,(y , z)) : w) (Jogador (a , b) c d)

largarCaixaEste :: [(Peca , Coordenadas)] -> Jogador -> Jogo
largarCaixaEste ((x ,(y , z)) : w) (Jogador (a , b) c d)
    | (Bloco, (a + 1 , b)) `elem` ((x ,(y , z)) : w)
        || (Caixa, (a + 1 , b)) `elem` ((x ,(y , z)) : w) =
            if (Bloco, (a + 1 , b - 1)) `elem` ((x ,(y , z)) : w)
                || (Caixa, (a + 1 , b - 1)) `elem` ((x ,(y , z)) : w)
            then Jogo (constroiMapa ((x ,(y , z)) : w)) (Jogador (a , b) c True)
            else Jogo (constroiMapa (cairCaixa (Caixa, (a + 1 , b - 1)) ((x ,(y , z)) : w))) (Jogador (a , b) c False)
    | otherwise = Jogo (constroiMapa (cairCaixa (Caixa, (a + 1 , b)) ((x ,(y , z)) : w))) (Jogador (a , b) c False)

largarCaixaOeste :: [(Peca , Coordenadas)] -> Jogador -> Jogo
largarCaixaOeste ((x ,(y , z)) : w) (Jogador (a , b) c d)
    | (Bloco, (a - 1 , b)) `elem` ((x ,(y , z)) : w)
        || (Caixa, (a - 1 , b)) `elem` ((x ,(y , z)) : w) =
            if (Bloco, (a - 1 , b - 1)) `elem` ((x ,(y , z)) : w)
                || (Caixa, (a - 1 , b - 1)) `elem` ((x ,(y , z)) : w)
            then Jogo (constroiMapa ((x ,(y , z)) : w)) (Jogador (a , b) c True)
            else Jogo (constroiMapa (cairCaixa (Caixa, (a - 1 , b - 1)) ((x ,(y , z)) : w))) (Jogador (a , b) c False)
    | otherwise = Jogo (constroiMapa (cairCaixa (Caixa, (a - 1 , b)) ((x ,(y , z)) : w))) (Jogador (a , b) c False)

