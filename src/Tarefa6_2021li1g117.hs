{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module Tarefa6_2021li1g117 where

import LI12122
import Tarefa4_2021li1g117
import Tarefa2_2021li1g117

--descobre qual o movimento ocorrido entre dois jogos

qualOMovimento :: Jogo -> Jogo -> Movimento 
qualOMovimento (Jogo m (Jogador (x , y) z w)) (Jogo n (Jogador (a , b) c d))
    | y > b = Trepar
    | w /= d = InterageCaixa 
    | c == Este = AndarDireita 
    | otherwise = AndarEsquerda 

--aplica a funçao anterior a uma lista de jogos consecutivos

quaisOsMovimentos :: [Jogo] -> [Movimento]
quaisOsMovimentos [] = []
quaisOsMovimentos [x] = []
quaisOsMovimentos (x : y : z) = qualOMovimento x y : quaisOsMovimentos (y : z)

--verifica se o jogador chega a porta num ultimo jogo de uma lista

verificarChegadaFinal :: [Jogo] -> Bool
verificarChegadaFinal (x : y) = verificarChegada (Jogo m (Jogador (a , b) c d)) (coordenadasPorta m) 
    where (Jogo m (Jogador (a , b) c d)) = last (x : y)


verificarChegada :: Jogo -> Coordenadas -> Bool
verificarChegada (Jogo l (Jogador (a,b) c d))  (x,y)
    | a == x && b == y = True
    | otherwise = False

coordenadasPorta :: Mapa -> Coordenadas
coordenadasPorta l = coordenadasPortaAux (desconstroiMapa l)

coordenadasPortaAux :: [(Peca , Coordenadas)] -> Coordenadas
coordenadasPortaAux ((x , (y , z)) : a)
    | x == Porta = (y , z)
    | otherwise = coordenadasPortaAux a



