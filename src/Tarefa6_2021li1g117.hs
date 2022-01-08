


module Tarefa6_2021li1g117 where

import LI12122
import Tarefa4_2021li1g117
import Tarefa2_2021li1g117
import Data.List (sortOn)

--função final

resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo movimentos (Jogo l (Jogador (a , b) c d))
    | length tentativasBemSucedidas == 0 = Nothing 
    | otherwise = Just (quaisOsMovimentos (head q))
                    where q = (sortOn length tentativasBemSucedidas)
                          tentativasBemSucedidas = filter verificarChegadaFinal (testaTentativas movimentos (Jogo l (Jogador (a,b) c d)))



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
verificarChegadaFinal [] = False 
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


--cria e testa possíveis tentativas conforme os movimentos

criaTentativas :: Jogo -> [Jogo] 
criaTentativas = criaTentativasAux [AndarEsquerda, 
                                    AndarDireita,  
                                    Trepar, 
                                    InterageCaixa]
    
criaTentativasAux :: [Movimento] -> Jogo -> [Jogo]
criaTentativasAux [] _ = []
criaTentativasAux (x : y) (Jogo l (Jogador (a,b) c d)) =
        if z /= (Jogo l (Jogador (a,b) c d)) 
        then z : criaTentativasAux y (Jogo l (Jogador (a,b) c d))
        else criaTentativasAux y (Jogo l (Jogador (a,b) c d))
            where z = moveJogador (Jogo l (Jogador (a,b) c d)) x 



testaTentativas :: Int -> Jogo -> [[Jogo]]
testaTentativas movimentos (Jogo l (Jogador (a,b) c d)) =
    if movimentos <= 0 
    then [[(Jogo l (Jogador (a,b) c d))]] ------ caso nao seja possivel retorna a lista com o jogo inicial
    else  [(Jogo l (Jogador (a,b) c d))] : map ((Jogo l (Jogador (a,b) c d)) :) (concatMap (testaTentativas (movimentos - 1)) (criaTentativas (Jogo l (Jogador (a,b) c d))))
--listas compostas do jogo inicial e dos jogos que vao leva-lo à vitoria 
