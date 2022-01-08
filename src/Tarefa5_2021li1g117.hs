


{- |
Module      : Tarefa5_2021li1gXXX
Description : Aplicação Gráfica

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game
import Tarefa1_2021li1g117
import Tarefa2_2021li1g117
import Tarefa3_2021li1g117
import Tarefa4_2021li1g117
import LI12122
import Mapa
import Data.Maybe



type Mundo = (Menu, [(Elementos, Picture )])

data Menu = MenuInicial OpcaoInicial
            | MenuJogo Jogo
            | MenuFinal OpcaoFinal
            deriving (Eq, Show)


data OpcaoInicial = Jogar deriving (Eq, Show, Read)

data OpcaoFinal = Sair deriving (Eq, Show, Read)

data Elementos = Blocos 
                | Vazios 
                | Caixas 
                | Portas 
                | FundoInicial 
                | FundoJogo 
                | FundoFinal 
                | JogadorEste 
                | JogadorOeste 
                    deriving Eq


dm :: Display
dm = InWindow "BlockDude" (1000 , 800) (0 , 0) 


fr :: Int
fr = 50

main :: IO ()
main = do
     bloco <- loadBMP "Imagensbmp/bloco.bmp"
     vazio <- loadBMP "Imagensbmp/vazio.bmp"
     caixa <- loadBMP "Imagensbmp/caixa.bmp"
     porta <- loadBMP "Imagensbmp/porta.bmp"
     jogadorEste <- loadBMP "Imagensbmp/jogadorEste.bmp"
     jogadorOeste <- loadBMP "Imagensbmp/jogadorOeste.bmp"
     fundoInicial <- loadBMP "Imagensbmp/fundoInicial.bmp"
     fundoJogo <- loadBMP "Imagensbmp/fundoJogo.bmp"
     fundoFinal <- loadBMP "Imagensbmp/fundoFinal.bmp"
     let i = [(Blocos, bloco) , (Vazios, vazio) , (Caixas, caixa), (Portas, porta), (FundoInicial, fundoInicial), (FundoJogo, fundoJogo), (FundoFinal, fundoFinal), (JogadorEste, jogadorEste), (JogadorOeste, jogadorOeste)]
     let começo = (MenuInicial Jogar, i)
     play dm
         (greyN 0.5)
         fr
         começo
         mundoPicture
         reageEvento
         reageTempo


reageEvento :: Event -> Mundo -> Mundo
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial _, i) = (MenuInicial Jogar, i)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Jogar, i) = (MenuJogo (Jogo mapaNivel (Jogador (8,6) Oeste False)), i)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (MenuJogo (Jogo l (Jogador (a,b) c d)), i) =
    if verificarChegada  (moveJogador (Jogo l (Jogador (a,b) c d)) Trepar) (coordenadasPorta l)
    then (MenuFinal Sair, i)
    else (MenuJogo (moveJogador (Jogo l (Jogador (a,b) c d)) Trepar), i)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (MenuJogo (Jogo l (Jogador (a,b) c d)), i) =
    if verificarChegada (moveJogador (Jogo l (Jogador (a,b) c d)) AndarEsquerda) (coordenadasPorta l)
    then (MenuFinal Sair, i)
    else (MenuJogo (moveJogador (Jogo l (Jogador (a,b) c d)) AndarEsquerda), i)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (MenuJogo (Jogo l (Jogador (a,b) c d)), i) =
    if verificarChegada (moveJogador (Jogo l (Jogador (a,b) c d)) AndarDireita) (coordenadasPorta l)
    then (MenuFinal Sair, i)
    else (MenuJogo (moveJogador (Jogo l (Jogador (a,b) c d)) AndarDireita), i)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (MenuJogo (Jogo l (Jogador (a,b) c d)), i) =
    (MenuJogo (moveJogador (Jogo l (Jogador (a,b) c d)) InterageCaixa), i)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuFinal Sair, i) =
    (MenuInicial Jogar, i)
reageEvento _ l = l

coordenadasPorta :: Mapa -> Coordenadas
coordenadasPorta l = coordenadasPortaAux (desconstroiMapa l)

coordenadasPortaAux :: [(Peca , Coordenadas)] -> Coordenadas
coordenadasPortaAux [] = (0 , 0)
coordenadasPortaAux ((x , (y , z)) : a)
    | x == Porta = (y , z)
    | otherwise = coordenadasPortaAux a

verificarChegada :: Jogo -> Coordenadas -> Bool
verificarChegada (Jogo l (Jogador (a,b) c d))  (x , y)
    | a == x && b == y = True
    | otherwise = False

----------------------------

--atribui Pictures ao mundo

mundoPicture :: Mundo -> Picture
mundoPicture (MenuInicial Jogar, i) = fromJust (lookup FundoInicial i)
mundoPicture (MenuFinal Sair, i) = fromJust (lookup FundoFinal i)
mundoPicture ((MenuJogo (Jogo mapa (Jogador (a,b) c d))) , i) =
    Pictures ([fromJust (lookup FundoJogo i)] ++ (mapaPicture mapa (0 , 0) i) ++ (jogadorPicture mapa (0 , 0) (Jogador (a,b) c d) i))

--cria o mapa de Pictures 

mapaPicture :: Mapa -> (Int,Int) -> [(Elementos, Picture )] -> [Picture]
mapaPicture [] _ _ = []
mapaPicture (x : y) (a , b) i = mapaPictureAux x (a , b) i ++ mapaPicture y (0,b + 1) i

mapaPictureAux :: [Peca] -> (Int,Int) -> [(Elementos, Picture )] -> [Picture]
mapaPictureAux [] _ _ = []
mapaPictureAux (a : b) (x , y) i
    | a == Vazio 
    = (Translate q w $ (fromJust (lookup Vazios i))) : mapaPictureAux b (x + 1 , y) i
    | a == Bloco 
    = (Translate q w $ (fromJust (lookup Blocos i))) : mapaPictureAux b (x + 1 , y) i
    | a == Porta 
    = (Translate q w $ (fromJust (lookup Portas i))) : mapaPictureAux b (x + 1 , y) i
    | a == Caixa 
    = (Translate q w $ (fromJust (lookup Caixas i))) : mapaPictureAux b (x + 1 , y) i
            where  q = (tilesize*(fromIntegral x))
                   w = negate (tilesize*(fromIntegral y))
mapaPictureAux (_ : _) (_ , _) _ = []

tilesize :: Float
tilesize = 30


--desenha o jogador

jogadorPicture :: Mapa -> (Int,Int) -> Jogador  -> [(Elementos, Picture)] -> [Picture]
jogadorPicture [] _ _ _ = []
jogadorPicture (q:e) (x,y) (Jogador (a,b) c d) i = jogadorPictureAux q (x,y) (Jogador (a,b) c d) i ++ jogadorPicture e (x,y+1) (Jogador (a,b) c d) i

jogadorPictureAux :: [Peca] -> (Int,Int) -> Jogador -> [(Elementos, Picture)] -> [Picture]
jogadorPictureAux [] _ _  _= []
jogadorPictureAux (_ : t) (x , y) (Jogador (a,b) c d) i
    | x == a 
        && y == b 
        && c == Este 
        = [Translate q w $ (fromJust (lookup  JogadorEste i))]
    | x == a 
        && y == b 
        && c == Oeste 
        = [Translate q w $ (fromJust (lookup JogadorOeste i))]
    | otherwise = jogadorPictureAux t (x + 1 , y) (Jogador (a,b) c d) i
            where  q = (tilesize * (fromIntegral x))
                   w = negate (tilesize * (fromIntegral y))


------------------------


reageTempo :: Float -> Mundo -> Mundo
reageTempo a b = b