module Main where 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import LI12122 
import Outro (mapa1dojogo) 
import Tarefa4_2021li1g063 (moveJogador)


data Estado = Estado { menu::Menu , game::Game ,  jogo::Jogo }

data Game = Nada | Play Mapas | Alterado 

--data Movs = AndarD | AndarE | Trep | Inter
             
data Menu = OpcaoNovojogo | OpcaoContinuar | OpcaoEscolherMapa Mapas 
data Mapas = Mapa1 | Mapa2 | Mapa3 | SemMapa

window::Display 
window = InWindow "Jogo" (1000,1000) (0,0) 

background::Color 
background = greyN 0.8

fr::Int 
fr = 50 

estadoInicial:: Estado  
estadoInicial = Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))

-- | Barras de Opção no Menu principal
barraOpcaoCont::Picture
barraOpcaoCont = rectangleSolid 500 150 

barraOpcaoNov::Picture
barraOpcaoNov = Translate 0 200 barraOpcaoCont

barraOpcaoEsc::Picture
barraOpcaoEsc = Translate 0 (-200) barraOpcaoCont  

-- | Nome das barras 
textCont:: Picture 
textCont = color white  (Translate (-150) (-10) (scale 0.5 0.5 (Text "Continuar"))) 

textNov::Picture 
textNov = color white (Translate (-150) 190 (scale 0.5 0.5 (Text "Novo Jogo")))

textEsc::Picture 
textEsc = color white (Translate (-230) (-210) (scale 0.5 0.5 (Text "Escolher Mapa")))


-- | Mapas disoníveis para opção
mapa1::Picture 
mapa1 = translate (-150) 150 (Polygon [(-75,75),(75,75),(75,-75),(-75,-75),(-75,75)])

mapa2::Picture 
mapa2 = translate 150 150 (Polygon [(-75,75),(75,75),(75,-75),(-75,-75),(-75,75)])

mapa3::Picture 
mapa3 = translate (-150) (-150) (Polygon [(-75,75),(75,75),(75,-75),(-75,-75),(-75,75)])

-- | Pictures do Menu inicial
menuInicialNov:: Picture 
menuInicialNov = pictures [barraOpcaoCont,barraOpcaoNov,barraOpcaoEsc,color blue barraOpcaoNov,textCont,textNov,textEsc] 

menuInicialCont:: Picture 
menuInicialCont = pictures [barraOpcaoCont,barraOpcaoNov,barraOpcaoEsc,color blue barraOpcaoCont,textCont,textEsc,textNov] 

menuInicialEsc:: Picture 
menuInicialEsc = pictures [barraOpcaoCont,barraOpcaoNov,barraOpcaoEsc,color blue barraOpcaoEsc,textCont,textEsc,textNov] 

-- | Pictures do Escolher Mapa
escolherMapa1:: Picture 
escolherMapa1 = pictures [mapa1,mapa2,mapa3,color blue mapa1]

escolherMapa2::Picture 
escolherMapa2 = pictures [mapa1,mapa2,mapa3,color blue mapa2]

escolherMapa3::Picture 
escolherMapa3 = pictures [mapa1,mapa2,mapa3,color blue mapa3]

-- | Pictures Mapas

playMapa1::Picture 
playMapa1 = pictures (treatGame(Jogo mapa1dojogo (Jogador (3,3) Este False)))

drawEstado::Estado -> Picture
drawEstado (Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = menuInicialNov 
drawEstado (Estado OpcaoContinuar Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = menuInicialCont 
drawEstado (Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = menuInicialEsc 
drawEstado (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = escolherMapa1
drawEstado (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = escolherMapa2
drawEstado (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = escolherMapa3
drawEstado (Estado OpcaoNovojogo (Play Mapa1) (Jogo mapa1dojogo (Jogador (3,3) Este False))) = playMapa1 
drawEstado (Estado OpcaoNovojogo Alterado (Jogo mapa1dojogo (Jogador (x,y) dir tf))) = pictures (treatGame (Jogo mapa1dojogo (Jogador (x,y) dir tf)))


reageEvento :: Event -> Estado -> Estado
-- Menu Principal
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (3,3) Este False)))  = Estado OpcaoContinuar Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado OpcaoContinuar Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado OpcaoContinuar Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado OpcaoContinuar Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
-- Menu Escolher Mapa
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyLeft ) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (Char 'f') Down _ _)  (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))                      
-- Começar Jogo Opção NovoJogo 
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado OpcaoNovojogo (Play Mapa1)  (Jogo mapa1dojogo (Jogador (3,3) Este False))
-- | Movimentos
--Andar Esquerda 
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado OpcaoNovojogo (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este tf))) =  Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado OpcaoNovojogo Alterado (Jogo mapa1dojogo (Jogador (x,y) dir tf))) =  Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda)
-- Andar Direita
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado OpcaoNovojogo (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este tf))) =  Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este tf)) AndarDireita )
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado OpcaoNovojogo Alterado (Jogo mapa1dojogo (Jogador (x,y) dir tf))) =  Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este tf)) AndarDireita)
-- Trepar
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado OpcaoNovojogo (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este tf))) = Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este tf)) Trepar)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado OpcaoNovojogo Alterado (Jogo mapa1dojogo (Jogador (x,y) Este tf))) = Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este tf)) Trepar)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado OpcaoNovojogo (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Oeste tf))) = Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste tf)) Trepar)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado OpcaoNovojogo Alterado (Jogo mapa1dojogo (Jogador (x,y) Oeste tf))) = Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste tf)) Trepar)
-- Interage Caixa
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado OpcaoNovojogo (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este False))) = Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este False)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado OpcaoNovojogo Alterado (Jogo mapa1dojogo (Jogador (x,y) Este False))) = Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este False)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado OpcaoNovojogo (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este True))) = Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este True)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado OpcaoNovojogo Alterado (Jogo mapa1dojogo (Jogador (x,y) Este True))) = Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este True)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado OpcaoNovojogo (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Oeste False))) = Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste False)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado OpcaoNovojogo Alterado (Jogo mapa1dojogo (Jogador (x,y) Oeste False))) = Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste False)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado OpcaoNovojogo (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Oeste True))) = Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste True)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado OpcaoNovojogo Alterado (Jogo mapa1dojogo (Jogador (x,y) Oeste True))) = Estado OpcaoNovojogo Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste True)) InterageCaixa)
reageEvento _ s = s 


{-
playGame:: Estado -> Picture
playGame (Estado OpcaoNovojogo Alterado (Jogo m (Jogador (x,y) dir tf))) = pictures (treatGame (moveJogador (Jogo m (Jogador (x,y) dir tf)) AndarEsquerda))
-}

treatGame:: Jogo -> [Picture]
treatGame j = transf j (-450,450) (0,0)

transf:: Jogo -> (Float,Float) -> (Int,Int) -> [Picture]
transf (Jogo [] _ ) _  _ = []
transf (Jogo [h] (Jogador (x,y) d tf)) (a,b) (cx,cy) = drawMap h (Jogador (x,y) d tf) a b (cx,cy)
transf (Jogo (h:t) (Jogador (x,y) d tf)) (a,b) (cx,cy) = drawMap h (Jogador (x,y) d tf) a b (cx,cy) ++ transf (Jogo t (Jogador (x,y) d tf)) (-450,b-100) (0,cy+1) 

-- | Desenha os mapas em uma lista de pictures 
drawMap:: [Peca] -> Jogador -> Float -> Float -> (Int,Int) -> [Picture] 
drawMap [h] (Jogador (x,y) d tf) a b (cx,cy)
        | x == cx && y == cy = [Translate a b (circle 10)] 
        | h == Bloco = [Translate a b (Polygon [(-50,50),(50,50),(50,-50),(-50,-50),(-50,50)])] 
        | h == Vazio = [Translate a b Blank] 
        | h == Porta = [Translate a b (Polygon [(-50,50),(0,0),(-50,-50),(-50,50)])] 
        | h == Caixa = [color blue (Translate a b (Polygon [(-50,50),(0,0),(-50,-50),(-50,50)]))]
--drawMap ([]:ys) a b  = drawMap ys (-450) (b-100)
drawMap (h:t) (Jogador (x,y) d tf) a b  (cx,cy)
        | tf && cy == y-1 && cx == x = color red (Translate a b (Polygon [(-50,50),(50,50),(50,-50),(-50,-50),(-50,50)])) : drawMap t (Jogador (x,y) d tf) (a + 100) b (cx + 1,cy)
        | x == cx && y == cy = Translate a b (circle 10) : drawMap t (Jogador (x,y) d tf) (a + 100) b (cx + 1,cy)
        | h == Bloco = Translate a b (Polygon [(-50,50),(50,50),(50,-50),(-50,-50),(-50,50)]) : drawMap t (Jogador (x,y) d tf) (a + 100) b (cx + 1,cy)
        | h == Vazio = Translate a b Blank : drawMap t (Jogador (x,y) d tf) (a + 100) b (cx + 1,cy)
        | h == Porta = Translate a b (Polygon [(-50,50),(0,0),(-50,-50),(-50,50)]) : drawMap t (Jogador (x,y) d tf) (a + 100) b (cx + 1,cy)
        | h == Caixa = color blue (Translate a b (Polygon [(-50,50),(50,50),(50,-50),(-50,-50),(-50,50)])) : drawMap t (Jogador (x,y) d tf) (a + 100) b (cx + 1,cy)


reageTempo :: Float -> Estado -> Estado
reageTempo n s = s

main :: IO ()
main = play window background fr
       estadoInicial drawEstado reageEvento reageTempo 
