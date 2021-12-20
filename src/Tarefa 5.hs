module Main where 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import LI12122 
import Outro (mapa1dojogo) 


data Estado = Estado { menu::Menu , game::Game }

data Game = Nada | PlayMapa1 | PlayMapa2 | PlayMapa3 
             
data Menu = OpcaoNovojogo | OpcaoContinuar | OpcaoEscolherMapa Mapas 
data Mapas = Mapa1 | Mapa2 | Mapa3 | SemMapa

{-
estadoInicial:: Estado 
estadoInicial = (0,0)

desenhaEstado:: Estado -> Picture 
desenhaEstado (x,y) = Translate x y poligono 
    where 
        poligono:: Picture
        poligono = Polygon [(0,0),(10,0),(10,10),(0,10),(0,0)]

reageTempo:: Float -> Estado -> Estado 
reageTempo n (x,y) = (x,y-0.3) 

fr :: Int 
fr = 50

dm :: Display 
dm = InWindow "Novo Jogo" (400,400) (0,0)
-}

window::Display 
window = InWindow "Jogo" (1000,1000) (0,0) 

background::Color 
background = greyN 0.8

fr::Int 
fr = 50 

estadoInicial:: Estado  
estadoInicial = Estado OpcaoNovojogo Nada

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
drawEstado (Estado OpcaoNovojogo Nada) = menuInicialNov 
drawEstado (Estado OpcaoContinuar Nada) = menuInicialCont 
drawEstado (Estado (OpcaoEscolherMapa SemMapa) Nada) = menuInicialEsc 
drawEstado (Estado (OpcaoEscolherMapa Mapa1) Nada) = escolherMapa1
drawEstado (Estado (OpcaoEscolherMapa Mapa2) Nada) = escolherMapa2
drawEstado (Estado (OpcaoEscolherMapa Mapa3) Nada) = escolherMapa3
drawEstado (Estado OpcaoNovojogo PlayMapa1) = playMapa1 

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado OpcaoNovojogo Nada)  = Estado OpcaoContinuar Nada
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado OpcaoContinuar Nada) = Estado OpcaoNovojogo Nada
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado OpcaoContinuar Nada) = Estado (OpcaoEscolherMapa SemMapa) Nada
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa SemMapa) Nada) = Estado OpcaoContinuar Nada 
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa SemMapa) Nada) = Estado (OpcaoEscolherMapa Mapa1) Nada
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada) = Estado (OpcaoEscolherMapa Mapa2) Nada
reageEvento (EventKey (SpecialKey KeyLeft ) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada) = Estado (OpcaoEscolherMapa Mapa1) Nada
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada) = Estado (OpcaoEscolherMapa Mapa3) Nada
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada) = Estado (OpcaoEscolherMapa Mapa1) Nada
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada) = Estado (OpcaoEscolherMapa SemMapa) Nada 
reageEvento (EventKey (Char 'f') Down _ _)  (Estado (OpcaoEscolherMapa Mapa2) Nada) = Estado (OpcaoEscolherMapa SemMapa) Nada
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada) = Estado (OpcaoEscolherMapa SemMapa) Nada                          
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado OpcaoNovojogo Nada) = Estado OpcaoNovojogo PlayMapa1  
reageEvento _ s = s 


--movejogador:: Jogo -> Movimento -> Jogo 
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
