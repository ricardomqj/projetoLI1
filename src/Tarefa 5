module Main where 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

data Estado = Estado { menu::Menu , game::Game }

data Game = Nada | PlayMapa1 | PlayMapa2 | PlayMapa3 
             
data Menu = OpcaoNovojogo | OpcaoContinuar | OpcaoEscolherMapa Mapas 
data Mapas = Mapa1 | Mapa2 | Mapa3 | SemMapa

window::Display 
window = FullScreen 

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

drawEstado::Estado -> Picture
drawEstado (Estado OpcaoNovojogo Nada) = menuInicialNov 
drawEstado (Estado OpcaoContinuar Nada) = menuInicialCont 
drawEstado (Estado (OpcaoEscolherMapa SemMapa) Nada) = menuInicialEsc 
drawEstado (Estado (OpcaoEscolherMapa Mapa1) Nada) = escolherMapa1
drawEstado (Estado (OpcaoEscolherMapa Mapa2) Nada) = escolherMapa2
drawEstado (Estado (OpcaoEscolherMapa Mapa3) Nada) = escolherMapa3

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

reageTempo :: Float -> Estado -> Estado
reageTempo n s = s

main :: IO ()
main = play window background fr
       estadoInicial drawEstado reageEvento reageTempo 
