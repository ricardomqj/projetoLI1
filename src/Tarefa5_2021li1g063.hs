{- |
Module      : Tarefa4_2021li1g063
Description : Movimentação do personagem
Copyright   : Ricardo Miguel Queirós de Jesus <a100066@alunos.uminho.pt>;
            : Rui Pinto <a100659@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}

module Main where 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


data Estado = Estado { menu::Menu , game::Game }

data Game = Nada | PlayMapa1 | PlayMapa2 | PlayMapa3 | PlayMapa4 | PlayMapa5 | PlayMapa6
             
data Menu = OpcaoNovojogo | OpcaoContinuar | OpcaoEscolherMapa Mapas 
data Mapas = Mapa1 | Mapa2 | Mapa3 | Mapa4 | Mapa5 | Mapa6 | SemMapa

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

textMapa1::Picture 
textMapa1 = color white (Translate (-620) 225 (scale 0.3 0.3 (Text "Nivel 1")))

textMapa2 :: Picture 
textMapa2 = color white (Translate (-620) 25 (scale 0.3 0.3 (Text "Nivel 2")))

textMapa3 :: Picture 
textMapa3 = color white (Translate (-620) (-165) (scale 0.3 0.3 (Text "Nivel 3")))

textMapa4 :: Picture 
textMapa4 = color white (Translate 350 225 (scale 0.3 0.3 (Text "Nivel 4")))

textMapa5 :: Picture 
textMapa5 = color white (Translate 350 25 (scale 0.3 0.3 (Text "Nivel 5")))

textMapa6 :: Picture 
textMapa6 = color white (Translate 350 (-165) (scale 0.3 0.3 (Text "Nivel 6")))

-- | Mapas disponíveis para opção
mapa1 :: Picture 
mapa1 = translate (-650) 250 (Polygon [(-75,75),(300,75),(300,-75),(-75,-75),(-75,75)])

mapa2 :: Picture 
mapa2 = translate (-650) 50 (Polygon [(-75,75),(300,75),(300,-75),(-75,-75),(-75,75)])

mapa3 :: Picture 
mapa3 = translate (-650) (-150) (Polygon [(-75,75),(300,75),(300,-75),(-75,-75),(-75,75)])

mapa4 :: Picture 
mapa4 = translate (350) (250) (Polygon [(-75,75),(300,75),(300,-75),(-75,-75),(-75,75)])

mapa5 :: Picture 
mapa5 = translate (350) (50) (Polygon [(-75,75),(300,75),(300,-75),(-75,-75),(-75,75)])

mapa6 :: Picture 
mapa6 = translate (350) (-150) (Polygon [(-75,75),(300,75),(300,-75),(-75,-75),(-75,75)])


-- | Pictures do Menu inicial
menuInicialNov:: Picture 
menuInicialNov = pictures [barraOpcaoCont,barraOpcaoNov,barraOpcaoEsc,color blue barraOpcaoNov,textCont,textNov,textEsc] 

menuInicialCont:: Picture 
menuInicialCont = pictures [barraOpcaoCont,barraOpcaoNov,barraOpcaoEsc,color blue barraOpcaoCont,textCont,textEsc,textNov] 

menuInicialEsc:: Picture 
menuInicialEsc = pictures [barraOpcaoCont,barraOpcaoNov,barraOpcaoEsc,color blue barraOpcaoEsc,textCont,textEsc,textNov] 

-- | Pictures do Escolher Mapa
escolherMapa1:: Picture 
escolherMapa1 = pictures [mapa1,mapa2,mapa3, mapa4, mapa5, mapa6, color blue mapa1, textMapa1, textMapa2, textMapa3, textMapa4, textMapa5, textMapa6]

escolherMapa2::Picture 
escolherMapa2 = pictures [mapa1,mapa2,mapa3, mapa4, mapa5, mapa6, color blue mapa2, textMapa1, textMapa2, textMapa3, textMapa4, textMapa5, textMapa6]

escolherMapa3::Picture 
escolherMapa3 = pictures [mapa1,mapa2,mapa3, mapa4, mapa5, mapa6, color blue mapa3, textMapa1, textMapa2, textMapa3, textMapa4, textMapa5, textMapa6]

escolherMapa4 :: Picture 
escolherMapa4 = pictures [mapa1, mapa2, mapa3, mapa4, mapa5, mapa6, color blue mapa4, textMapa1, textMapa2, textMapa3, textMapa4, textMapa5, textMapa6]

escolherMapa5 :: Picture 
escolherMapa5 = pictures [mapa1, mapa2, mapa3, mapa4, mapa5, mapa6, color blue mapa5, textMapa1, textMapa2, textMapa3, textMapa4, textMapa5, textMapa6]

escolherMapa6 :: Picture 
escolherMapa6 = pictures [mapa1, mapa2, mapa3, mapa4, mapa5, mapa6, color blue mapa6, textMapa1, textMapa2, textMapa3, textMapa4, textMapa5, textMapa6]

drawEstado::Estado -> Picture
drawEstado (Estado OpcaoNovojogo Nada) = menuInicialNov 
drawEstado (Estado OpcaoContinuar Nada) = menuInicialCont 
drawEstado (Estado (OpcaoEscolherMapa SemMapa) Nada) = menuInicialEsc 
drawEstado (Estado (OpcaoEscolherMapa Mapa1) Nada) = escolherMapa1
drawEstado (Estado (OpcaoEscolherMapa Mapa2) Nada) = escolherMapa2
drawEstado (Estado (OpcaoEscolherMapa Mapa3) Nada) = escolherMapa3
drawEstado (Estado (OpcaoEscolherMapa Mapa4) Nada) = escolherMapa4 
drawEstado (Estado (OpcaoEscolherMapa Mapa5) Nada) = escolherMapa5
drawEstado (Estado (OpcaoEscolherMapa Mapa6) Nada) = escolherMapa6 

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado OpcaoNovojogo Nada)  = Estado OpcaoContinuar Nada
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado OpcaoContinuar Nada) = Estado OpcaoNovojogo Nada
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado OpcaoContinuar Nada) = Estado (OpcaoEscolherMapa SemMapa) Nada
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa SemMapa) Nada) = Estado OpcaoContinuar Nada 
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa SemMapa) Nada) = Estado (OpcaoEscolherMapa Mapa1) Nada
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada) = Estado (OpcaoEscolherMapa Mapa2) Nada
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada) = Estado (OpcaoEscolherMapa Mapa3) Nada
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada) = Estado (OpcaoEscolherMapa Mapa1) Nada 
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada) = Estado (OpcaoEscolherMapa Mapa2) Nada 
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada) = Estado (OpcaoEscolherMapa Mapa1) Nada
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada) = Estado (OpcaoEscolherMapa Mapa3) Nada
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada) = Estado (OpcaoEscolherMapa Mapa4) Nada
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada) = Estado (OpcaoEscolherMapa Mapa5) Nada
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada) = Estado (OpcaoEscolherMapa Mapa6) Nada
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada) = Estado (OpcaoEscolherMapa Mapa5) Nada 
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada) = Estado (OpcaoEscolherMapa Mapa6) Nada 
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada) = Estado (OpcaoEscolherMapa Mapa4) Nada
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada) = Estado (OpcaoEscolherMapa Mapa6) Nada 
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada) = Estado (OpcaoEscolherMapa Mapa4) Nada 
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada) = Estado (OpcaoEscolherMapa Mapa5) Nada  
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada) = Estado (OpcaoEscolherMapa Mapa1) Nada
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada) = Estado (OpcaoEscolherMapa Mapa2) Nada
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada) = Estado (OpcaoEscolherMapa Mapa3) Nada
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

       
