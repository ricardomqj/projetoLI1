module Main where 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy 
import LI12122 
import Outro (mapa1dojogo, mapa2dojogo, mapa5dojogo, mapa3dojogo) 
import Tarefa4_2021li1g063 (moveJogador)
import Tarefa2_2021li1g063 (desconstroiMapa)


data Estado = Estado { menu::Menu , game::Game ,  jogo::Jogo , imagens::[Picture] } 

data Game = Nada | Play Mapas | Alterado 
             
data Menu = OpcaoNovojogo | OpcaoContinuar | OpcaoEscolherMapa Mapas | Win
data Mapas = Mapa1 | Mapa2 | Mapa3 | SemMapa

window::Display 
window = InWindow "Jogo" (1000,1000) (0,0) 

background::Color 
background = greyN 0.8

fr::Int 
fr = 50 

estadoInicial:: [Picture] -> Estado  
estadoInicial i = Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (3,3) Este False)) i 

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

-- | Menu "Ganhou"

mensWin::Picture 
mensWin = Text "Win"

-- | Pictures Mapas

playMapa1::[Picture] -> Picture 
playMapa1 i = pictures (treatGame i (Jogo mapa1dojogo  (Jogador (3,3) Este False)))

playMapa2::[Picture] -> Picture 
playMapa2 i = pictures (treatGame i (Jogo mapa2dojogo  (Jogador (2,3) Este False)))

playMapa3::[Picture] -> Picture 
playMapa3  i = pictures (treatGame i (Jogo mapa3dojogo  (Jogador (8,8) Este False)))

giveWin::Estado -> Bool
giveWin (Estado opc  est   (Jogo m (Jogador cord dir tf)) _) | cord == snd (head(filter ((== Porta).fst) (desconstroiMapa m))) =  True
                                                           | otherwise = False


drawEstado::Estado -> Picture
-- | Mensagem Win
drawEstado (Estado Win Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False) ) _ ) = menuInicialNov
-- | Opções dos Menus
drawEstado (Estado OpcaoNovojogo Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False)) _ ) = menuInicialNov 
drawEstado (Estado OpcaoContinuar Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False)) _ ) = menuInicialCont 
drawEstado (Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False)) _) = menuInicialEsc 
drawEstado (Estado (OpcaoEscolherMapa Mapa1) Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False)) _) = escolherMapa1
drawEstado (Estado (OpcaoEscolherMapa Mapa2) Nada  (Jogo mapa2dojogo (Jogador (2,3) Este False)) _) = escolherMapa2
drawEstado (Estado (OpcaoEscolherMapa Mapa3) Nada  (Jogo mapa3dojogo (Jogador (11,11) Este False))_) = escolherMapa3
-- | Desenhar Mapas 
drawEstado (Estado est (Play Mapa1)  (Jogo mapa1dojogo (Jogador (3,3) Este False)) i) = playMapa1 i
drawEstado (Estado est (Play Mapa2)  (Jogo mapa2dojogo (Jogador (2,3) Este False)) i) = playMapa2 i
drawEstado (Estado est (Play Mapa3)  (Jogo mapa3dojogo (Jogador (11,11) Este False)) i) = playMapa3 i
drawEstado (Estado est Alterado  (Jogo mapa1dojogo (Jogador (x,y) dir tf)) i) = pictures (treatGame i (Jogo mapa1dojogo (Jogador (x,y) dir tf)))
drawEstado (Estado (OpcaoEscolherMapa Mapa1) (Play Mapa1)  (Jogo mapa1dojogo (Jogador (x,y) Este False)) i) = pictures (treatGame i (Jogo mapa1dojogo (Jogador (x,y) Este False)))
drawEstado (Estado (OpcaoEscolherMapa Mapa2) (Play Mapa2)  (Jogo mapa2dojogo (Jogador (x,y) Este False)) i) = pictures (treatGame i (Jogo mapa2dojogo  (Jogador (x,y) Este False)))
drawEstado (Estado (OpcaoEscolherMapa Mapa3) (Play Mapa3)  (Jogo mapa3dojogo (Jogador (x,y) Este False)) i) = pictures (treatGame i (Jogo mapa3dojogo (Jogador (x,y) Este False)))

reageEvento :: Event -> Estado -> Estado

-- Menu Principal
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado OpcaoNovojogo Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False)) i)  = Estado OpcaoContinuar Nada (Jogo mapa1dojogo (Jogador (3,3) Este False)) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado OpcaoContinuar Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False)) i) = Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (3,3) Este False) ) i
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado OpcaoContinuar Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))i) = Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False) ) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False)) i) = Estado OpcaoContinuar Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False) ) i
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False)) i) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False)) i

-- Menu Escolher Mapa
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))i) = Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (2,3) Este False)) i
reageEvento (EventKey (SpecialKey KeyLeft ) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (2,3) Este False))i) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))i) = Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (11,11) Este False))i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa1dojogo (Jogador (11,11) Este False))i) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))i
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))i) = Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))i
reageEvento (EventKey (Char 'f') Down _ _)  (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))i) = Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))i
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))i) = Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False)) i                     
-- Começar Jogo Opção NovoJogo 
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))i) = Estado OpcaoNovojogo (Play Mapa1)  (Jogo mapa1dojogo (Jogador (3,3) Este False))i
-- Começar Jogo Opção Escolher Mapa
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))i) = Estado (OpcaoEscolherMapa Mapa1) (Play Mapa1) (Jogo mapa1dojogo (Jogador (3,3) Este False))i
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (2,3) Este False))i) = Estado (OpcaoEscolherMapa Mapa2) (Play Mapa2) (Jogo mapa2dojogo (Jogador (2,3) Este False))i
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (11,11) Este False))i) = Estado (OpcaoEscolherMapa Mapa3) (Play Mapa3)  (Jogo mapa3dojogo (Jogador (11,11) Este False))i

-- | Movimentos
--Andar Esquerda 
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este tf)) i) =  Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda)i
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Este tf))i) =  Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda)i
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa3) (Jogo mapa3dojogo (Jogador (x,y) Este tf))i) =  Estado est Alterado (moveJogador (Jogo mapa3dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda)i
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) dir tf))i) =  Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Oeste tf)) AndarEsquerda)i
-- Andar Direita
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play m) (Jogo mapa1dojogo (Jogador (x,y) Este tf))i) =  Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este tf)) AndarDireita )i
--reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Este tf))) =  Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Este tf)) AndarDireita )
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) dir tf))i) =  Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Este tf)) AndarDireita)i
-- Trepar
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play m) (Jogo mapa1dojogo (Jogador (x,y) Este tf))i) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este tf)) Trepar)i
--reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Este tf))) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Este tf)) Trepar)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Este tf))i) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Este tf)) Trepar)i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play m) (Jogo mapa1dojogo (Jogador (x,y) Oeste tf))i) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste tf)) Trepar)i
--reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Oeste tf))) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Oeste tf)) Trepar)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Oeste tf))i) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Oeste tf)) Trepar)i
-- Interage Caixa
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa1) (Jogo ma (Jogador (x,y) Este False))i) = Estado est Alterado (moveJogador (Jogo ma (Jogador (x,y) Este False)) InterageCaixa)i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Este False))i) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Este False)) InterageCaixa)i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este True))i) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este True)) InterageCaixa)i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Este True))i) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Este True)) InterageCaixa)i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Este True))i) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Este True)) InterageCaixa)i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Oeste False))i) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste False)) InterageCaixa)i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Oeste False))i) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Oeste False)) InterageCaixa)i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Oeste False))i) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Oeste False)) InterageCaixa)i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Oeste True))i) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste True)) InterageCaixa)i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Oeste True))i) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Oeste True)) InterageCaixa)i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Oeste True))i) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Oeste True)) InterageCaixa)i
reageEvento _ s = s 


{-
playGame:: Estado -> Picture
playGame (Estado OpcaoNovojogo Alterado (Jogo m (Jogador (x,y) dir tf))) = pictures (treatGame (moveJogador (Jogo m (Jogador (x,y) dir tf)) AndarEsquerda))
-}

treatGame::[Picture] -> Jogo -> [Picture]
treatGame (i:is) j = transf (i:is) j (-450,450) (0,0)

transf::[Picture] -> Jogo -> (Float,Float) -> (Int,Int) -> [Picture]
transf (i:is) (Jogo [] _ ) _  _ = []
transf (i:is) (Jogo [h] (Jogador (x,y) d tf)) (a,b) (cx,cy) = drawMap (i:is) h (Jogador (x,y) d tf) a b (cx,cy)
transf (i:is) (Jogo (h:t) (Jogador (x,y) d tf)) (a,b) (cx,cy) = drawMap (i:is) h (Jogador (x,y) d tf) a b (cx,cy) ++ transf (i:is) (Jogo t (Jogador (x,y) d tf)) (-450,b-100) (0,cy+1) 

-- | Desenha os mapas em uma lista de pictures 
drawMap::[Picture] -> [Peca] -> Jogador -> Float -> Float -> (Int,Int) -> [Picture] 
drawMap i [h] (Jogador (x,y) d tf) a b (cx,cy)
        | x == cx && y == cy = [Translate a b (Polygon [(-50,50),(0,0),(-50,-50),(-50,50)])] 
        | h == Bloco = [Translate a b (scale 0.39 0.39  ((!!) i 0 ))] 
        | h == Vazio = [Translate a b Blank] 
        | h == Porta = [Translate a b (scale 0.167 0.128 ((!!) i 1) )] 
        | h == Caixa = [Translate a b (scale 0.39 0.39 ((!!) i 2))]
--drawMap ([]:ys) a b  = drawMap ys (-450) (b-100)
drawMap i (h:t) (Jogador (x,y) d tf) a b  (cx,cy)
        | tf && cy == y-1 && cx == x = Translate a b (scale 0.39 0.39 ((!!) i 2)) : drawMap i t (Jogador (x,y) d tf) (a + 100) b (cx + 1,cy)
        | x == cx && y == cy && d == Este = Translate a b (Polygon [(-50,50),(0,0),(-50,-50),(-50,50)]) : drawMap i t (Jogador (x,y) d tf) (a + 100) b (cx + 1,cy)
        | x == cx && y == cy && d == Oeste = Translate a b (Polygon [(50,50),(0,0),(50,-50),(50,50)]) : drawMap i t (Jogador (x,y) d tf) (a + 100) b (cx + 1,cy)        
        | h == Bloco = Translate a b (scale 0.39 0.39  ((!!) i 0)) : drawMap i t (Jogador (x,y) d tf) (a + 100) b (cx + 1,cy)
        | h == Vazio = Translate a b Blank : drawMap i t (Jogador (x,y) d tf) (a + 100) b (cx + 1,cy)
        | h == Porta = Translate a b (scale 0.167 0.128 ((!!) i 1)) : drawMap i t (Jogador (x,y) d tf) (a + 100) b (cx + 1,cy)
        | h == Caixa = Translate a b (scale 0.39 0.39 ((!!) i 2)) : drawMap i t (Jogador (x,y) d tf) (a + 100) b (cx + 1,cy)          

reageTempo :: Float -> Estado -> Estado
reageTempo n (Estado OpcaoNovojogo gam (Jogo m jogd ) i )
                         | giveWin (Estado OpcaoNovojogo gam (Jogo m jogd ) i )  &&  m == mapa1dojogo = Estado (OpcaoEscolherMapa Mapa2) (Play Mapa2) (Jogo mapa2dojogo (Jogador (2,3) Este False)) i  -- TRATAR DO MAPA!!
                         | giveWin (Estado OpcaoNovojogo gam (Jogo m jogd ) i) &&  m == mapa2dojogo = Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (3,3) Este False)) i
reageTempo n (Estado (OpcaoEscolherMapa mp) gam (Jogo m jogd) i ) 
                         | giveWin (Estado (OpcaoEscolherMapa mp) gam (Jogo m jogd)i) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False)) i                       
reageTempo _ s = s

main :: IO ()
main = do 
      Just bloco <- loadJuicy "bloco.png" 
      Just porta <- loadJuicy "Porta.png"
      Just caixa <- loadJuicy "caixa.png"
      play window background fr (estadoInicial [bloco,porta,caixa]) drawEstado reageEvento reageTempo 
