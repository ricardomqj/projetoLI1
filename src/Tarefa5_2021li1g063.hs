{- |
Module      : Tarefa5_2021li1g063
Description : Movimentação do personagem
Copyright   : Ricardo Miguel Queirós de Jesus <a100066@alunos.uminho.pt>;
            : Rui Pinto <a100659@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}

module Main where 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI12122 
import Tarefa4_2021li1g063 (moveJogador)
import Tarefa2_2021li1g063 (desconstroiMapa)
data Estado = Estado { menu::Menu , game::Game ,  jogo::Jogo } 

data Game = Nada | Play Mapas | Alterado 
             
data Menu = OpcaoNovojogo | OpcaoContinuar | OpcaoEscolherMapa Mapas | Win | OpcaoInfos Info

data Mapas = Mapa1 | Mapa2 | Mapa3 | Mapa4 | Mapa5 | Mapa6 | Voltar | SemMapa

data Info = InfoControlos MenuControlos | InfoRegras MenuRegras | InfoCreditos MenuCreditos | NoInfo 

data MenuControlos = ControlosInfo | NoControlos 

data MenuRegras = RegrasInfo | NoRegras 

data MenuCreditos = CreditosInfo | NoCreditos 

window :: Display 
--window = InWindow "Jogo" (1280,720) (0,0) 
window = FullScreen

background::Color 
background = greyN 0.8

fr :: Int 
fr = 50 

estadoInicial:: Estado  
estadoInicial = Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))

-- | Barras de Opção no Menu principal
barraOpcaoCont :: Picture
barraOpcaoCont = Translate 0 100 (rectangleSolid 500 150) 

barraOpcaoNov :: Picture
barraOpcaoNov = Translate 0 200 (barraOpcaoCont)

barraOpcaoEsc :: Picture
barraOpcaoEsc = Translate 0 (-200) (barraOpcaoCont)

barraInformacoes :: Picture 
barraInformacoes = Translate 0 (-400) (barraOpcaoCont)

-- | Nome das barras 
textCont :: Picture 
textCont = color white (Translate (-150) (80) (scale 0.5 0.5 (Text "Continuar"))) 

textNov :: Picture 
textNov = color white (Translate (-150) 290 (scale 0.5 0.5 (Text "Novo Jogo")))

textEsc :: Picture 
textEsc = color white (Translate (-230) (-110) (scale 0.5 0.5 (Text "Escolher Mapa")))

textControlos :: Picture 
textControlos = color white (Translate (-210) (-320) (scale 0.5 0.5 (Text "Ver Controlos")))


-- | Menu das Infos

menuInfosPic :: Picture 
menuInfosPic = pictures [backgroundInfos, menuInfoTitle, barraControlos, barraRegras, barraCreditos, textBarraControlos, textBarraRegras, textBarraCreditos]

backgroundInfos :: Picture 
backgroundInfos = Color azure $ rectangleSolid 2000 2000 

menuInfoTitle :: Picture 
menuInfoTitle = Translate (-350) 350 $ Text "Informacoes" 

barraControlos :: Picture 
barraControlos = Translate 0 100 $ rectangleSolid 500 150

barraRegras :: Picture 
barraRegras = Translate 0 (-200) barraControlos

barraCreditos :: Picture
barraCreditos = Translate 0 (-200) barraRegras 

textBarraControlos :: Picture 
textBarraControlos = Translate (-190) 70 $ Color white $ Scale 0.7 0.7 $ Text "Controlos" 

textBarraRegras :: Picture 
textBarraRegras = Translate (-200) (-120) $ Color white $ Scale 0.4 0.4 $ Text "Regras de Jogo"

textBarraCreditos :: Picture 
textBarraCreditos = Translate (-170) (-330) $ Color white $ Scale 0.7 0.7 $ Text "Creditos"

{-

-- | Menu dos Controlos

infoControlos :: Picture 
infoControlos = pictures [backgroundControlos, btnAndarSimbols, andarSimbols, textAndar, titleControlos, btnTreparSimbols, treparSimbols, textTrepar, btnIntCaixa, intCaixaSimbols]

titleControlos :: Picture 
titleControlos = Translate (-450) 300 $ Color black $ Scale 1.5 1.5 $ Text "Controlos"

andarSimbols :: Picture 
andarSimbols = Translate (-800) 150 $ Scale 0.5 0.5 $ Color white $ Text "< >"

btnAndarSimbols :: Picture      
btnAndarSimbols = Translate (-730) 170 $ Color black $ rectangleSolid 150 100

textAndar :: Picture 
textAndar = Translate (-625) 160 $ Scale 0.3 0.3 $ Color white $ Text "Andar para a esquerda ou direita"

backgroundControlos :: Picture
backgroundControlos = Color azure $ rectangleSolid 2000 2000

treparSimbols :: Picture
treparSimbols =  Translate (-750) (-15) $ Scale 0.5 0.5 $ Color white $ Text "^"

btnTreparSimbols :: Picture
btnTreparSimbols = Translate 0 (-150) btnAndarSimbols

textTrepar :: Picture 
textTrepar = Translate (-625) 10 $ Scale 0.3 0.3 $ Color white $ Text "Trepar"

btnIntCaixa :: Picture 
btnIntCaixa = Translate 0 (-150) btnTreparSimbols

intCaixaSimbols :: Picture 
intCaixaSimbols = Translate (-750) (-180) $ Color white $ Scale 0.5 0.5 $ Text "v" 


-}

-- | Menu dos Controlos

menuControlosPic :: Picture 
menuControlosPic = undefined 

-- | Menu das Regras 

menuRegrasPic :: Picture 
menuRegrasPic = undefined

-- | Menu dos Créditos

menuCreditosPic :: Picture 
menuCreditosPic = undefined 

-- | Nome das barras dos mapas e botão voltar

textMapa1 :: Picture 
textMapa1 = color white (translate (-470) 230 (scale 0.5 0.5 (Text "Mapa 1")))

textMapa2 :: Picture
textMapa2 = color white (translate (-470) (30) (scale 0.5 0.5 (Text "Mapa 2")))

textMapa3 :: Picture 
textMapa3 = color white (translate (-470) (-170) (scale 0.5 0.5 (Text "Mapa 3")))

textMapa4 :: Picture 
textMapa4 = color white (translate 240 230 (scale 0.5 0.5 (Text "Mapa 4")))

textMapa5 :: Picture 
textMapa5 = color white (translate 240 (30) (scale 0.5 0.5 (Text "Mapa 5")))

textMapa6 :: Picture 
textMapa6 = color white (translate 240 (-170) (scale 0.5 0.5 (Text "Mapa 6")))

textVoltar :: Picture
textVoltar = color white (translate (-80) (-440) (scale 0.5 0.5 (Text "Voltar")))

-- | Mapas disponíveis para opção
mapa1::Picture 
mapa1 = translate (-350) 250 (Polygon [(-150,75),(150,75),(150,-75),(-150,-75),(-150,75)])

mapa2::Picture 
mapa2 = translate (-350) 50 (Polygon [(-150,75),(150,75),(150,-75),(-150,-75),(-150,75)])

mapa3::Picture 
mapa3 = translate (-350) (-150) (Polygon [(-150,75),(150,75),(150,-75),(-150,-75),(-150,75)])

mapa4 :: Picture 
mapa4 = translate (350) (250) (Polygon [(-150,75),(150,75),(150,-75),(-150,-75),(-150,75)])

mapa5 :: Picture 
mapa5 = translate (350) (50) (Polygon [(-150,75),(150,75),(150,-75),(-150,-75),(-150,75)])

mapa6 :: Picture 
mapa6 = translate (350) (-150) (Polygon [(-150,75),(150,75),(150,-75),(-150,-75),(-150,75)])

btnVoltar :: Picture 
btnVoltar = translate 0 (-420) (Polygon [(-150,75),(150,75),(150,-75),(-150,-75),(-150,75)])


-- | Pictures do Menu inicial
menuInicialNov:: Picture 
menuInicialNov = pictures [barraOpcaoCont,barraOpcaoNov,barraOpcaoEsc, barraInformacoes,color blue barraOpcaoNov,textCont,textNov,textEsc,textControlos] 

menuInicialCont:: Picture 
menuInicialCont = pictures [barraOpcaoCont,barraOpcaoNov,barraOpcaoEsc,barraInformacoes,color blue barraOpcaoCont,textCont,textEsc,textNov,textControlos] 

menuInicialEsc:: Picture 
menuInicialEsc = pictures [barraOpcaoCont,barraOpcaoNov,barraOpcaoEsc,barraInformacoes,color blue barraOpcaoEsc,textCont,textEsc,textNov,textControlos] 

menuInicialInfos :: Picture 
menuInicialInfos = pictures [barraOpcaoCont, barraOpcaoNov, barraOpcaoEsc,barraInformacoes,color blue barraInformacoes,textCont,textEsc,textNov,textControlos]

-- | Pictures do Escolher Mapa
escolherMapa1:: Picture 
escolherMapa1 = pictures [mapa1,mapa2,mapa3, mapa4, mapa5, mapa6, color blue mapa1, textMapa1, textMapa2, textMapa3, textMapa4, textMapa5, textMapa6, btnVoltar, textVoltar]

escolherMapa2::Picture 
escolherMapa2 = pictures [mapa1,mapa2,mapa3, mapa4, mapa5, mapa6, color blue mapa2, textMapa1, textMapa2, textMapa3, textMapa4, textMapa5, textMapa6, btnVoltar, textVoltar]

escolherMapa3::Picture 
escolherMapa3 = pictures [mapa1,mapa2,mapa3, mapa4, mapa5, mapa6, color blue mapa3, textMapa1, textMapa2, textMapa3, textMapa4, textMapa5, textMapa6, btnVoltar, textVoltar]

escolherMapa4::Picture 
escolherMapa4 = pictures [mapa1,mapa2,mapa3, mapa4, mapa5, mapa6, color blue mapa4, textMapa1, textMapa2, textMapa3, textMapa4, textMapa5, textMapa6, btnVoltar, textVoltar]

escolherMapa5::Picture 
escolherMapa5 = pictures [mapa1,mapa2,mapa3, mapa4, mapa5, mapa6, color blue mapa5, textMapa1, textMapa2, textMapa3, textMapa4, textMapa5, textMapa6, btnVoltar, textVoltar]

escolherMapa6::Picture 
escolherMapa6 = pictures [mapa1,mapa2,mapa3, mapa4, mapa5, mapa6, color blue mapa6, textMapa1, textMapa2, textMapa3, textMapa4, textMapa5, textMapa6, btnVoltar, textVoltar]

escolherVoltar :: Picture 
escolherVoltar = pictures [mapa1,mapa2,mapa3, mapa4, mapa5, mapa6, color blue btnVoltar, textMapa1, textMapa2, textMapa3, textMapa4, textMapa5, textMapa6, btnVoltar, textVoltar]



-- | Menu "Ganhou"

mensWin::Picture 
mensWin = Text "Win"

-- | Pictures Mapas

playMapa1::Picture 
playMapa1 = pictures (treatGame (Jogo mapa1dojogo  (Jogador (3,3) Este False)))

playMapa2 :: Picture 
playMapa2 = pictures (treatGame (Jogo mapa2dojogo (Jogador (3,3) Este False)))

playMapa3 :: Picture 
playMapa3 = pictures (treatGame (Jogo mapa3dojogo (Jogador (3,3) Este False)))

playMapa4 :: Picture 
playMapa4 = pictures (treatGame (Jogo mapa4dojogo (Jogador (3,3) Este False)))

playMapa5 :: Picture 
playMapa5 = pictures (treatGame (Jogo mapa5dojogo (Jogador (3,3) Este False)))

playMapa6 :: Picture 
playMapa6 = pictures (treatGame (Jogo mapa6dojogo (Jogador (3,3) Este False)))


giveWin::Estado -> Bool
giveWin (Estado opc  est   (Jogo m (Jogador cord dir tf))) | cord == snd (head(filter ((== Porta).fst) (desconstroiMapa m))) =  True
                                                           | otherwise = False


drawEstado::Estado -> Picture
-- | Mensagem Win
drawEstado (Estado Win Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))) = menuInicialNov
-- | Opções dos Menus
drawEstado (Estado OpcaoNovojogo Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))) = menuInicialNov 
drawEstado (Estado OpcaoContinuar Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = menuInicialCont 
drawEstado (Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))) = menuInicialEsc 
drawEstado (Estado (OpcaoEscolherMapa Mapa1) Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))) = escolherMapa1
drawEstado (Estado (OpcaoEscolherMapa Mapa2) Nada  (Jogo mapa2dojogo (Jogador (3,3) Este False))) = escolherMapa2
drawEstado (Estado (OpcaoEscolherMapa Mapa3) Nada  (Jogo mapa3dojogo (Jogador (3,3) Este False))) = escolherMapa3
drawEstado (Estado (OpcaoEscolherMapa Mapa4) Nada  (Jogo mapa4dojogo (Jogador (3,3) Este False))) = escolherMapa4
drawEstado (Estado (OpcaoEscolherMapa Mapa5) Nada  (Jogo mapa5dojogo (Jogador (3,3) Este False))) = escolherMapa5
drawEstado (Estado (OpcaoEscolherMapa Mapa6) Nada  (Jogo mapa6dojogo (Jogador (3,3) Este False))) = escolherMapa6
--drawEstado (Estado (OpcaoEscolherMapa Voltar) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = escolherVoltar

-- | Menus das informações
drawEstado (Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = menuInicialInfos
drawEstado (Estado (OpcaoInfos (InfoControlos NoControlos)) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = menuInfosPic
drawEstado (Estado (OpcaoInfos (InfoRegras NoRegras)) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = menuInfosPic 
drawEstado (Estado (OpcaoInfos (InfoCreditos NoCreditos)) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = menuInfosPic
drawEstado (Estado (OpcaoInfos (InfoControlos ControlosInfo)) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = menuControlosPic
drawEstado (Estado (OpcaoInfos (InfoRegras RegrasInfo)) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = menuRegrasPic
drawEstado (Estado (OpcaoInfos (InfoCreditos CreditosInfo)) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = menuCreditosPic

-- | Desenhar Mapas 
drawEstado (Estado est (Play Mapa1)  (Jogo mapa1dojogo (Jogador (3,3) Este False))) = playMapa1 
drawEstado (Estado est Alterado  (Jogo mapa1dojogo (Jogador (x,y) dir tf))) = pictures (treatGame (Jogo mapa1dojogo (Jogador (x,y) dir tf)))
drawEstado (Estado (OpcaoEscolherMapa Mapa1) (Play Mapa1)  (Jogo mapa1dojogo (Jogador (x,y) Este False))) = pictures (treatGame (Jogo mapa1dojogo (Jogador (x,y) Este False)))
drawEstado (Estado (OpcaoEscolherMapa Mapa2) (Play Mapa2)  (Jogo mapa2dojogo (Jogador (x,y) Este False))) = pictures (treatGame (Jogo mapa2dojogo  (Jogador (x,y) Este False)))
drawEstado (Estado (OpcaoEscolherMapa Mapa3) (Play Mapa3)  (Jogo mapa3dojogo (Jogador (x,y) Este False))) = pictures (treatGame (Jogo mapa3dojogo  (Jogador (x,y) Este False)))
drawEstado (Estado (OpcaoEscolherMapa Mapa4) (Play Mapa4)  (Jogo mapa4dojogo (Jogador (x,y) Este False))) = pictures (treatGame (Jogo mapa4dojogo  (Jogador (x,y) Este False)))
drawEstado (Estado (OpcaoEscolherMapa Mapa5) (Play Mapa5)  (Jogo mapa5dojogo (Jogador (x,y) Este False))) = pictures (treatGame (Jogo mapa5dojogo  (Jogador (x,y) Este False)))
drawEstado (Estado (OpcaoEscolherMapa Mapa6) (Play Mapa6)  (Jogo mapa6dojogo (Jogador (x,y) Este False))) = pictures (treatGame (Jogo mapa6dojogo  (Jogador (x,y) Este False)))

reageEvento :: Event -> Estado -> Estado

-- | Menu Principal
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado OpcaoNovojogo Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False)))  = Estado OpcaoContinuar Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado OpcaoContinuar Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado OpcaoContinuar Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado OpcaoContinuar Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoInfos (InfoControlos NoControlos)) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
-- | Menu das Informações
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoInfos (InfoControlos NoControlos)) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))

-- | Menu Escolher Mapa
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (Char 'f') Down _ _)  (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (3,3) Este False))                      
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa6) (Play Mapa1) (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
-- | Começar Jogo Opção NovoJogo 
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado OpcaoNovojogo (Play Mapa1)  (Jogo mapa1dojogo (Jogador (3,3) Este False))
-- | Começar Jogo Opção Escolher Mapa
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa1) (Play Mapa1) (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (MouseButton LeftButton) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa1) (Play Mapa1) (Jogo mapa1dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa2) (Play Mapa2) (Jogo mapa2dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (MouseButton LeftButton) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa2) (Play Mapa2) (Jogo mapa2dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa3) (Play Mapa3) (Jogo mapa3dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (MouseButton LeftButton) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa3) (Play Mapa3) (Jogo mapa3dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa4) (Play Mapa4) (Jogo mapa4dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (MouseButton LeftButton) Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa4) (Play Mapa4) (Jogo mapa4dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa5) (Play Mapa5) (Jogo mapa5dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (MouseButton LeftButton) Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa5) (Play Mapa5) (Jogo mapa5dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa6) (Play Mapa6) (Jogo mapa6dojogo (Jogador (3,3) Este False))
reageEvento (EventKey (MouseButton LeftButton) Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (3,3) Este False))) = Estado (OpcaoEscolherMapa Mapa6) (Play Mapa6) (Jogo mapa6dojogo (Jogador (3,3) Este False))
-- | Movimentos
--Andar Esquerda 
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este tf))) =  Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Este tf))) =  Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) dir tf))) =  Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Oeste tf)) AndarEsquerda)
-- Andar Direita
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este tf))) =  Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este tf)) AndarDireita )
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Este tf))) =  Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Este tf)) AndarDireita )
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) dir tf))) =  Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Este tf)) AndarDireita)
-- Trepar
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este tf))) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este tf)) Trepar)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Este tf))) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Este tf)) Trepar)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Este tf))) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Este tf)) Trepar)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Oeste tf))) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste tf)) Trepar)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Oeste tf))) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Oeste tf)) Trepar)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Oeste tf))) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Oeste tf)) Trepar)
-- Interage Caixa
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este False))) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este False)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Este False))) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Este False)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Este False))) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Este False)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este True))) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este True)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Este True))) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Este True)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Este True))) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Este True)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Oeste False))) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste False)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Oeste False))) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Oeste False)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Oeste False))) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Oeste False)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Oeste True))) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste True)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Oeste True))) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Oeste True)) InterageCaixa)
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Oeste True))) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Oeste True)) InterageCaixa)
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
reageTempo n s | giveWin s = Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (3,3) Este False))
               | otherwise = s 

main :: IO ()
main = play window background fr
       estadoInicial drawEstado reageEvento reageTempo 

mapa1dojogo :: Mapa 
mapa1dojogo = [[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio], 
               [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
               [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
               [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio],
               [Porta, Vazio, Bloco, Vazio, Caixa, Caixa, Bloco, Vazio, Vazio, Bloco, Caixa, Vazio],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

mapa2dojogo :: Mapa 
mapa2dojogo = [[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Bloco, Vazio, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Bloco, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Bloco, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Bloco, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
               [Porta, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco],
               [Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Vazio, Vazio, Caixa, Bloco, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

mapa3dojogo :: Mapa 
mapa3dojogo = [[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Bloco, Vazio, Bloco, Bloco, Vazio, Bloco, Vazio, Bloco, Vazio, Bloco, Vazio, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

mapa4dojogo :: Mapa
mapa4dojogo = [[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
               [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
               [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio],
               [Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Vazio],
               [Caixa, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Bloco, Porta],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

mapa5dojogo :: Mapa
mapa5dojogo = [[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Vazio, Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
               [Bloco, Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco],
               [Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Caixa],
               [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Caixa],
               [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Caixa],
               [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
               [Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Bloco, Bloco],
               [Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Caixa, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Vazio, Vazio, Bloco, Vazio, Caixa, Caixa, Caixa, Vazio, Vazio, Bloco, Bloco, Vazio, Caixa, Caixa, Caixa, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

mapa6dojogo :: Mapa 
mapa6dojogo = [[Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio],
               [Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio],
               [Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Bloco, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Bloco],
               [Bloco, Caixa, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Bloco, Vazio, Vazio, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Porta, Vazio, Bloco],
               [Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Bloco],
               [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Caixa, Caixa, Bloco, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Caixa, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Caixa, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Caixa, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]
