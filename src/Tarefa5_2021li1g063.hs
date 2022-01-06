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
import Graphics.Gloss.Juicy
import LI12122 
import Tarefa4_2021li1g063 (moveJogador)
import Tarefa2_2021li1g063 (desconstroiMapa)
data Estado = Estado { menu::Menu , game::Game ,  jogo::Jogo , imagens::[Picture]} 

data Game = Nada | Play Mapas | Alterado 
             
data Menu = OpcaoNovojogo | OpcaoContinuar | OpcaoEscolherMapa Mapas | Win | OpcaoInfos Info | VerControlos Controlos | VerRegras Regras | VerCreditos Creditos

data Mapas = Mapa1 | Mapa2 | Mapa3 | Mapa4 | Mapa5 | Mapa6 | Voltar | SemMapa

data Info = InfoControlos | InfoRegras | InfoCreditos | NoInfo 

data MenuControlos = ControlosInfo | NoControlos 

data MenuRegras = RegrasInfo | NoRegras 

data MenuCreditos = CreditosInfo | NoCreditos 

data Controlos = ShowControlos | SemControlos

data Regras = ShowRegras | SemRegras 

data Creditos = ShowCreditos | SemCreditos    

window :: Display 
--window = InWindow "Jogo" (1280,720) (0,0) 
window = FullScreen

background::Color 
background = greyN 0.8

fr :: Int 
fr = 50 

estadoInicial :: [Picture] -> Estado  
estadoInicial i = Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i 

-- | Menu principal

mainTitle :: Picture
mainTitle = Translate (-450) 350 $ Scale 1.3 1.3 $ Color black $ Text "Block Dude" 

barraOpcaoCont :: Picture
barraOpcaoCont = Translate 0 0 (rectangleSolid 500 150) 

barraOpcaoNov :: Picture
barraOpcaoNov = Translate 0 200 (barraOpcaoCont)

barraOpcaoEsc :: Picture
barraOpcaoEsc = Translate 0 (-200) (barraOpcaoCont)

barraInformacoes :: Picture 
barraInformacoes = Translate 0 (-400) (barraOpcaoCont)

textCont :: Picture 
textCont = color white (Translate (-150) (-20) (scale 0.5 0.5 (Text "Continuar"))) 

textNov :: Picture 
textNov = color white (Translate (-150) 190 (scale 0.5 0.5 (Text "Novo Jogo")))

textEsc :: Picture 
textEsc = color white (Translate (-230) (-210) (scale 0.5 0.5 (Text "Escolher Mapa")))

textInformacoes :: Picture 
textInformacoes = color white (Translate (-170) (-420) (scale 0.5 0.5 (Text "Informacoes")))

menuInicialNov:: Picture 
menuInicialNov = pictures [barraOpcaoCont,barraOpcaoNov,barraOpcaoEsc, barraInformacoes,color blue barraOpcaoNov,textCont,textNov,textEsc,textInformacoes, mainTitle] 

menuInicialCont:: Picture 
menuInicialCont = pictures [barraOpcaoCont,barraOpcaoNov,barraOpcaoEsc,barraInformacoes,color blue barraOpcaoCont,textCont,textEsc,textNov,textInformacoes, mainTitle] 

menuInicialEsc:: Picture 
menuInicialEsc = pictures [barraOpcaoCont,barraOpcaoNov,barraOpcaoEsc,barraInformacoes,color blue barraOpcaoEsc,textCont,textEsc,textNov,textInformacoes, mainTitle] 

menuInicialInfos :: Picture 
menuInicialInfos = pictures [barraOpcaoCont, barraOpcaoNov, barraOpcaoEsc,barraInformacoes,color blue barraInformacoes,textCont,textEsc,textNov,textInformacoes, mainTitle]


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

escolherInfoControlos :: Picture 
escolherInfoControlos = pictures [backgroundInfos, menuInfoTitle, barraControlos, barraRegras, barraCreditos, color blue barraControlos, textBarraControlos, textBarraRegras, textBarraCreditos]

escolherInfoRegras :: Picture 
escolherInfoRegras = pictures [backgroundInfos, menuInfoTitle, barraRegras, barraControlos, barraCreditos, color blue barraRegras, textBarraControlos, textBarraRegras, textBarraCreditos]

escolherInfoCreditos :: Picture 
escolherInfoCreditos = pictures [backgroundInfos, menuInfoTitle, barraCreditos, barraControlos, barraRegras, color blue barraCreditos, textBarraCreditos, textBarraControlos, textBarraRegras]

-- | Menu dos Controlos


menuControlosPic :: Picture 
menuControlosPic = pictures [backgroundControlos, btnAndarSimbols, andarSimbols, textAndar, titleControlos, btnTreparSimbols, treparSimbols, textTrepar, btnIntCaixa, intCaixaSimbols, textIntCaixa, btnVoltarSimbols, voltarSimbols, textVoltarInfo]

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
intCaixaSimbols = Translate (-740) (-150) $ Color white $ Scale 0.5 0.5 $ Text "v" 

textIntCaixa :: Picture 
textIntCaixa = Translate (-625) (-150) $ Color white $ Scale 0.3 0.3 $ Text "Pegar ou largar caixa"

btnVoltarSimbols :: Picture 
btnVoltarSimbols = Translate 0 (-150) btnIntCaixa 

voltarSimbols :: Picture 
voltarSimbols = Translate (-735) (-300) $ Scale 0.5 0.5 $ Color white $ Text "f"

textVoltarInfo :: Picture 
textVoltarInfo = Translate (-625) (-300) $ Scale 0.3 0.3 $ Color white $ Text "Voltar para o menu anterior/Sair do mapa"


-- | Menu das Regras 

menuRegrasPic :: Picture 
menuRegrasPic = pictures [backgroundControlos, titleRegras, regra1, p1, p2, regra2, regra2', p3, regra3, regra3', p4, regra4, p5, regra5, p6, regra6, regra6', p7, regra7, regra7', p8, regra8]

titleRegras :: Picture 
titleRegras = Translate (-280) 300 $ Scale 1.5 1.5 $ Color white $ Text "Regras"

p1 :: Picture 
p1 = Translate (-905) (160) $ circleSolid 6 

regra1 :: Picture 
regra1 = Translate (-890) 150 $ Scale 0.2 0.2 $ Color white $ Text "O objetivo deste jogo consiste em controlar uma personagem ate a porta de um mapa." 

p2 :: Picture  
p2 = Translate (-905) 100 $ circleSolid 6

regra2 :: Picture 
regra2 = Translate (-890) 90 $ Scale 0.2 0.2 $ Color white $ Text "Num mapa existem, para alem do personagem e da porta, blocos e caixas, sendo permitido ao jogador mover as caixas para conseguir"

regra2' :: Picture 
regra2' = Translate (-890) 60 $ Scale 0.2 0.2 $ Color white $ Text "chegar a porta."

p3 :: Picture 
p3 = Translate (-905) 30 $ circleSolid 6

regra3 :: Picture 
regra3 = Translate (-890) 20 $ Scale 0.2 0.2 $ Color white $ Text "Os unicos movimentos permitidos ao jogador sao de carregar ou largar uma caixa, avancar nas direcoes Este/Oeste e trepar um bloco"

regra3' :: Picture 
regra3' = Translate (-890) (-10) $ Scale 0.2 0.2 $ Color white $ Text "ou caixa."

p4 :: Picture 
p4 = Translate (-905) (-40) $ circleSolid 6

regra4 :: Picture 
regra4 = Translate (-890) (-50) $ Scale 0.2 0.2 $ Color white $ Text "O jogador pode avancar uma unidade numa certa direcao desde que esse espaco esteja livre de obstaculos."

p5 :: Picture 
p5 = Translate (-905) (-90) $ circleSolid 6

regra5 :: Picture 
regra5 = Translate (-890) (-100) $ Scale 0.2 0.2 $ Color white $ Text "O jogador apenas pode trepar um obstaculo que se encontre imediatamente a sua frente e sem outro obstaculo por cima."

p6 :: Picture 
p6 = Translate (-905) (-140) $ circleSolid 6

regra6 :: Picture 
regra6 = Translate (-890) (-150) $ Scale 0.2 0.2 $ Color white $ Text "Para carregar uma caixa, e necessario que a caixa esteja na posicao imediatamente a frente do personagem e que nao haja"

regra6' :: Picture 
regra6' = Translate (-890) (-185) $ Scale 0.2 0.2 $ Color white $ Text "nenhum outro obtaculo acima do personagem ou da caixa."

p7 :: Picture 
p7 = Translate (-905) (-215) $ circleSolid 6

regra7 :: Picture 
regra7 = Translate (-890) (-225) $ Scale 0.2 0.2 $ Color white $ Text "Se a frente do jogador existir um obstaculo, a caixa sera largada por cima deste, desde que o obstaculo esteja a altura do"

regra7' :: Picture 
regra7' = Translate (-890) (-260) $ Scale 0.2 0.2 $ Color white $ Text "jogador."

p8 :: Picture 
p8 = Translate (-905) (-295) $ circleSolid 6

regra8 :: Picture 
regra8 = Translate (-890) (-305) $ Scale 0.2 0.2 $ Color white $ Text "Em caso de queda eminente a caixa sera largada imediatamente para a ultima posicao de queda."

-- | Menu dos Créditos

menuCreditosPic :: Picture 
menuCreditosPic = pictures [backgroundControlos, titleCreditos, textCreditos1, textCreditos2, textCreditos3, textCreditos4]

titleCreditos :: Picture 
titleCreditos = Translate (-350) 300 $ Scale 1.5 1.5 $ Text "Creditos"

textCreditos1 :: Picture 
textCreditos1 = Translate (-900) (150) $ Scale 0.4 0.4 $ Color white $ Text "Jogo 'Block Dude', criado em Haskell no ambito da cadeira de" 

textCreditos2 :: Picture 
textCreditos2 = Translate (-900) (70) $ Scale 0.4 0.4 $ Color white $ Text "Laboratorios de Informatica I, na Licenciatura em Engenharia "

textCreditos3 :: Picture 
textCreditos3 = Translate (-900) (-10) $ Scale 0.4 0.4 $ Color white $ Text "Informatica na Univerdade do Minho."

textCreditos4 :: Picture 
textCreditos4 = Translate (-900) (-170) $ Scale 0.4 0.4 $ Color white $ Text "Feito por: Ricardo Jesus e Rui Pinto"
 
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
escolherVoltar = pictures [mapa1,mapa2,mapa3, mapa4, mapa5, mapa6, btnVoltar, color blue btnVoltar, textMapa1, textMapa2, textMapa3, textMapa4, textMapa5, textMapa6, textVoltar]



-- | Menu "Ganhou"

mensWin::Picture 
mensWin = Text "Win"

-- | Pictures Mapas

playMapa1 :: [Picture] -> Picture  
playMapa1 i = pictures (treatGame i (Jogo mapa1dojogo (Jogador (10,10) Oeste False)))

playMapa2 :: [Picture] -> Picture 
playMapa2 i = pictures (treatGame i (Jogo mapa2dojogo (Jogador (8,10) Oeste False)))

playMapa3 :: [Picture] -> Picture 
playMapa3 i = pictures (treatGame i (Jogo mapa3dojogo (Jogador (11,10) Oeste False)))
 
playMapa4 :: [Picture] -> Picture 
playMapa4  i = pictures (treatGame i (Jogo mapa4dojogo (Jogador (3,9) Oeste False)))

playMapa5 :: [Picture] ->Picture 
playMapa5 i = pictures (treatGame i (Jogo mapa5dojogo (Jogador (16,8) Oeste False)))

playMapa6 :: [Picture] -> Picture 
playMapa6 i = pictures (treatGame i (Jogo mapa6dojogo (Jogador (14,7) Oeste False)))


giveWin::Estado -> Bool
giveWin (Estado opc  est   (Jogo m (Jogador cord dir tf)) _) | cord == snd (head(filter ((== Porta).fst) (desconstroiMapa m))) =  True
                                                           | otherwise = False


drawEstado :: Estado -> Picture
-- | Mensagem Win
drawEstado (Estado Win Nada  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = menuInicialNov
-- | Opções dos Menus
drawEstado (Estado OpcaoNovojogo Nada  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = menuInicialNov 
drawEstado (Estado OpcaoContinuar Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = menuInicialCont 
drawEstado (Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = menuInicialInfos
drawEstado (Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = menuInicialEsc 
drawEstado (Estado (OpcaoEscolherMapa Mapa1) Nada  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) _) = escolherMapa1
drawEstado (Estado (OpcaoEscolherMapa Mapa2) Nada  (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) _) = escolherMapa2
drawEstado (Estado (OpcaoEscolherMapa Mapa3) Nada  (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) _) = escolherMapa3
drawEstado (Estado (OpcaoEscolherMapa Mapa4) Nada  (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) _) = escolherMapa4
drawEstado (Estado (OpcaoEscolherMapa Mapa5) Nada  (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) _) = escolherMapa5
drawEstado (Estado (OpcaoEscolherMapa Mapa6) Nada  (Jogo mapa6dojogo (Jogador (14,7) Oeste False)) _) = escolherMapa6
drawEstado (Estado (OpcaoEscolherMapa Voltar) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) _) = escolherVoltar

-- | Menus das informações
drawEstado (Estado (OpcaoInfos InfoControlos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) _) = escolherInfoControlos 
drawEstado (Estado (OpcaoInfos InfoRegras) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) _) = escolherInfoRegras 
drawEstado (Estado (OpcaoInfos InfoCreditos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) _) = escolherInfoCreditos
drawEstado (Estado (VerControlos SemControlos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) _) = escolherInfoControlos 
drawEstado (Estado (VerRegras SemRegras) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) _) = escolherInfoRegras
drawEstado (Estado (VerCreditos SemCreditos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) _) = escolherInfoCreditos
drawEstado (Estado (VerControlos ShowControlos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) _) = menuControlosPic 
drawEstado (Estado (VerRegras ShowRegras) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) _) = menuRegrasPic 
drawEstado (Estado (VerCreditos ShowCreditos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) _) = menuCreditosPic 


-- | Desenhar Mapas 
 
drawEstado (Estado est (Play Mapa1)  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = playMapa1 i
drawEstado (Estado est (Play Mapa2)  (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) i) = playMapa2 i
drawEstado (Estado est (Play Mapa3)  (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i) = playMapa3 i
drawEstado (Estado est (Play Mapa4)  (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) i) = playMapa4 i 
drawEstado (Estado est (Play Mapa5)  (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) i) = playMapa5 i 
drawEstado (Estado est (Play Mapa6)  (Jogo mapa6dojogo (Jogador (14,7) Oeste False)) i) = playMapa6 i 
drawEstado (Estado est Alterado  (Jogo mapa1dojogo (Jogador (x,y) dir tf)) i) = pictures (treatGame i (Jogo mapa1dojogo (Jogador (x,y) dir tf)))
drawEstado (Estado (OpcaoEscolherMapa Mapa1) (Play Mapa1)  (Jogo mapa1dojogo (Jogador (x,y) Oeste False)) i) = pictures (treatGame i (Jogo mapa1dojogo (Jogador (x,y) Oeste False)))
drawEstado (Estado (OpcaoEscolherMapa Mapa2) (Play Mapa2)  (Jogo mapa2dojogo (Jogador (x,y) Oeste False)) i) = pictures (treatGame i (Jogo mapa2dojogo  (Jogador (x,y) Oeste False)))
drawEstado (Estado (OpcaoEscolherMapa Mapa3) (Play Mapa3)  (Jogo mapa3dojogo (Jogador (x,y) Oeste False)) i) = pictures (treatGame i (Jogo mapa3dojogo (Jogador (x,y) Oeste False)))
drawEstado (Estado (OpcaoEscolherMapa Mapa4) (Play Mapa4)  (Jogo mapa4dojogo (Jogador (x,y) Oeste False)) i) = pictures (treatGame i (Jogo mapa4dojogo  (Jogador (x,y) Oeste False)))
drawEstado (Estado (OpcaoEscolherMapa Mapa5) (Play Mapa5)  (Jogo mapa5dojogo (Jogador (x,y) Oeste False)) i) = pictures (treatGame i (Jogo mapa5dojogo  (Jogador (x,y) Oeste False)))
drawEstado (Estado (OpcaoEscolherMapa Mapa6) (Play Mapa6)  (Jogo mapa6dojogo (Jogador (x,y) Oeste False)) i) = pictures (treatGame i (Jogo mapa6dojogo  (Jogador (x,y) Oeste False)))

reageEvento :: Event -> Estado -> Estado

-- | Menu Principal
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado OpcaoNovojogo Nada  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i)  = Estado OpcaoContinuar Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado OpcaoContinuar Nada  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado OpcaoContinuar Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado OpcaoContinuar Nada  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoInfos InfoControlos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
-- | Menu das Informações
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoInfos InfoControlos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoInfos InfoRegras) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoInfos InfoRegras) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoInfos InfoCreditos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoInfos InfoCreditos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoInfos InfoControlos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoInfos InfoControlos) Nada  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoInfos InfoCreditos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoInfos InfoRegras) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoInfos InfoControlos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoInfos InfoCreditos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoInfos InfoRegras) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoInfos InfoControlos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoInfos InfoRegras) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoInfos InfoCreditos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoInfos NoInfo) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoInfos InfoControlos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (VerControlos ShowControlos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado (VerControlos ShowControlos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoInfos InfoControlos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoInfos InfoRegras) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (VerRegras ShowRegras) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado (VerRegras ShowRegras) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoInfos InfoRegras) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoInfos InfoCreditos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (VerCreditos ShowCreditos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado (VerCreditos ShowCreditos) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoInfos InfoCreditos) Nada (Jogo mapa1dojogo (Jogador (11,10) Este False)) i 
-- | Menu Escolher Mapa
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Voltar) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Voltar) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (14,7) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Voltar) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa Voltar) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Voltar) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Voltar) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (14,7) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (14,7) Oeste False)) i) = Estado (OpcaoEscolherMapa Voltar) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (14,7) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) i) = Estado (OpcaoEscolherMapa Voltar) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (14,7) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (14,7) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (14,7) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (14,7) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _)  (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa SemMapa) Nada  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i                    
reageEvento (EventKey (Char 'f') Down _ _) (Estado (OpcaoEscolherMapa Mapa6) (Play Mapa1) (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa SemMapa) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
-- | Começar Jogo Opção NovoJogo 
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado OpcaoNovojogo (Play Mapa1)  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
-- | Começar Jogo Opção Escolher Mapa
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa1) (Play Mapa1) (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (MouseButton LeftButton) Down _ _) (Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa1) (Play Mapa1) (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i 
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa2) (Play Mapa2) (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) i
reageEvento (EventKey (MouseButton LeftButton) Down _ _) (Estado (OpcaoEscolherMapa Mapa2) Nada (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa2) (Play Mapa2) (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa3) (Play Mapa3) (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i
reageEvento (EventKey (MouseButton LeftButton) Down _ _) (Estado (OpcaoEscolherMapa Mapa3) Nada (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa3) (Play Mapa3) (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa4) (Play Mapa4) (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) i
reageEvento (EventKey (MouseButton LeftButton) Down _ _) (Estado (OpcaoEscolherMapa Mapa4) Nada (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa4) (Play Mapa4) (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa5) (Play Mapa5) (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) i
reageEvento (EventKey (MouseButton LeftButton) Down _ _) (Estado (OpcaoEscolherMapa Mapa5) Nada (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa5) (Play Mapa5) (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) i
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (14,7) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa6) (Play Mapa6) (Jogo mapa6dojogo (Jogador (14,7) Oeste False)) i
reageEvento (EventKey (MouseButton LeftButton) Down _ _) (Estado (OpcaoEscolherMapa Mapa6) Nada (Jogo mapa6dojogo (Jogador (14,7) Oeste False)) i) = Estado (OpcaoEscolherMapa Mapa6) (Play Mapa6) (Jogo mapa6dojogo (Jogador (11,10) Este False)) i
-- | Dentro dos mapas
reageEvento (EventKey (Char 'f') Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este tf)) i) = Estado OpcaoNovojogo Nada  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Oeste tf)) i) = Estado OpcaoNovojogo Nada  (Jogo mapa1dojogo (Jogador (10,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Este tf)) i) = Estado OpcaoNovojogo Nada  (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Oeste tf)) i) = Estado OpcaoNovojogo Nada  (Jogo mapa2dojogo (Jogador (8,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado est (Play Mapa3) (Jogo mapa3dojogo (Jogador (x,y) Este tf)) i) = Estado OpcaoNovojogo Nada  (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado est (Play Mapa3) (Jogo mapa3dojogo (Jogador (x,y) Oeste tf)) i) = Estado OpcaoNovojogo Nada  (Jogo mapa3dojogo (Jogador (11,10) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado est (Play Mapa4) (Jogo mapa4dojogo (Jogador (x,y) Este tf)) i) = Estado OpcaoNovojogo Nada  (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado est (Play Mapa4) (Jogo mapa4dojogo (Jogador (x,y) Oeste tf)) i) = Estado OpcaoNovojogo Nada  (Jogo mapa4dojogo (Jogador (3,9) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado est (Play Mapa5) (Jogo mapa5dojogo (Jogador (x,y) Este tf)) i) = Estado OpcaoNovojogo Nada  (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado est (Play Mapa5) (Jogo mapa5dojogo (Jogador (x,y) Oeste tf)) i) = Estado OpcaoNovojogo Nada  (Jogo mapa5dojogo (Jogador (16,8) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado est (Play Mapa6) (Jogo mapa6dojogo (Jogador (x,y) Este tf)) i) = Estado OpcaoNovojogo Nada  (Jogo mapa6dojogo (Jogador (14,7) Oeste False)) i
reageEvento (EventKey (Char 'f') Down _ _) (Estado est (Play Mapa6) (Jogo mapa6dojogo (Jogador (x,y) Oeste tf)) i) = Estado OpcaoNovojogo Nada  (Jogo mapa6dojogo (Jogador (14,7) Oeste False)) i


-- | Movimentos
--Andar Esquerda 
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este tf)) i) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda) i
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Este tf)) i) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda) i
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa3) (Jogo mapa3dojogo (Jogador (x,y) Este tf)) i) = Estado est Alterado (moveJogador (Jogo mapa3dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda) i                 
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa4) (Jogo mapa4dojogo (Jogador (x,y) Este tf)) i) = Estado est Alterado (moveJogador (Jogo mapa4dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda) i 
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa5) (Jogo mapa5dojogo (Jogador (x,y) Este tf)) i) = Estado est Alterado (moveJogador (Jogo mapa5dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda) i 
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa6) (Jogo mapa6dojogo (Jogador (x,y) Este tf)) i) = Estado est Alterado (moveJogador (Jogo mapa6dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda) i 
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Oeste tf)) i) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda) i
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Oeste tf)) i) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda) i
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa3) (Jogo mapa3dojogo (Jogador (x,y) Oeste tf)) i) = Estado est Alterado (moveJogador (Jogo mapa3dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda) i                 
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa4) (Jogo mapa4dojogo (Jogador (x,y) Oeste tf)) i) = Estado est Alterado (moveJogador (Jogo mapa4dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda) i 
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa5) (Jogo mapa5dojogo (Jogador (x,y) Oeste tf)) i) = Estado est Alterado (moveJogador (Jogo mapa5dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda) i 
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est (Play Mapa6) (Jogo mapa6dojogo (Jogador (x,y) Oeste tf)) i) = Estado est Alterado (moveJogador (Jogo mapa6dojogo (Jogador (x,y) Oeste tf)) AndarEsquerda) i 
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) dir tf)) i) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Oeste tf)) AndarEsquerda) i
-- Andar Direita
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este tf)) i) =  Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este tf)) AndarDireita ) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Este tf)) i) =  Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Este tf)) AndarDireita ) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play Mapa3) (Jogo mapa3dojogo (Jogador (x,y) Este tf)) i) =  Estado est Alterado (moveJogador (Jogo mapa3dojogo (Jogador (x,y) Este tf)) AndarDireita ) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play Mapa4) (Jogo mapa4dojogo (Jogador (x,y) Este tf)) i) =  Estado est Alterado (moveJogador (Jogo mapa4dojogo (Jogador (x,y) Este tf)) AndarDireita ) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play Mapa5) (Jogo mapa5dojogo (Jogador (x,y) Este tf)) i) =  Estado est Alterado (moveJogador (Jogo mapa5dojogo (Jogador (x,y) Este tf)) AndarDireita ) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play Mapa6) (Jogo mapa6dojogo (Jogador (x,y) Este tf)) i) =  Estado est Alterado (moveJogador (Jogo mapa6dojogo (Jogador (x,y) Este tf)) AndarDireita ) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Oeste tf)) i) =  Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este tf)) AndarDireita ) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Oeste tf)) i) =  Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Este tf)) AndarDireita ) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play Mapa3) (Jogo mapa3dojogo (Jogador (x,y) Oeste tf)) i) =  Estado est Alterado (moveJogador (Jogo mapa3dojogo (Jogador (x,y) Este tf)) AndarDireita ) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play Mapa4) (Jogo mapa4dojogo (Jogador (x,y) Oeste tf)) i) =  Estado est Alterado (moveJogador (Jogo mapa4dojogo (Jogador (x,y) Este tf)) AndarDireita ) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play Mapa5) (Jogo mapa5dojogo (Jogador (x,y) Oeste tf)) i) =  Estado est Alterado (moveJogador (Jogo mapa5dojogo (Jogador (x,y) Este tf)) AndarDireita ) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est (Play Mapa6) (Jogo mapa6dojogo (Jogador (x,y) Oeste tf)) i) =  Estado est Alterado (moveJogador (Jogo mapa6dojogo (Jogador (x,y) Este tf)) AndarDireita ) i
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) dir tf)) i) =  Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Este tf)) AndarDireita) i
-- Trepar
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este tf)) i) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este tf)) Trepar) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Este tf)) i) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Este tf)) Trepar) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa3) (Jogo mapa3dojogo (Jogador (x,y) Este tf)) i) = Estado est Alterado (moveJogador (Jogo mapa3dojogo (Jogador (x,y) Este tf)) Trepar) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa4) (Jogo mapa4dojogo (Jogador (x,y) Este tf)) i) = Estado est Alterado (moveJogador (Jogo mapa4dojogo (Jogador (x,y) Este tf)) Trepar) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa5) (Jogo mapa5dojogo (Jogador (x,y) Este tf)) i) = Estado est Alterado (moveJogador (Jogo mapa5dojogo (Jogador (x,y) Este tf)) Trepar) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa6) (Jogo mapa6dojogo (Jogador (x,y) Este tf)) i) = Estado est Alterado (moveJogador (Jogo mapa6dojogo (Jogador (x,y) Este tf)) Trepar) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Este tf)) i) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Este tf)) Trepar) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Oeste tf)) i) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste tf)) Trepar) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Oeste tf)) i) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Oeste tf)) Trepar) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa3) (Jogo mapa3dojogo (Jogador (x,y) Oeste tf)) i) = Estado est Alterado (moveJogador (Jogo mapa3dojogo (Jogador (x,y) Oeste tf)) Trepar) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa4) (Jogo mapa4dojogo (Jogador (x,y) Oeste tf)) i) = Estado est Alterado (moveJogador (Jogo mapa4dojogo (Jogador (x,y) Oeste tf)) Trepar) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa5) (Jogo mapa5dojogo (Jogador (x,y) Oeste tf)) i) = Estado est Alterado (moveJogador (Jogo mapa5dojogo (Jogador (x,y) Oeste tf)) Trepar) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est (Play Mapa6) (Jogo mapa6dojogo (Jogador (x,y) Oeste tf)) i) = Estado est Alterado (moveJogador (Jogo mapa6dojogo (Jogador (x,y) Oeste tf)) Trepar) i
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Oeste tf)) i) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Oeste tf)) Trepar) i
-- Interage Caixa
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este False)) i) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este False)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Este False)) i) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Este False)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa3) (Jogo mapa3dojogo (Jogador (x,y) Este False)) i) = Estado est Alterado (moveJogador (Jogo mapa3dojogo (Jogador (x,y) Este False)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa4) (Jogo mapa4dojogo (Jogador (x,y) Este False)) i) = Estado est Alterado (moveJogador (Jogo mapa4dojogo (Jogador (x,y) Este False)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa5) (Jogo mapa5dojogo (Jogador (x,y) Este False)) i) = Estado est Alterado (moveJogador (Jogo mapa5dojogo (Jogador (x,y) Este False)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa6) (Jogo mapa6dojogo (Jogador (x,y) Este False)) i) = Estado est Alterado (moveJogador (Jogo mapa6dojogo (Jogador (x,y) Este False)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Este False)) i) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Este False)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Este True)) i) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Este True)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Este True)) i) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Este True)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa3) (Jogo mapa3dojogo (Jogador (x,y) Este True)) i) = Estado est Alterado (moveJogador (Jogo mapa3dojogo (Jogador (x,y) Este True)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa4) (Jogo mapa4dojogo (Jogador (x,y) Este True)) i) = Estado est Alterado (moveJogador (Jogo mapa4dojogo (Jogador (x,y) Este True)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa5) (Jogo mapa5dojogo (Jogador (x,y) Este True)) i) = Estado est Alterado (moveJogador (Jogo mapa5dojogo (Jogador (x,y) Este True)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa6) (Jogo mapa6dojogo (Jogador (x,y) Este True)) i) = Estado est Alterado (moveJogador (Jogo mapa6dojogo (Jogador (x,y) Este True)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Este True)) i) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Este True)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Oeste False)) i) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste False)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Oeste False)) i) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Oeste False)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa3) (Jogo mapa3dojogo (Jogador (x,y) Oeste False)) i) = Estado est Alterado (moveJogador (Jogo mapa3dojogo (Jogador (x,y) Oeste False)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa4) (Jogo mapa4dojogo (Jogador (x,y) Oeste False)) i) = Estado est Alterado (moveJogador (Jogo mapa4dojogo (Jogador (x,y) Oeste False)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa5) (Jogo mapa5dojogo (Jogador (x,y) Oeste False)) i) = Estado est Alterado (moveJogador (Jogo mapa5dojogo (Jogador (x,y) Oeste False)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa6) (Jogo mapa6dojogo (Jogador (x,y) Oeste False)) i) = Estado est Alterado (moveJogador (Jogo mapa6dojogo (Jogador (x,y) Oeste False)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Oeste False)) i) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Oeste False)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa1) (Jogo mapa1dojogo (Jogador (x,y) Oeste True)) i) = Estado est Alterado (moveJogador (Jogo mapa1dojogo (Jogador (x,y) Oeste True)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa2) (Jogo mapa2dojogo (Jogador (x,y) Oeste True)) i) = Estado est Alterado (moveJogador (Jogo mapa2dojogo (Jogador (x,y) Oeste True)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa3) (Jogo mapa3dojogo (Jogador (x,y) Oeste True)) i) = Estado est Alterado (moveJogador (Jogo mapa3dojogo (Jogador (x,y) Oeste True)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa4) (Jogo mapa4dojogo (Jogador (x,y) Oeste True)) i) = Estado est Alterado (moveJogador (Jogo mapa4dojogo (Jogador (x,y) Oeste True)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa5) (Jogo mapa5dojogo (Jogador (x,y) Oeste True)) i) = Estado est Alterado (moveJogador (Jogo mapa5dojogo (Jogador (x,y) Oeste True)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est (Play Mapa6) (Jogo mapa6dojogo (Jogador (x,y) Oeste True)) i) = Estado est Alterado (moveJogador (Jogo mapa6dojogo (Jogador (x,y) Oeste True)) InterageCaixa) i
reageEvento (EventKey (SpecialKey KeyDown ) Down _ _) (Estado est Alterado (Jogo mp (Jogador (x,y) Oeste True)) i) = Estado est Alterado (moveJogador (Jogo mp (Jogador (x,y) Oeste True)) InterageCaixa) i
reageEvento _ s = s 


{-
playGame:: Estado -> Picture
playGame (Estado OpcaoNovojogo Alterado (Jogo m (Jogador (x,y) dir tf))) = pictures (treatGame (moveJogador (Jogo m (Jogador (x,y) dir tf)) AndarEsquerda))
-}

treatGame :: [Picture] -> Jogo -> [Picture]
treatGame (i:is) j = transf (i:is) j (-200,200) (0,0)

transf::[Picture] -> Jogo -> (Float,Float) -> (Int,Int) -> [Picture]
transf (i:is) (Jogo [] _ ) _  _ = []
transf (i:is) (Jogo [h] (Jogador (x,y) d tf)) (a,b) (cx,cy) = drawMap (i:is) h (Jogador (x,y) d tf) a b (cx,cy)
transf (i:is) (Jogo (h:t) (Jogador (x,y) d tf)) (a,b) (cx,cy) = drawMap (i:is) h (Jogador (x,y) d tf) a b (cx,cy) ++ transf (i:is) (Jogo t (Jogador (x,y) d tf)) (-450,b-50) (0,cy+1) 

-- | Desenha os mapas em uma lista de pictures 
drawMap::[Picture] -> [Peca] -> Jogador -> Float -> Float -> (Int,Int) -> [Picture] 
drawMap i [h] (Jogador (x,y) d tf) a b (cx,cy)
        | x == cx && y == cy = [Translate a b (scale 0.1953125 0.1953125 ((!!) i 3) )] 
        | h == Bloco = [Translate a b (scale 0.1953125 0.1953125  ((!!) i 0 ))] 
        | h == Vazio = [Translate a b Blank] 
        | h == Porta = [Translate a b (scale 0.1953125 0.1953125 ((!!) i 1) )] 
        | h == Caixa = [Translate a b (scale 0.1953125 0.1953125 ((!!) i 2))]
--drawMap ([]:ys) a b  = drawMap ys (-450) (b-100)
drawMap i (h:t) (Jogador (x,y) d tf) a b  (cx,cy)
        | tf && cy == y-1 && cx == x = Translate a b (scale 0.1953125 0.1953125 ((!!) i 2)) : drawMap i t (Jogador (x,y) d tf) (a + 50) b (cx + 1,cy)
        | x == cx && y == cy && d == Este = Translate a b (scale 0.1953125 0.1953125 ((!!) i 4)) : drawMap i t (Jogador (x,y) d tf) (a + 50) b (cx + 1,cy)
        | x == cx && y == cy && d == Oeste = Translate a b (scale 0.1953125 0.1953125 ((!!) i 3)) : drawMap i t (Jogador (x,y) d tf) (a + 50) b (cx + 1,cy)        
        | h == Bloco = Translate a b (scale 0.1953125 0.1953125  ((!!) i 0)) : drawMap i t (Jogador (x,y) d tf) (a + 50) b (cx + 1,cy)
        | h == Vazio = Translate a b Blank : drawMap i t (Jogador (x,y) d tf) (a + 50) b (cx + 1,cy)
        | h == Porta = Translate a b (scale 0.1953125 0.1953125 ((!!) i 1)) : drawMap i t (Jogador (x,y) d tf) (a + 50) b (cx + 1,cy)
        | h == Caixa = Translate a b (scale 0.1953125 0.1953125 ((!!) i 2)) : drawMap i t (Jogador (x,y) d tf) (a + 50) b (cx + 1,cy)         




reageTempo :: Float -> Estado -> Estado
reageTempo n (Estado OpcaoNovojogo gam (Jogo m jogd ) i )
                         | giveWin (Estado OpcaoNovojogo gam (Jogo m jogd ) i )  &&  m == mapa1dojogo = Estado (OpcaoEscolherMapa Mapa2) (Play Mapa2) (Jogo mapa2dojogo (Jogador (2,3) Este False)) i  -- TRATAR DO MAPA!!
                         | giveWin (Estado OpcaoNovojogo gam (Jogo m jogd ) i) &&  m == mapa2dojogo = Estado OpcaoNovojogo Nada (Jogo mapa1dojogo (Jogador (10,11) Este False)) i
reageTempo n (Estado (OpcaoEscolherMapa mp) gam (Jogo m jogd) i ) 
                         | giveWin (Estado (OpcaoEscolherMapa mp) gam (Jogo m jogd)i) = Estado (OpcaoEscolherMapa Mapa1) Nada (Jogo mapa1dojogo (Jogador (10,11) Este False)) i                       
reageTempo _ s = s




main :: IO ()
main = do 
      Just bloco     <- loadJuicy "img/bloco.png" 
      Just porta     <- loadJuicy "img/portal.png"
      Just caixa     <- loadJuicy "img/caixa.png"
      Just playeresq <- loadJuicy "img/playeresq.png"
      Just playerdir <- loadJuicy "img/playerdir.png"
      Just mainbackg <- loadJuicy "img/spacebackground.png"
      play window background fr (estadoInicial [bloco,porta,caixa,playeresq,playerdir]) drawEstado reageEvento reageTempo 

-- (10,10)
mapa3dojogo :: Mapa 
mapa3dojogo = [[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
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

-- (8,10)
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


-- (11,10)
mapa1dojogo :: Mapa 
mapa1dojogo = [[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
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

-- (3,10)
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


-- (16,8)
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

-- (14,7)
mapa6dojogo :: Mapa 
mapa6dojogo = [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
               [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
               [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco],
               [Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
               [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Caixa, Caixa, Bloco],
               [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco],
               [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco],
               [Bloco, Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco],
               [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
               [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Caixa, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco],
               [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio],
               [Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio],
               [Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio],
               [Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]]
