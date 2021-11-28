module Fixtures where

import LI12122

m1 :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

pecaCordBlocoSolt:: [(Peca, Coordenadas)]
pecaCordBlocoSolt =
   [(Bloco, (0,2)),
    (Bloco, (1,2)),
    (Caixa, (2,0)),
    (Bloco, (2,2))
  ]

mapaBlocoSolt:: Mapa
mapaBlocoSolt =
  [[Vazio,Vazio,Vazio],
   [Vazio,Vazio,Vazio],
   [Bloco,Bloco,Bloco]
  ]  

pecaCordMesmaCord:: [(Peca, Coordenadas)]
pecaCordMesmaCord = 
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1)),
    (Bloco, (6, 1))
  ]

pecaCordMapaCheio:: [(Peca, Coordenadas)]
pecaCordMapaCheio = 
  [ (Bloco, (0, 0)),
    (Bloco, (1, 0)),
    (Bloco, (0, 1)),
    (Porta, (1, 1))
  ]

pecaCordChaoVazioPorta:: [(Peca, Coordenadas)]
pecaCordChaoVazioPorta = 
  [ (Bloco, (0, 1)),
    (Porta, (1, 0)),
    (Bloco, (1, 1))
  ]

pecaCordVazioDef:: [(Peca, Coordenadas)]
pecaCordVazioDef = 
  [ (Vazio, (0, 0)),
    (Bloco, (0, 1)),
    (Porta, (1, 0)),
    (Bloco, (1, 1))
  ]

pecaCordCaminhoUnico:: [(Peca, Coordenadas)]
pecaCordCaminhoUnico =
  [   (Bloco, (0, 2)),
      (Bloco, (1, 2)),
      (Bloco, (2, 2)),
      (Bloco, (2, 1)),
      (Bloco, (3, 1)),
      (Porta, (3, 0))
  ]

pecaCordMultCaminhos:: [(Peca, Coordenadas)]
pecaCordMultCaminhos =
  [   (Bloco, (0, 2)),
      (Bloco, (1, 2)),
      (Bloco, (1, 3)),
      (Bloco, (2, 3)),
      (Bloco, (3, 1)),
      (Porta, (3, 0))
  ]

jogadorComCaixa:: Mapa 
jogadorComCaixa =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]    

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)


jogadorCaixa:: Jogo
jogadorCaixa = Jogo jogadorComCaixa (Jogador (3,2) Este True)
