module Tarefa1_2021li1g063_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g063
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    , "Tarefa 1 - Teste Valida Mapa Caixa flutuante" ~: validaPotencialMapa pecaCordBlocoSolt ~=? False
    , "Tarefa 1 - Teste Valida Mapa Pecas com a mesma coordenada" ~: validaPotencialMapa pecaCordMesmaCord ~=? False
    , "Tarefa 1 - Teste Valida Mapa Cheio" ~: validaPotencialMapa pecaCordMapaCheio ~=? False
    , "Tarefa 1 - Teste Valida Mapa com bloco chao, vazio e porta" ~: validaPotencialMapa pecaCordChaoVazioPorta ~=? True
    , "Tarefa 1 - Teste Valida Mapa Vazio Definido" ~: validaPotencialMapa pecaCordVazioDef ~=? True
    , "Tarefa 1 - Teste Valida Mapa Caminho Completo" ~: validaPotencialMapa pecaCordCaminhoUnico ~=? True
    , "Tarefa 1 - Teste Valida Mapa Caminho Incompleto" ~: validaPotencialMapa pecaCordMultCaminhos ~=? False
    ]
