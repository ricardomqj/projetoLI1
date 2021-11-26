{- |
Module      : Tarefa4_2021li1g063
Description : Movimentação do personagem
Copyright   : Ricardo Miguel Queirós de Jesus <a100066@alunos.uminho.pt>;
            : Rui Pinto <a100659@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g063 where

import LI12122


moveJogador :: Jogo -> Movimento -> Jogo
moveJogador jogo movimento = undefined

-- correrMovimentos aplica consecutivamente os comandos dados pela lista de movimentos
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos (Jogo listapecas (Jogador (x,y) dir bool)) (h:t) 
    | (dir == Oeste) && (h == AndarEsquerda) = correrMovimentos (Jogo listapecas (Jogador (x-1,y) Oeste bool)) t 
    | (dir == Oeste) && (h == AndarDireita) = correrMovimentos (Jogo listapecas (Jogador (x,y) Este bool)) t 
    | (dir == Este) && (h == AndarEsquerda) = correrMovimentos (Jogo listapecas (Jogador (x,y) Oeste bool)) t 
    | (dir == Este) && (h == AndarDireita) = correrMovimentos (Jogo listapecas (Jogador (x+1,y) Este bool)) t 
    | (h == InterageCaixa) && (bool == True) = correrMovimentos (Jogo listapecas (Jogador (x,y) dir False)) t
    | (h == InterageCaixa) && (bool == False) = correrMovimentos (Jogo listapecas (Jogador (x,y) dir True)) t 


-- função que verifica se é possível trepar o obstáculo imediatamente à sua frente
podeTreparEsq :: Jogador -> (Peca, Coordenadas) -> Bool 
podeTreparEsq (Jogador (x1,y1) dir bool) (pecaseguinte, (x1 - 1,y2))
    | pecaseguinte == Porta || Vazio = False 
    | 

-- devolve a peça com o menor y (peça mais alta de uma determinada coluna)

pecaMaisAltaColuna :: [(Peca, Coordenadas)] -> (Peca, Coordenadas)
pecaMaisAltaColuna [e] = e  
pecaMaisAltaColuna ((peca1, (x1,y1)):xs:t) 
    | x1 /= x2 = []  
    | y1 > y2 = pecaMaisAltaColuna (xs:t)
    | otherwise = pecaMaisAltaColuna ()





