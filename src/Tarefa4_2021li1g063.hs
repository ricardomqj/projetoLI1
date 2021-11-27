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


 ̣{- Explicação da função correrMovimentos
 
 -}


-- correrMovimentos aplica consecutivamente os comandos dados pela lista de movimentos
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos (Jogo listapecas (Jogador (x,y) dirj bool)) (h:t) 
    | (dirj == Oeste) && (h == AndarEsquerda) = correrMovimentos (Jogo listapecas (Jogador (x-1,y) Oeste bool)) t 
    | (dirj == Oeste) && (h == AndarDireita) = correrMovimentos (Jogo listapecas (Jogador (x+1,y) Este bool)) t 
    | (dirj == Este) && (h == AndarEsquerda) = correrMovimentos (Jogo listapecas (Jogador (x-1,y) Oeste bool)) t 
    | (dirj == Este) && (h == AndarDireita) = correrMovimentos (Jogo listapecas (Jogador (x+1,y) Este bool)) t 
    | (h == InterageCaixa) && (bool == True) = correrMovimentos (Jogo listapecas (Jogador (x,y) dirj False)) t
    | (h == InterageCaixa) && (bool == False) = correrMovimentos (Jogo listapecas (Jogador (x,y) dirj True)) t 
    


-- função que verifica se é possivel trepar o obstáculo

podeTrepar :: Jogo -> (Peca, Coordenadas) -> Bool 
podeTrepar (Jogo (peca:t) (Jogador (xj,yj) dirj bool)) (p,(x,y))
    | (p == Bloco) && (x == xj + 1) && (dirj == Este) && (obstaculoAlto == False) = True
    | (p == Caixa) && (x == xj + 1) && (dirj == Este) && (obstaculoAlto == False) = True 
    | (p == Porta) && (x == xj + 1) && (dirj == Este) = False 
    | (p == Bloco) && (x == xj - 1) && (dirj == Oeste) && (obstaculoAlto == False) = True 
    | (p == Caixa) && (x == xj - 1) && (dirj == Oeste) && (obstaculoAlto == False) = True 
    | otherwise = False 

-- obstáculoAlto -> função que verifica se à direita do Jogador o obstáculo é maior que uma peça de altura

obstaculoAlto :: Jogo -> [(Peca, Coordenadas)] -> Bool 
obstaculoAlto (Jogo (peca:t) (Jogador (xj,yj) dirj bool)) ((peca, (x,y)):t)
    | (x == xj + 1) && (y == yj + 1) && (peca == Caixa || peca == Bloco) = True 
    | (x == xj - 1) && (y == yj - 1) && (peca == Caixa || peca == Bloco) = True 
    | otherwise = False




