{- |
Module      : Tarefa6_2021li1gXXX
Description : Resolução de um puzzle

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1gXXX where

import LI12122
import Outro 
import Tarefa4_2021li1g063 (moveJogador, correrMovimentos, printJogo)
import Tarefa2_2021li1g063 (desconstroiMapa)

data RTree a = R a [RTree a]
   deriving (Show)

-- | A função resolveJogo iŕa receber um número de movimentos e um Jogo, e vai tentar resolver o jogo nesse número de movimentos  

resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo 0 jogo | daWin jogo = Just [] 
                   | otherwise = Nothing                     
resolveJogo n (Jogo mp (Jogador coord dir tf)) = testPatchs (paths (inicRtMov n dir)) (Jogo mp (Jogador coord dir tf)) 

-- | A função testPatchs vai pegar em todos os possíveis caminhos gerados pela função paths e vai aplicalos ao jogo, quando encontrar uma sequência de movimentos que levem o jogador até a porta, a função vai entregar essa lista de movimentos, caso não haja entrega "Nothing"   

testPatchs:: [[Movimento]] -> Jogo -> Maybe [Movimento]
testPatchs [] jog = Nothing 
testPatchs (h:t) (Jogo mp (Jogador coord dir tf)) 
                     | daWin (correrMovimentos (Jogo mp (Jogador coord dir tf)) h) = Just h 
                     | otherwise  = testPatchs t (Jogo mp (Jogador coord dir tf))

{-
testMov::[Movimento] -> Jogo -> Bool  
testMov mov jogo | daWin (correrMovimentos jogo mov) = True
                 | otherwise = False  
-}

-- | As funçoẽs paths e listRTree vão premetir ir a uma RoseTree (criada pela Função inicRtMov) e retirar desta todos os possíveis caminhos que o bot pode efetuar para um determinado número de movimentos

paths::RTree a -> [[a]]
paths rt = listRTree [] rt

listRTree:: [a] -> RTree a -> [[a]]
listRTree l (R a []) = [l ++ [a]]
listRTree l (R a rs) = concat (map (listRTree (l ++ [a])) rs)                                                                                      

-- | A Função daWin vai comparar as coordenadas do jogador com as coordenadas da porta,permite saber se o jogador já completou o jogo

daWin::Jogo -> Bool
daWin (Jogo mp (Jogador coord dir tf))  | coord == snd (head(filter ((== Porta).fst) (desconstroiMapa mp))) =  True
                                        | otherwise = False

-- | A Função inicRtMov e a Função vão fucionar quase que como uma só, a inicRtMov vai dar inicio a uma RoseTree de Movimentos, com  movimento para a esquerda , consequentemente o bot vai começar sempre a tentar andar para a esquerda

inicRtMov:: Int ->  Direcao -> RTree  Movimento  
inicRtMov n dr = R AndarEsquerda [criaRtMov Oeste (n-1) AndarEsquerda, criaRtMov Oeste (n-1) Trepar , criaRtMov Oeste (n-1) AndarDireita, criaRtMov Oeste (n-1) InterageCaixa]

-- | A Função criaRtMov vai dar continuidade a inicRtMov, vai construi uma RoseTree de Movimentos com uma profundidade de n (dado pela função resolveJogo)

criaRtMov::Direcao -> Int -> Movimento -> RTree Movimento
criaRtMov dr 1 mov = R mov [] 
criaRtMov dr n AndarDireita = R AndarDireita [criaRtMov Este (n-1) AndarDireita, criaRtMov Este (n-1) Trepar , criaRtMov Este (n-1) InterageCaixa]
criaRtMov dr n AndarEsquerda  = R AndarEsquerda  [criaRtMov Oeste (n-1) AndarEsquerda, criaRtMov Oeste (n-1) Trepar , criaRtMov Oeste (n-1) InterageCaixa]
criaRtMov dr n InterageCaixa   = R InterageCaixa   [criaRtMov dr (n-1) AndarEsquerda, criaRtMov dr (n-1) Trepar , criaRtMov dr (n-1) AndarDireita ]
criaRtMov Este n Trepar  = R Trepar  [criaRtMov Este (n-1) Trepar , criaRtMov Este (n-1) AndarDireita, criaRtMov Este (n-1) InterageCaixa]  
criaRtMov Oeste n Trepar  = R Trepar  [criaRtMov Oeste (n-1) AndarEsquerda, criaRtMov Oeste (n-1) Trepar , criaRtMov Oeste (n-1) InterageCaixa]  
