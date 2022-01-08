{- |
Module      : Tarefa6_2021li1gXXX
Description : Resolução de um puzzle

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1gXXX where

import LI12122
import Outro 
import Tarefa4_2021li1g063 (moveJogador, correrMovimentos)
import Tarefa2_2021li1g063 (desconstroiMapa)

data RTree a = R a [RTree a]
   deriving (Show)
 

resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo 0 jogo | daWin jogo = Just [] 
                   | otherwise = Nothing                     
resolveJogo n (Jogo mp (Jogador coord dir tf)) = testPatchs (paths (inicRtMov n dir)) (Jogo mp (Jogador coord dir tf)) 

testPatchs:: [[Movimento]] -> Jogo -> Maybe [Movimento]
testPatchs [] jog = Nothing 
testPatchs (h:t) (Jogo mp (Jogador coord dir tf)) 
                     | daWin (correrMovimentos (Jogo mp (Jogador coord dir tf)) h) = Just h 
                     | otherwise  = testPatchs t (Jogo mp (Jogador coord dir tf))


paths::RTree a -> [[a]]
paths rt = listRTree [] rt 

listRTree:: [a] -> RTree a -> [[a]]
listRTree l (R a []) = [l ++ [a]]
listRTree l (R a rs) = concat (map (listRTree (l ++ [a])) rs)



extractPath:: RTree Movimento -> [[Movimento]]
extractPath (R  mov []) = [[mov]]
extractPath (R mov l) = map ((:) mov . concat  . extractPath) l 
                                                                                      

daWin::Jogo -> Bool
daWin (Jogo mp (Jogador coord dir tf))  | coord == snd (head(filter ((== Porta).fst) (desconstroiMapa mp))) =  True
                                        | otherwise = False


inicRtMov:: Int ->  Direcao -> RTree  Movimento  
inicRtMov n dr = R AndarEsquerda [criaRtMov dr (n-1) AndarEsquerda, criaRtMov dr (n-1) Trepar , criaRtMov dr (n-1) AndarDireita, criaRtMov dr (n-1) InterageCaixa]

criaRtMov::Direcao -> Int -> Movimento -> RTree Movimento
criaRtMov dr 1 mov = R mov [] 
criaRtMov dr n AndarDireita = R AndarDireita [criaRtMov Este (n-1) AndarDireita, criaRtMov Este (n-1) Trepar , criaRtMov Este (n-1) InterageCaixa]
criaRtMov dr n AndarEsquerda  = R AndarEsquerda  [criaRtMov Oeste (n-1) AndarEsquerda, criaRtMov Oeste (n-1) Trepar , criaRtMov Oeste (n-1) InterageCaixa]
criaRtMov dr n InterageCaixa   = R InterageCaixa   [criaRtMov dr (n-1) AndarEsquerda, criaRtMov dr (n-1) Trepar , criaRtMov dr (n-1) AndarDireita ]
criaRtMov Este n Trepar  = R Trepar  [criaRtMov Este (n-1) Trepar , criaRtMov Este (n-1) AndarDireita, criaRtMov Este (n-1) InterageCaixa]  
criaRtMov Oeste n Trepar  = R Trepar  [criaRtMov Oeste (n-1) AndarEsquerda, criaRtMov Oeste (n-1) Trepar , criaRtMov Oeste (n-1) InterageCaixa]  
