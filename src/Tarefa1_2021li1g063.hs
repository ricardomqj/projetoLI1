{- |
Module      : Tarefa1_2021li1g063
Description : Validação de um potencial mapa
Copyright   : Ricardo Miguel Queirós de Jesus <a100066@alunos.uminho.pt>;
            : Rui Pinto <a100659@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g063 where

import LI12122


{- 
type Coordenadas = (Int, Int)
data Peca = Bloco | Porta | Caixa | Vazio deriving (Show, Eq) 
type Mapa = [(Peca)] 
-}





validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa pecas = undefined 

--1
naoRepetirPosicao :: (Peca, Coordenadas) -> [(Peca,Coordenadas)] -> Bool 
naoRepetirPosicao _ [] = True 
naoRepetirPosicao (p1,(x1,y1)) ((p2,(x2,y2)):t) 
    | (x1 == x2) && (y1 == y2) = False
    | otherwise = naoRepetirPosicao (p1,(x1,y1)) t 

-- 2

{- 
O objetivo do ponto 2 é garantir que o mapa contém apenas uma porta.
Ora, a função numPortas conta o número de portas contidas no mapa.
Então, o ponto 2 apenas é verdadeiro caso o número de portas seja igual a 1 
A função numPortas diz se há apenas uma porta(True) ou não(False)
-}
numPortas :: [(Peca, Coordenadas)] -> Int 
numPortas [] = 0 
numPortas ((p, c):t) = case p of Porta -> 1 + numPortas t 
                                 Bloco -> numPortas t 
                                 Caixa -> numPortas t 
                                 Vazio -> numPortas t 

soUmaPorta :: [(Peca, Coordenadas)] -> Bool 
soUmaPorta [] = False 
soUmaPorta l = if numPortas l == 1 then True else False 
   
-- 5 

-- função principal 

-- calcula a peça que tenha o y maior, ou  seja, que seja uma das peças da base 
yMaior :: [(Peca, Coordenadas)] -> Int 
yMaior [] = 0 
yMaior [(p, (x,y))] = y 
yMaior ((p1,(x1,y1)):(p2,(x2,y2)):t)
    | y2 > y1 && x2 > x1 = yMaior (peca1:t)
    | otherwise = yMaior (peca2:t)
    where peca1 = (p1, (x1,y1))
          peca2 = (p2, (x2,y2))

-- devolve uma lista apenas com os blocos da base
listaBlocosBase :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] 
listaBlocosBase [] = []
listaBlocosBase ((p,(x,y)):t) 
    | y == yMaior ((p,(x,y)):t) = peca1' : listaBlocosBase t  
    | otherwise = listaBlocosBase t 
    where peca1' = (p,(x,y))

-- verifica se exitem espaços vazios entre os blocos da base 

verificaEspVaziosBase :: [(Peca, Coordenadas)] -> Bool 
verificaEspVaziosBase [] = False 
verificaEspVaziosBase ((p1,(x1,y1)):(p2,(x2,y2)):t)  
    
