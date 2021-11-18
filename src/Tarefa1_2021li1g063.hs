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


numPortas :: [(Peca, Coordenadas)] -> Int 
numPortas [] = 0 
numPortas ((p, c):t) = case p of Porta -> 1 + numPortas t 
                                 Bloco -> numPortas t 
                                 Caixa -> numPortas t 
                                 Vazio -> numPortas t 

soUmaPorta :: [(Peca, Coordenadas)] -> Bool 
soUmaPorta [] = False 
soUmaPorta l = if numPortas l == 1 then True else False 
   



 




--fazer uma função que conte o numero de portas e depois se for =1 = True 


-- ver elemMSet 

