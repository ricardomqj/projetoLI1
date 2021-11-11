{- |
Module      : Tarefa1_2021li1g063
Description : Validação de um potencial mapa
Copyright   : Ricardo Miguel Queirós de Jesus <a100066@alunos.uminho.pt>;
            : Rui Pinto <a100659@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g063 where

import LI12122


type Coordenadas = (Int, Int)
data Peca = Bloco | Porta | Caixa | Vazio deriving (Show, Eq) 
type Mapa = [(Peca)]

data Direcao = Este | Oeste 
data Jogador = Jogador Coordenadas Direcao Bool 



validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa pecas = undefined 

naorepetirposicao :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Bool 
naorepetirposicao _ [] = True 
noarepetirposicao (p1,(x1,y1)) ((p2,(x2,y2)):t) 
    | (x1 == x2) && (y1 == y2) = False
    | otherwise = naorepetirposicao (p1,(x1,y1)) t 

soumaporta :: [(Peca,Coordenadas)] -> Bool 
soumaporta [] = False 
soumaporta ((p1,(x1,y1)):t) 
    | p1 == Porta &&  temporta t = False 
    | p1 /= Porta = soumaporta t
    | otherwise = True 

temporta:: (Peca,Coordenadas) -> Bool 
temporta (p1, (x1,y1))  
    | p1 == Porta = True
    | otherwise = False
 

{- 
soumaporta ((p1,(x1,y1)):t)
    | p1 == Porta && Porta elem t = False
    | p1 /= Porta = soumaporta t  
    | otherwise = True 
-}

-- ver elemMSet 

