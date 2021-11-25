{- |
Module      : Tarefa2_2021li1g063
Description : Construção/Desconstrução do mapa
Copyright   : Ricardo Miguel Queirós de Jesus <a100066@alunos.uminho.pt>;
            : Rui Pinto <a100659@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g063 where

import LI12122

constroiMapa :: [(Peca, Coordenadas)] -> Mapa 
constroiMapa [] = []
constroiMapa ((p,(x,y)):t) = preencherListas (formarlistas ((p,(x,y)):t) 0 (maximoY (contay((p,(x,y)):t)) 0)) (maximoX (contax((p,(x,y)):t)) 0) 
           

contay::[(Peca,Coordenadas)] -> [Int]
contay [] = []
contay ((p,(x,y)):t) = y : contay t 

maximoY::[Int] -> Int -> Int 
maximoY [] ym = ym 
maximoY (h:t) ym 
        | h > ym = maximoY t h 
        | otherwise  = maximoY t ym 



contax::[(Peca,Coordenadas)] -> [Int]
contax [] = []
contax ((p,(x,y)):t) = x : contax t

maximoX::[Int] -> Int -> Int 
maximoX [] xm = xm 
maximoX (h:t) xm 
        | h > xm = maximoX t h 
        | otherwise  = maximoX t xm 


formarlistas::[(Peca,Coordenadas)] -> Int -> Int -> [[(Peca,Coordenadas)]]
formarlistas ((p,(x,y)):t) n ym 
                        | n == ym   = [procuraPecaCoord ((p,(x,y)):t) ym]
                        | otherwise = procuraPecaCoord ((p,(x,y)):t) n : formarlistas ((p,(x,y)):t) (n+1) ym  


procuraPecaCoord::[(Peca,Coordenadas)] -> Int -> [(Peca,Coordenadas)]
procuraPecaCoord _ 0 = []
procuraPecaCoord [] n = [] 
procuraPecaCoord ((p,(x,y)):t) n 
            | n == y = (p,(x,y)) : procuraPecaCoord t n 
            |otherwise = procuraPecaCoord t n 

preencherListas::[[(Peca,Coordenadas)]] -> Int -> [[Peca]] 
preencherListas [] _ = [] 
preencherListas ([]:t) xm = criaListadePecas [] 0 xm : preencherListas t xm 
preencherListas (((p,(x,y)):xs):t) xm = criaListadePecas (((p,(x,y)):xs)) 0 xm : preencherListas t xm 
        

criaListadePecas::[(Peca,Coordenadas)] -> Int -> Int  -> [Peca]
criaListadePecas [] n xm 
                |n == xm   = [Vazio]
                |otherwise = Vazio : criaListadePecas [] (n+1) xm 
criaListadePecas ((p,(x,y)):t) n xm
            | n == xm = [encontrarXpeca xm (((p,(x,y)):t))]
            | elem n (contax ((p,(x,y)):t))  = encontrarXpeca n ((p,(x,y)):t) : criaListadePecas ((p,(x,y)):t) (n+1) xm
            | otherwise = Vazio : criaListadePecas ((p,(x,y)):t) (n+1) xm

encontrarXpeca:: Int -> [(Peca,Coordenadas)] -> Peca 
encontrarXpeca _ [] = Vazio 
encontrarXpeca n ((p,(x,y)):t) 
                | n == x = p 
                | otherwise = encontrarXpeca n t            



desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = aux 0 0  mapa 

aux::Int -> Int -> Mapa -> [(Peca,Coordenadas)]
aux _ _ [] = []
aux cx cy ([]:t) = aux 0 (cy+1) t
aux cx cy ((h:xs):t)
        |h == Vazio = aux (cx+1) cy (xs:t) 
        |otherwise  = (h,(cx,cy)) : aux (cx+1) cy (xs:t)  

