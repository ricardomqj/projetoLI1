{- |
Module      : Tarefa1_2021li1g063
Description : Validação de um potencial mapa
Copyright   : Ricardo Miguel Queirós de Jesus <a100066@alunos.uminho.pt>;
            : Rui Pinto <a100659@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g063 where

type Coordenadas = (Int,Int) 
data Peca = Bloco | Porta | Caixa | Vazio 
    deriving (Show, Eq) 
type Mapa = [[Peca]] 

data Direcao = Este | Oeste 
data Jogador = Jogador Coordenadas Direcao Bool 

data Jogo = Jogo Mapa Jogador 

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False 
validaPotencialMapa (h:t) 
        | validaposicoes (h:t) == True && vereficarcaixas (h:t) == True  && verificaVazio (h:t) == True = True   
        | otherwise = False


--não haver posições repetidas 

naorepetirposicao:: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Bool                  -- a função vai vereficar se as coordenadas de uma peça se repetem em outra peça 
naorepetirposicao _ [] = True  
naorepetirposicao (p1,(x1,y1)) ((p2,(x2,y2)):t)
        | x1 == x2 && y1==y2 = False 
        | otherwise = naorepetirposicao (p1,(x1,y1)) t 
 

validaposicoes:: [(Peca,Coordenadas)] -> Bool  
validaposicoes [] = True 
validaposicoes (h:t) 
        | naorepetirposicao h t  = validaposicoes t 
        | otherwise = False 



-- vereficar se a caixa não está a fultuar 

vereficarcaixas::[(Peca,Coordenadas)] -> Bool 
vereficarcaixas [] = True
vereficarcaixas ((p,(x,y)):t)
           |p == Caixa    = if (vereficardebaixo (p,(x,y)) ((p,(x,y)):t)) == True then vereficarcaixas t else False     --verificarcaixas t       --nao verifica a lista toda 
           |otherwise     = vereficarcaixas t 

vereficardebaixo:: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Bool              
vereficardebaixo _ [] = False
vereficardebaixo (p1,(x1,y1)) ((p2,(x2,y2)):t) 
        | p2 == Caixa && x2 == x1 && y2 == y1 + 1 = True 
        | p2 == Bloco && x2 == x1 && y2 == y1 + 1 = True  
        | otherwise = vereficardebaixo (p1,(x1,y1)) t 



-- vereficar se há espaços vazios pelo menos um espaço vazio 
 
verificaVazio:: [(Peca,Coordenadas)] -> Bool 
verificaVazio [] = False
verificaVazio ((p,(x,y)):t) 
        |procurarVazio ((p,(x,y)):t) == True = True                                                    
        |tamanhodoMapa (maiorelementodaLista (x,y) t) > length ((p,(x,y)):t) = True 
        |otherwise = False 

--procura por a existencia de vazios definidos 

procurarVazio::[(Peca,Coordenadas)] -> Bool 
procurarVazio [] = False 
procurarVazio ((p,(x,y)):t) 
                |p == Vazio  = True 
                |otherwise = procurarVazio t

--calcula o nº total de elementos que um mapa tem ao todo 

tamanhodoMapa:: (Int,Int) -> Int 
tamanhodoMapa (0,y) = y+1 
tamanhodoMapa (x,0) = x+1 
tamanhodoMapa (x,y) = (x+1) * (y+1)  

--verefica quais são os maiores x e y de uma lista 

maiorelementodaLista::(Int,Int) -> [(Peca,Coordenadas)] -> (Int,Int) 
maiorelementodaLista (x,y) [] = (x,y) 
maiorelementodaLista (xm,ym) ((p,(x,y)):t)
                | x > xm && y > ym = maiorelementodaLista (x,y) t 
                | x == xm && y > ym = maiorelementodaLista (x,y) t 
                | x > xm && y == ym = maiorelementodaLista (x,y) t 
                | otherwise = maiorelementodaLista (xm,ym) t 

