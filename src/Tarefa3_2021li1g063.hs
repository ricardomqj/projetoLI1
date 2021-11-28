{- |
Module      : Tarefa3_2021li1g063
Description : Representação textual do jogo
Copyright   : Ricardo Miguel Queirós de Jesus <a100066@alunos.uminho.pt>;
            : Rui Pinto <a100659@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g063 where

import LI12122

instance Show Jogo where
     show = jogoParaString 

{- 
TAREFA 3
O objetivo da Tarefa 3 é tornar o tipo de dados Jogo uma instância da class Show de acordo com as formatações pedidas no enunciado.
-}
-- | Vai imprimir o Jogo 

printJogo:: Jogo -> IO() 
printJogo j = putStrLn $ jogoParaString j

jogoParaString:: Jogo -> String 
jogoParaString j = transformador j (0,0) 

-- | Função que servir para tirar as listas da String 

transformador:: Jogo -> (Int,Int) -> String  
transformador (Jogo [] _ ) _ = []
transformador (Jogo [h] (Jogador (x,y) d tf)) (a,b) = transformadorLinha h (Jogador (x,y) d tf) (a,b)
transformador (Jogo (h:t) (Jogador (x,y) d tf)) (a,b) 
                        |tf  && b == y -1  = transformadorLinhaCaixa  h x (a,b) ++  ['\n'] ++ transformador (Jogo t (Jogador (x,y) d tf)) (a,b+1)
                        |otherwise = transformadorLinha h (Jogador (x,y) d tf) (a,b) ++ ['\n'] ++ transformador (Jogo t (Jogador (x,y) d tf)) (a,b+1)  

-- | vai percorrer a linha e transformar cada Peca no correspondente Char e adicionaro Jogador  

transformadorLinha:: [Peca] -> Jogador -> (Int,Int) -> String 
transformadorLinha [] _ _ = [] 
transformadorLinha (h:t) (Jogador (x,y) d tf) (a,b) 
            | x == a && y == b = jogadorToChar (Jogador (x,y) d tf) : transformadorLinha t (Jogador (x,y) d tf) (a+1,b) 
            | otherwise = pecaToChar h : transformadorLinha t (Jogador (x,y) d tf) (a+1,b) 

-- | Função que no caso que o jgador carrega uma caixa, vai adicionar essa caixa a String 

transformadorLinhaCaixa:: [Peca] -> Int -> (Int,Int) -> String 
transformadorLinhaCaixa [] _ _ = []
transformadorLinhaCaixa (h:t) n (a,b) 
                | n == a = 'C' : transformadorLinhaCaixa t n (a+1,b)
                | otherwise = pecaToChar h : transformadorLinhaCaixa t n (a+1,b) 

-- | função que atribui a cada peça a formatação correspondente

pecaToChar:: Peca -> Char 
pecaToChar p 
    | p == Bloco = 'X'
    | p == Caixa = 'C'
    | p == Porta = 'P'
    | p == Vazio = ' '  

-- | função que atribui ao Jogador a formatação correspondente, tendo em conta a sua direção

jogadorToChar:: Jogador -> Char 
jogadorToChar (Jogador _ d _)
    | d == Este  = '>' 
    | d == Oeste = '<'




