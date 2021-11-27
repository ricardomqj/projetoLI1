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

printJogo:: a -> IO() 
printJogo j = putStrLn "Olá Mundo" 

jogoParaString:: Jogo -> String 
jogoParaString j = " "

transformador:: Jogo -> (Int,Int) -> String  
transformador (Jogo _ (Jogador (x,y) d tf)) (a,b) 
                | x == a && y == b = jogadorToChar (Jogador (x,y) d tf) ++ transformador (Jogo _ )


pecaToChar:: Peca -> Char 
pecaToChar p 
    | p == Bloco = 'X'
    | p == Caixa = 'C'
    | p == Porta = 'P'
    | p == Vazio = ' '  


jogadorToChar:: Jogador -> Char 
jogadorToChar (Jogador _ d _)
    | d == Este  = '<' 
    | d == Oeste = '>'

