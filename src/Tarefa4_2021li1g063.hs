{- |
Module      : Tarefa4_2021li1g063
Description : Movimentação do personagem
Copyright   : Ricardo Miguel Queirós de Jesus <a100066@alunos.uminho.pt>;
            : Rui Pinto <a100659@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g063 where

import LI12122
    ( Coordenadas,
      Direcao(Oeste, Este),
      Jogador(Jogador),
      Jogo(..),
      Mapa,
      Movimento(..),
      Peca(Caixa, Bloco, Vazio) )


moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo m (Jogador (x,y) d tf)) mov 
                | mov == AndarDireita || mov == AndarEsquerda = movimentoNaLateral (Jogo m (Jogador (x,y) d tf)) mov
                | mov == Trepar = trepar (Jogo m (Jogador (x,y) d tf))
                | mov == InterageCaixa = interageCaixa (Jogo m (Jogador (x,y) d tf))


-- Interage Caixa 

interageCaixa:: Jogo -> Jogo 
interageCaixa (Jogo m (Jogador (x,y) d tf)) 
           |tf == False && d == Este  &&  pecaCoordenada m (x+1,y) (0,0) == Caixa && (pecaCoordenada m (x+1,y-1) (0,0) == Bloco || pecaCoordenada m (x+1,y-1) (0,0) == Caixa)  =  (Jogo m (Jogador (x,y) d tf))    -- tenta pegar numa caixa a sua direita, há obstáculo
           |tf ==  False && d == Oeste  && pecaCoordenada m (x-1,y) (0,0) == Caixa && (pecaCoordenada m (x-1,y-1) (0,0) == Bloco || pecaCoordenada m (x-1,y-1) (0,0) == Caixa) = (Jogo m (Jogador (x,y) d tf))     -- tesnta oegar numa caixa a sua esq, há obstáculo
           | tf == False && d == Este  &&  pecaCoordenada m (x+1,y) (0,0) == Caixa = (Jogo (tirarCaixa m (x+1,y) 0 ) (Jogador (x,y) d True))                                                                       -- pega numa caixa que está a sua direita
           | tf == False && d == Oeste && pecaCoordenada m (x+1,y) (0,0) == Caixa  = (Jogo (tirarCaixa m (x-1,y) 0 ) (Jogador (x,y) d True))                                                                       -- pega numa caixa que está a sua esquerda
           | tf == False && (d == Oeste || d == Este) =  (Jogo m (Jogador (x,y) d tf))                                                                                                                             -- nãohá caixa para pegar 
           |tf == True && d == Este  && (pecaCoordenada m (x+1,y-1) (0,0) == Bloco || pecaCoordenada m (x+1,y-1) (0,0) == Caixa) && (pecaCoordenada m (x+1,y-2) (0,0) == Bloco || pecaCoordenada m (x+1,y-2) (0,0) == Caixa) = (Jogo m (Jogador (x,y) d tf)) -- largar caixa mas há obstáculo
           |tf == True && d == Oeste && (pecaCoordenada m (x-1,y-1) (0,0) == Bloco || pecaCoordenada m (x-1,y-1) (0,0) == Caixa) && (pecaCoordenada m (x-1,y-2) (0,0) == Bloco || pecaCoordenada m (x-1,y-2) (0,0) == Caixa) = (Jogo m (Jogador (x,y) d tf)) -- largar caixa mas há obstáculo 
           | tf == True && d == Este  && (pecaCoordenada m (x+1,y-1) (0,0) == Bloco || pecaCoordenada m (x+1,y-1) (0,0) == Caixa) = (Jogo (tirarVazio m (x+1,y-1) 0) (Jogador (x,y) d False))                         -- largar caixa em cima de outra caixa/bloco
           | tf == True && d == Oeste &&  (pecaCoordenada m (x-1,y-1) (0,0) == Bloco || pecaCoordenada m (x-1,y-1) (0,0) == Caixa) = (Jogo (tirarVazio m (x-1,y-1) 0) (Jogador (x,y) d False))                        -- largar caixa em cima de outra caixa/bloco
           |tf == True && (d == Este || d == Oeste )  = deixaCairCaixa (Jogo m (Jogador (x,y) d tf)) (x,y) 


deixaCairCaixa::Jogo -> Coordenadas -> Jogo 
deixaCairCaixa (Jogo m (Jogador (x,y) d tf)) (a,b)
                | d == Este && (pecaCoordenada m (a+1,b+1) (0,0) == Bloco || pecaCoordenada m (a+1,b-1) (0,0) == Caixa) = (Jogo (tirarVazio m (a+1,b) 0)  (Jogador (x,y) Este False))
                | d == Oeste && (pecaCoordenada m (a-1,b+1) (0,0) == Bloco || pecaCoordenada m (a-1,b-1) (0,0) == Caixa) = (Jogo (tirarVazio m (a-11,b) 0)  (Jogador (x,y) Oeste False))
                | d == Este = deixaCairCaixa  (Jogo m (Jogador (x,y) d tf))  (a,b+1)
                | d == Oeste = deixaCairCaixa (Jogo m (Jogador (x,y) d tf))  (a,b+1)

tirarVazio:: Mapa -> Coordenadas -> Int ->  Mapa 
tirarVazio [] _ _ = []  
tirarVazio (h:t) (x,y) b
                | y == b = substituirVazioCaixa h x 0 : tirarVazio t (x,y) (b+1)   
                | otherwise = h : tirarVazio t (x,y) (b+1)

substituirVazioCaixa::[Peca] -> Int -> Int -> [Peca]              
substituirVazioCaixa [] _ _ = undefined 
substituirVazioCaixa (h:t) x a
        | x == a = Caixa : substituirVazioCaixa t x (a+1)
        | otherwise = h :  substituirVazioCaixa t x (a+1)                 




tirarCaixa:: Mapa -> Coordenadas -> Int ->  Mapa 
tirarCaixa [] _ _ = []  
tirarCaixa (h:t) (x,y) b
                | y == b = substituirCaixaVazio h x 0 : tirarCaixa t (x,y) (b+1)   
                | otherwise = h : tirarCaixa t (x,y) (b+1) 

substituirCaixaVazio::[Peca] -> Int -> Int -> [Peca]              
substituirCaixaVazio [] _ _ = undefined 
substituirCaixaVazio (h:t) x a
        | x == a = Vazio : substituirCaixaVazio t x (a+1)
        | otherwise = h : substituirCaixaVazio t x (a+1) 
-- movimento de Trepar 

trepar:: Jogo -> Jogo 
trepar (Jogo m (Jogador (x,y) d tf))
            | d == Este && (pecaCoordenada m (x+1,y) (0,0) == Bloco || pecaCoordenada m (x+1,y) (0,0) == Caixa) && (pecaCoordenada m (x+1,y-1) (0,0) == Bloco || pecaCoordenada m (x+1,y-1) (0,0) == Caixa) = (Jogo m (Jogador (x,y) d tf)) --subir c/ obstáculo s\ caixa
            | d == Oeste && (pecaCoordenada m (x-11,y) (0,0) == Bloco || pecaCoordenada m (x-1,y) (0,0) == Caixa) && (pecaCoordenada m (x-1,y-1) (0,0) == Bloco || pecaCoordenada m (x-1,y-1) (0,0) == Caixa) = (Jogo m (Jogador (x,y) d tf)) --subir c/ obstáculo s\ caxa
            |  d == Este && tf == True && (pecaCoordenada m (x+1,y) (0,0) == Bloco || pecaCoordenada m (x+1,y) (0,0) == Caixa) && pecaCoordenada m (x+1,y-2) (0,0) == Bloco = (Jogo m (Jogador (x,y) d tf)) --subir c/ obstáculo c/ caixa 
            |  d == Oeste && tf == True && (pecaCoordenada m (x-1,y) (0,0) == Bloco || pecaCoordenada m (x-1,y) (0,0) == Caixa) && pecaCoordenada m (x-1,y-2) (0,0) == Bloco  = (Jogo m (Jogador (x,y) d tf)) --subir c/ obstáculo c/ caixa 
            | d == Este && (pecaCoordenada m (x+1,y) (0,0) == Bloco || pecaCoordenada m (x+1,y) (0,0) == Caixa) = (Jogo m (Jogador (x+1,y-1) d tf))
            | d == Oeste && (pecaCoordenada m (x-1,y) (0,0) == Bloco || pecaCoordenada m (x-1,y) (0,0) == Caixa)= (Jogo m (Jogador (x-1,y-1) d tf))
            |  d == Este = (Jogo m (Jogador (x,y) d tf))
            |  d == Oeste = (Jogo m (Jogador (x,y) d tf)) 
    

-- movimentos Laterais 

movimentoNaLateral:: Jogo -> Movimento -> Jogo 
movimentoNaLateral (Jogo m (Jogador (x,y) d tf)) mov 
                    | mov == AndarDireita && ( pecaCoordenada m (x+1,y) (0,0) == Bloco || pecaCoordenada m (x+1,y) (0,0) == Caixa) && tf == False = Jogo m (Jogador (x,y) Este tf)                                                        -- andar para Dir s\ caix com obstáculo
                    | mov == AndarEsquerda && ( pecaCoordenada m (x-1,y) (0,0) == Bloco || pecaCoordenada m (x-1,y) (0,0) == Caixa) && tf == False = Jogo m (Jogador (x,y) Oeste tf)                                                      -- andar para Esq s\ caix com obstáculo
                    | mov == AndarDireita && ( ( pecaCoordenada m (x+1,y) (0,0) == Bloco || pecaCoordenada m (x+1,y) (0,0) == Caixa) || pecaCoordenada m (x+1,y-1) (0,0) == Bloco ) && tf == True = Jogo m (Jogador (x,y) Este tf)        -- andar para Dir c/ caix com obstáculo
                    | mov == AndarEsquerda && ( ( pecaCoordenada m (x-1,y) (0,0) == Bloco || pecaCoordenada m (x-1,y) (0,0) == Caixa) || pecaCoordenada m (x+1,y-1) (0,0) == Bloco ) && tf == True = Jogo m (Jogador (x,y) Oeste tf)      -- andar para Esq c/ caix com obstáculo 
                    | mov == AndarDireita = andarNaLateral (Jogo m (Jogador (x,y) d tf)) AndarDireita  (x,y)
                    | mov == AndarEsquerda = andarNaLateral (Jogo m (Jogador (x,y) d tf)) AndarEsquerda  (x,y)

andarNaLateral:: Jogo -> Movimento -> Coordenadas -> Jogo
andarNaLateral (Jogo m (Jogador (x,y) d tf)) mov  (a,b)
                    | mov == AndarDireita && (pecaCoordenada m (a+1,b+1) (0,0) == Bloco || pecaCoordenada m (a+1,b+1) (0,0) == Caixa) = (Jogo m (Jogador (a+1,b) Este  tf))               -- anda para a direita encontra um bloco para andar Dir
                    | mov == AndarEsquerda  && (pecaCoordenada m (a-1,b+1) (0,0) == Bloco || pecaCoordenada m (a-1,b+1) (0,0) == Caixa) = (Jogo m (Jogador (a-1,b) Oeste  tf))             --anda para a esquerda encontra um bloco para andar Esq
                    | mov == AndarDireita = andarNaLateral (Jogo m (Jogador (x,y) d tf)) mov  (a,b+1)                                                                                     -- tenta encontrar um bloco ou caixa para a Dir
                    | mov == AndarEsquerda = andarNaLateral (Jogo m (Jogador (x,y) d tf)) mov  (a,b+1)                                                                                    -- -- tenta encontrar um bloco ou caixa para a ESq


pecaCoordenada:: Mapa -> Coordenadas -> (Int,Int) -> Peca
pecaCoordenada [] _ _ = undefined 
pecaCoordenada ([]:t) (x,y) (a,b) = pecaCoordenada t (x,y) (0,b+1)
pecaCoordenada ((f:rest):t) (x,y) (a,b)
                | x == a && b == y = f  
                | otherwise = pecaCoordenada  (rest:t) (x,y) (a+1,b) 
                

--  Explicação da função correrMovimentos
 
-- correrMovimentos aplica consecutivamente os comandos dados pela lista de movimentos
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos (Jogo listapecas (Jogador (x,y) dirj bool)) (h:t) 
    | (dirj == Oeste) && (h == AndarEsquerda) = correrMovimentos (Jogo listapecas (Jogador (x-1,y) Oeste bool)) t 
    | (dirj == Oeste) && (h == AndarDireita) = correrMovimentos (Jogo listapecas (Jogador (x+1,y) Este bool)) t 
    | (dirj == Este) && (h == AndarEsquerda) = correrMovimentos (Jogo listapecas (Jogador (x-1,y) Oeste bool)) t 
    | (dirj == Este) && (h == AndarDireita) = correrMovimentos (Jogo listapecas (Jogador (x+1,y) Este bool)) t 
    | (h == InterageCaixa) && (bool == True) = correrMovimentos (Jogo listapecas (Jogador (x,y) dirj False)) t
    | (h == InterageCaixa) && (bool == False) = correrMovimentos (Jogo listapecas (Jogador (x,y) dirj True)) t 




