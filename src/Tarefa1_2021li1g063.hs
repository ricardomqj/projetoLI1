{- |
Module      : Tarefa1_2021li1g063
Description : Validação de um potencial mapa
Copyright   : Ricardo Miguel Queirós de Jesus <a100066@alunos.uminho.pt>;
            : Rui Pinto <a100659@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g063 where

import LI12122
import Tarefa2_2021li1g063 (maximoX, contax)


{- 
type Coordenadas = (Int, Int)
data Peca = Bloco | Porta | Caixa | Vazio deriving (Show, Eq) 
type Mapa = [(Peca)] 
-}

{̣-  | 
O objetivo desta função é implementar a função validaPotencialMapa
A função validaPotencialMapa tem como objetivo testar se uma lista de peças e as repetivas coordenadas definem corretamente um mapa.
Na 
-}

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa (h:t)
        | validaposicoes (h:t) && vereficarcaixas (h:t) && verificaVazio (h:t) && soUmaPorta (h:t) && verificaBase (h:t) = True
        | otherwise = False


--não haver posições repetidas 
{- | 
Ponto 1

A função naoRepetirPosição vai servir de função auxiliar para a função validaPosicoes.
A função naoRepetirPosicao, basicamente, com base numa peça, verifica se as coordenadas dessa peça se repetem noutra peça. Caso se repita, a função devolve False.
A função validaPosicoes com base na função anteriormente definida, naoRepetirPosicao, verifica se há mais do que uma declaração por peça para a mesma posição
-}

naoRepetirPosicao:: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Bool               -- a função vai vereficar se as coordenadas de uma peça se repetem em outra peça 
naoRepetirPosicao _ [] = True
naoRepetirPosicao (p1,(x1,y1)) ((p2,(x2,y2)):t)
        | x1 == x2 && y1==y2 = False
        | otherwise = NaoRepetirPosicao (p1,(x1,y1)) t


validaPosicoes:: [(Peca,Coordenadas)] -> Bool
validaPosicoes [] = True
validaPosicoes (h:t)
        | NaoRepetirPosicao h t  = validaPosicoes t
        | otherwise = False


{- |           
Ponto 2

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

{̣- |
Ponto 3
Neste jogo, todas as caixas devem estar posicionadas em cima de um bloco ou de outra caixa.
No ponto 3, a função "vereficarCaixas" verifica se as caixas não se encontram a "flutuar".
a função auxiliar "vereficarDeBaixo", dado uma peça, verifica se por baixo de uma caixa tem uma caixa ou um bloco, caso tenho um dos dois, a mesma devolve "True", caso contrário, devolve "False".
Então, a função "vereficarCaixas", dado, o conjunto de peças e coordenadas de um mapa, devolve um True(caso as caixas não estejam a flutuar) ou um False(caso as caixas estejam a flutuar).
A função vereficarCaixas, recebe uma lista de peças e coordenadas, e caso a primeira peça(cabeça da lista) seja uma caixa, entao se em baixo dessa mesma caixa estiver outra caixa ou um bloco, então a função vereficarCaixas vai percorrer a lista à "procura de caixas a "flutuar" caso não as encontre, vai devolver True caso encontre, devolve False 
-}

vereficarCaixas::[(Peca,Coordenadas)] -> Bool
vereficarCaixas [] = True
vereficarCaixas ((p,(x,y)):t)
           |p == Caixa    = if (vereficarDeBaixo (p,(x,y)) ((p,(x,y)):t)) == True then vereficarCaixas t else False  
           |otherwise     = vereficarCaixas t

vereficarDeBaixo:: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Bool
vereficarDeBaixo _ [] = False
vereficarDeBaixo (p1,(x1,y1)) ((p2,(x2,y2)):t)
        | p2 == Caixa && x2 == x1 && y2 == y1 + 1 = True
        | p2 == Bloco && x2 == x1 && y2 == y1 + 1 = True
        | otherwise = vereficarDeBaixo (p1,(x1,y1)) t

{- | 
Ponto 4 
O objetivo do ponto 4 é verificar se existem espaços vazios, sendo que esses espaços vazios podem estar definidos ou não.

Para verificar isso, usamos quatro funções:

- A função procurarVazio, que, caso alguma peça esteja definida como "Vazio" devolve True caso, contrário, a função percorre a lista de peças e respetivas coordenadas, até encontrar uma peça definida por "Vazio" caso não encontre, então devolverá False.
- A função tamanhoDoMapa é uma função auxiliar bastante simples, que dado umas coordenadas, multiplica o x e o y. Esta função é útil pois vai "ajudar" a calcular o "tamanho total" do mapa (ex: mapa 10 por 10, a função tamanhoDoMapa devolveria 100), usando o x e o y fornecidos pela função maiorElementoDaLista.
- A função maiorElementoDaLista calcula qual o maior x e y de cada lista;
- A função verificaVazio, a função principal do ponto 4, com base das 3 funções descritas anteriormente, verifica se há pelo menos um espaço vazio, ou seja:
        - Caso a função procurarVazio devolva True, então já se sabe que há pelo menos um espaço vazio;
        - Caso a função procurarVazio não devolva True, então ou não há espaços vazios, ou então os espaços vazios não estão definidos e, então caso a função tamanhoDoMapa devolva um número inteiro superior à "lenght" da lista com as peças e coordenadas, então é porque existem espaços vazios não definidos, caso a função tamanhoDoMapa devolva um número igual à lenght da lista com as peças e coordenadas, então é porque não existem espaços vazios logo, a função verificaVazio devolverá False.
-}

verificaVazio :: [(Peca,Coordenadas)] -> Bool
verificaVazio [] = False
verificaVazio ((p,(x,y)):t)
        |procurarVazio ((p,(x,y)):t) = True 
        |tamanhoDoMapa (maiorElementoDaLista (x,y) t) > length ((p,(x,y)):t) = True
        |otherwise = False

procurarVazio :: [(Peca,Coordenadas)] -> Bool
procurarVazio [] = False
procurarVazio ((p,(x,y)):t)
                |p == Vazio  = True
                |otherwise = procurarVazio t

tamanhoDoMapa :: (Int,Int) -> Int
tamanhoDoMapa (0,y) = y+1
tamanhoDoMapa (x,0) = x+1
tamanhoDoMapa (x,y) = (x+1) * (y+1)

maiorElementoDaLista :: (Int,Int) -> [(Peca,Coordenadas)] -> (Int,Int)
maiorElementoDaLista (x,y) [] = (x,y)
maiorElementoDaLista (xm,ym) ((p,(x,y)):t)
                | x > xm && y > ym = maiorElementoDaLista (x,y) t
                | x == xm && y > ym = maiorElementoDaLista (x,y) t
                | x > xm && y == ym = maiorElementoDaLista (x,y) t
                | otherwise = maiorElementoDaLista (xm,ym) t

{- |
Ponto 5
O objetivo do ponto 5 é verificar se a base do mapa é composta por blocos.

-}


--a função verifica a existencia de uma base contínua 

verificaBase::[(Peca,Coordenadas)] -> Bool
verificaBase [] = False
verificaBase l = continuacaoDoChao l (encontraBlocoX l (0,0)) (encontraBlocoX l (maximoX (contax l) 0,0) )

encontraBlocoX::[(Peca,Coordenadas)] -> Coordenadas  -> Coordenadas               
encontraBlocoX [] (x0,ym) = (x0,ym)
encontraBlocoX ((p,(x,y)):t) (x0,ym)
                |p == Bloco && x == x0 && y > ym = encontraBlocoX  t (x0,y)
                |otherwise = encontraBlocoX t (x0,ym)


continuacaoDoChao::[(Peca,Coordenadas)] -> Coordenadas -> Coordenadas -> Bool 
continuacaoDoChao ((p,(x,y)):t) (a,b) (xf,yf)
                | a == xf && b == yf = True
                | (Bloco,(a,b+1)) `elem` ((p,(x,y)):t)   = continuacaoDoChao ((p,(x,y)):t) (a,b+1) (xf,yf)
                | (Bloco,(a+1,b+1)) `elem` ((p,(x,y)):t) = continuacaoDoChao ((p,(x,y)):t) (a+1,b+1) (xf,yf)
                | (Bloco,(a+1,b)) `elem` ((p,(x,y)):t)   = continuacaoDoChao ((p,(x,y)):t) (a+1,b) (xf,yf)          
                | (Bloco,(a+1,b-1)) `elem` ((p,(x,y)):t) = continuacaoDoChao ((p,(x,y)):t) (a+1,b-1) (xf,yf)
                | (Bloco,(a,b-1)) `elem` ((p,(x,y)):t)   = continuacaoPorCima ((p,(x,y)):t) (a,b-1) (xf,yf)
                | otherwise = False                  


continuacaoPorCima::[(Peca,Coordenadas)] -> Coordenadas -> Coordenadas -> Bool 
continuacaoPorCima ((p,(x,y)):t) (a,b) (xf,yf) 
                | a == xf && b == yf = True
                | (Bloco,(a+1,b+1)) `elem` ((p,(x,y)):t) = continuacaoDoChao ((p,(x,y)):t) (a+1,b+1) (xf,yf)
                | (Bloco,(a+1,b)) `elem` ((p,(x,y)):t)   = continuacaoDoChao ((p,(x,y)):t) (a+1,b) (xf,yf)          
                | (Bloco,(a+1,b-1)) `elem` ((p,(x,y)):t) = continuacaoDoChao ((p,(x,y)):t) (a+1,b-1) (xf,yf)
                | (Bloco,(a,b-1)) `elem` ((p,(x,y)):t)   = continuacaoPorCima ((p,(x,y)):t) (a,b-1) (xf,yf) 
                | otherwise = False                  

   
 
    

