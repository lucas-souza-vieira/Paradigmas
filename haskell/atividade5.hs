lista :: [Int]
lista = [0,2..10]

cabeca :: [x] -> x
cabeca (a:_) = a

comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (_:b) = 1 + comprimento b

igual :: [Int] -> [Int] -> Bool
igual [] [] = True
igual [] _ = False
igual _ [] = False
igual (a:b) (c:d) 
    | (a == c) = igual b d
    | otherwise = False


multi :: [Int] -> Int -> [Int]
multi [] x1 = []
multi (a:b) x = a*x : multi b x


ordena :: [Int] -> [Int]
ordena [] = []
ordena (a:b) = add a (ordena b)

add :: Int -> [Int] -> [Int]
add a [] = [a]
add a (b:c) | (a <= b) = a : b : c
            | otherwise = b : (add a c)

-- map: Aplica uma função a cada elemento da lista
--     map abs [-1,2,-5,3,-8] = [1,2,5,3,8]
dobrarTodos :: [Int] -> [Int]
dobrarTodos lista = map (*2) lista

-- filter: aplica uma função sobre cada elementoe retorna a lista na qual o resultado da aplicação é verdadeiro 
--     filter odd [1,2,3,4,5,6] = [1,3,5]
soPositivos :: [Int] -> [Int]
soPositivos lista = filter (>0) lista


lista_mat = [x | x <- [1,2,3]]

entre :: [Int] -> Int -> Int -> [Int]
entre lista p q = [b | b <- lista, maiorQue b, menorQue b]
    where
        maiorQue x = x > p
        menorQue x = x < q

gerarPares :: [x] -> [y] -> [(x, y)]
gerarPares l1 l2 = [(a,b) | a <- l1, b <-l2]

soma :: [Int] -> Int
soma [] = 0
soma (a:b) = a + (soma b)

media :: [Int] -> Float
media [] = 0
media lista = fromIntegral (soma lista) / fromIntegral (comprimento lista)

menor :: [Int] -> Int
menor [] = 0
menor (a:b) = 
    let (first:_) = ordena (a:b)
    in first

maior :: [Int] -> Int
maior [] = 0
maior lista =
    let ordenada = ordena lista
        ultimo = pegarUltimo ordenada
    in ultimo

pegarUltimo :: [Int] -> Int
pegarUltimo [x] = x
pegarUltimo (_:xs) = pegarUltimo xs

diferencaMaiorMenor :: [Int] -> Int
diferencaMaiorMenor [] = 0
diferencaMaiorMenor lista = (maior lista) - (menor lista)

busca :: [Int] -> Int -> Bool
busca [] x = False
busca (a:b) c = if (a == c) then True else busca b c

contarOcorrencias :: [Int] -> Int -> Int
contarOcorrencias [] _ = 0
contarOcorrencias (a:b) c = if (a == c) then 1 + contarOcorrencias b c else contarOcorrencias b c

inverte :: [t] -> [t]
inverte [] = []  -- Caso base: lista vazia, inverte nada
inverte (x:xs) = inverte xs ++ [x]  -- Chamada recursiva: inverte o resto da lista e coloca x no final

mapear :: (t -> y) -> [t] -> [y]
mapear _ [] = []  -- Caso base: se a lista estiver vazia, a lista resultante também é vazia
mapear f (x:xs) = f x : mapear f xs  -- Aplica f no primeiro elemento e chama recursivamente para o resto da lista

filtrar :: (t -> Bool) -> [t] -> [t]
filtrar f lista = [x | x <- lista, f x]

primeiros :: Int -> [t] -> [t]
primeiros _ [] = []
primeiros 0 _ = []
primeiros n (x:xs) = x : primeiros (n-1) xs

apagar :: Int -> [t] -> [t]
apagar _ [] = []
apagar 0 lista = lista
apagar n (_:xs) = apagar (n-1) xs

apagarEnquanto :: (t -> Bool) -> [t] -> [t]
apagarEnquanto _ [] = []
apagarEnquanto f (x:xs)
    | f x       = apagarEnquanto f xs
    | otherwise = x:xs


-- Concatenação: ++ [1,2,3,4]++[5,6] = [1,2,3,4,5,6]
-- Index: !! [1,2,3,4,5,6]!!3 = 4
-- Subtração: // [1,2,2,3,4,5] \\[2,3,4] = [1,2,5]
main :: IO ()
main = do
    print (busca lista 10)
 