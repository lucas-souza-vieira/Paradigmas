data Arvore = Null | No Int Arvore Arvore

minhaArvore :: Arvore
minhaArvore = No 52 (No 32 (No 12 Null Null) (No 35 Null Null)) (No 56 (No 55 Null Null) (No 64 Null Null))

somaElementos :: Arvore -> Int
somaElementos Null = 0
somaElementos (No n esq dir) = n + somaElementos esq + somaElementos dir

buscaElemento :: Arvore -> Int -> Bool
buscaElemento Null _ = False
buscaElemento (No n esq dir) x 
    | n == x = True
    | otherwise = buscaElemento esq x || buscaElemento dir x

limiteSup :: Int
limiteSup = 1000 --Define um limite superior para o maior número

minimo :: Int -> Int -> Int
minimo x y | x < y = x
           | otherwise = y

minimoElemento :: Arvore -> Int
minimoElemento Null = limiteSup 
minimoElemento (No n esq dir) = 
    minimo n (minimo (minimoElemento esq) (minimoElemento dir))

-- A: Contar ocorrencias de um elemento
ocorrenciasElemento :: Arvore -> Int -> Int
ocorrenciasElemento Null _ = 0
ocorrenciasElemento (No n esq dir) x
    | n == x    = 1 + ocorrenciasElemento esq x + ocorrenciasElemento dir x
    | otherwise = ocorrenciasElemento esq x + ocorrenciasElemento dir x

-- B: Contar elementos maiores que um valor\mmaioresQueElemento :: Arvore -> Int -> Int
maioresQueElemento Null _ = 0
maioresQueElemento (No n esq dir) x
    | n > x     = 1 + maioresQueElemento esq x + maioresQueElemento dir x
    | otherwise = maioresQueElemento esq x + maioresQueElemento dir x

-- D: Contar total de elementos na árvore
quantidade :: Arvore -> Int
quantidade Null = 0
quantidade (No _ esq dir) = 1 + quantidade esq + quantidade dir

-- C: Média dos elementos da árvore
mediaElementos :: Arvore -> Float
mediaElementos arv = fromIntegral (somaElementos arv) / fromIntegral (quantidade arv)

-- E: Retornar todos os elementos da árvore como lista
elementos :: Arvore -> [Int]
elementos Null = []
elementos (No n esq dir) = elementos esq ++ [n] ++ elementos dir

main = do
    putStrLn ("Soma dos elementos:")
    print (somaElementos minhaArvore)

    putStrLn ("Busca pelo elemento 30:")
    print (buscaElemento minhaArvore 30)

    putStrLn ("Busca pelo elemento 55:")
    print (buscaElemento minhaArvore 55)

    putStrLn ("Elemento mínimo:")
    print (minimoElemento minhaArvore)

    putStrLn ("Ocorrências do elemento 55:")
    print (ocorrenciasElemento minhaArvore 55)

    putStrLn ("Quantidade de elementos maiores que 50:")
    print (maioresQueElemento minhaArvore 50)

    putStrLn ("Quantidade total de elementos:")
    print (quantidade minhaArvore)

    putStrLn ("Média dos elementos:")
    print (mediaElementos minhaArvore)

    putStrLn ("Lista de elementos na árvore:")
    print (elementos minhaArvore)
