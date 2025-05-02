import Control.Monad (forM_)
import qualified Data.Map as M

-- Tipo coordenada para guardar i e j
type Coordenada = (Int, Int)
-- Tipo criado para a matriz 
type Matriz a = [[a]]

-- Criação do Tipo de dado Estado para representar o estado do tabuleiro
data Estado = Estado {
    tamanho :: Int,
    valores :: Matriz Int,
    regiao :: Matriz Char,
    regioes :: M.Map Char [Coordenada]
} deriving Show

-- Verifica se está dentro da matriz e retorna o valor da celula
get :: Matriz a -> Coordenada -> Maybe a
get matriz (i, j) =
    if i >= 0 && i < length matriz && j >= 0 && j < length (head matriz)
    then Just ((matriz !! i) !! j)
    else Nothing

-- Verifica se uma atribuição é válida
movValido :: Estado -> Coordenada -> Int -> Bool
movValido est (i, j) num =
    -- Pega a região da celula atual
    let rId = (regiao est !! i) !! j
        -- Pega as celulas dessa região usando a função pronta de biblioteca map que busca pela chave o valor no map
        reg = M.findWithDefault [] rId (regioes est)
        -- Para cada celula da região, pego seu valor
        currentVals = [ valores est !! x !! y | (x, y) <- reg ]
        -- Lista com as coordenadas das células adjacentes (cima, baixo, esquerda, direita)
        adj = [ (i-1,j), (i+1,j), (i,j-1), (i,j+1) ]
        -- Verifica se o numero já não está presente naquela região 
        regiaoValida = not (num `elem` currentVals)
        -- all: recebe uma função e uma lista, e verifica se retorna true para toda lista
        -- Verifico para cada uma das adjacencias se o valor já não está presente
        validAdj = all (/= Just num) [ get (valores est) pos | pos <- adj ]
        -- verifico se 2 celulas da mesma região, uma em cima da outra, se a de cima é maior
        adjVertical =
            let acima = (i-1, j)
                abaixo = (i+1, j)
                acimaValido = case get (regiao est) acima of
                    Just r | r == rId -> maybe True (> num) (get (valores est) acima)
                    _ -> True

                abaixoValido = case get (regiao est) abaixo of
                    Just r | r == rId -> maybe True (< num) (get (valores est) abaixo)
                    _ -> True
            in acimaValido && abaixoValido
    in regiaoValida && validAdj && adjVertical

-- Atualiza a matriz
setMatriz :: Matriz a -> Coordenada -> a -> Matriz a
setMatriz matriz (i, j) val =
    -- take: função nativa que pega os primeiros elementos até o indice
    -- drop: função nativa que pega os elementos apois o indice
    take i matriz ++ [ take j row ++ [val] ++ drop (j+1) row ] ++ drop (i+1) matriz
    where row = matriz !! i

-- Função principal de backtracking para resolver o jogo
jogar :: Estado -> Maybe (Matriz Int)
jogar est = jogar' est 0 0
  where
    -- Função recursiva que tenta preencher a matriz com valores válidos
    jogar' est i j
        -- Se já percorremos todas as linhas, retorna a matriz atual
        | i == tamanho est = Just (valores est)
        -- Se já percorremos todas as colunas de uma linha, passa para a próxima linha
        | j == tamanho est = jogar' est (i+1) 0
        -- Se a célula já estiver preenchida, pula para a próxima célula
        | (valores est !! i !! j) /= 0 = jogar' est i (j+1)
        -- Caso contrário, tenta preencher a célula com um valor válido
        | otherwise =
            let rId = (regiao est !! i) !! j
                -- Obtém o número máximo de valores possíveis para a célula, de acordo com a região
                maxVal = length $ M.findWithDefault [] rId (regioes est)
                -- Gera uma lista de números de 1 até maxVal para tentar preencher a célula
                elementos = [1..maxVal]
            in for elementos
          where
            -- Função recursiva para tentar atribuir valores às células
            for [] = Nothing  
            for (x:xs)
                -- Verifico se o movimento é valido e tento atribuí-lo
                | movValido est (i, j) x =
                    let novoValor = setMatriz (valores est) (i, j) x
                        -- Crio uma nova copia do estado, com um novo atributo valores e vou para o proximo elemento
                        resultado = jogar' (est { valores = novoValor }) i (j+1)
                    in case resultado of
                        -- Se encontrar uma solução valida, retorna a nova matriz
                        Just matriz -> Just matriz
                        -- Caso contrário, tenta o próximo valor
                        Nothing -> for xs
                -- Se não for valido, tento o proximo elemento
                | otherwise = for xs


-- Leitura da entrada
lerMatriz :: Int -> IO (Matriz String)
lerMatriz n = sequence [ fmap words getLine | _ <- [1..n] ]

montarEstado :: Int -> [[String]] -> [[String]] -> Estado
montarEstado n valStr regStr =
    let valMatriz = map (map read) valStr :: Matriz Int
        regMatriz = map (map head) regStr
        -- Cria as cordenadas
        coords = [ (i,j) | i <- [0..n-1], j <- [0..n-1] ]
        -- Cria o map
        regMap = foldr
                    (\(i,j) acc ->
                        let r = (regMatriz !! i) !! j
                        in M.insertWith (++) r [(i,j)] acc)
                    M.empty
                    coords
    in Estado n valMatriz regMatriz regMap

main :: IO ()
main = do
    nLine <- getLine
    let n = read nLine
    valStrs <- lerMatriz n
    regStrs <- lerMatriz n
    let est = montarEstado n valStrs regStrs
    case jogar est of
        Just solution ->
            -- Função da biblioteca Control.Monad para imprimir o resultado
            forM_ solution $ \row -> putStrLn $ unwords (map show row)
        Nothing -> putStrLn "Sem solução."