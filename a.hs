import Control.Monad (forM_)
import qualified Data.Map as M

-- Tipo coordenada para guardar i e j
type Coordenada = (Int, Int)
-- Tipo
type Matriz a = [[a]]

data State = State {
    size :: Int,
    values :: Matriz Int,
    regions :: Matriz Char,
    regionCells :: M.Map Char [Coordenada]
} deriving Show

-- Acessa uma posição com segurança
get :: Matriz a -> Coordenada -> Maybe a
get matriz (i, j) =
    if i >= 0 && i < length matriz && j >= 0 && j < length (head matriz)
    then Just ((matriz !! i) !! j)
    else Nothing

-- Verifica se uma atribuição é válida
isValid :: State -> Coordenada -> Int -> Bool
isValid st (i, j) num =
    let rId = (regions st !! i) !! j
        region = M.findWithDefault [] rId (regionCells st)
        currentVals = [ values st !! x !! y | (x, y) <- region ]
        adj = [ (i-1,j), (i+1,j), (i,j-1), (i,j+1) ]
        validRegion = not (num `elem` currentVals)
        validAdj = all (/= Just num) [ get (values st) pos | pos <- adj ]
        verticalRule =
            let above = (i-1, j)
                below = (i+1, j)
                checkAbove = case get (regions st) above of
                    Just r | r == rId -> maybe True (> num) (get (values st) above)
                    _ -> True
                checkBelow = case get (regions st) below of
                    Just r | r == rId -> maybe True (< num) (get (values st) below)
                    _ -> True
            in checkAbove && checkBelow
    in validRegion && validAdj && verticalRule

-- Atualiza a matriz
setMatriz :: Matriz a -> Coordenada -> a -> Matriz a
setMatriz matriz (i, j) val =
    take i matriz ++ [ take j row ++ [val] ++ drop (j+1) row ] ++ drop (i+1) matriz
    where row = matriz !! i

-- Backtracking
solve :: State -> Maybe (Matriz Int)
solve st = solve' st 0 0
  where
    solve' st i j
        | i == size st = Just (values st)
        | j == size st = solve' st (i+1) 0
        | (values st !! i !! j) /= 0 = solve' st i (j+1)
        | otherwise =
            let rId = (regions st !! i) !! j
                maxVal = length $ M.findWithDefault [] rId (regionCells st)
                tryNums = [1..maxVal]
            in try tryNums
          where
            try [] = Nothing
            try (x:xs)
                | isValid st (i, j) x =
                    let newVals = setMatriz (values st) (i, j) x
                        result = solve' (st { values = newVals }) i (j+1)
                    in case result of
                        Just matriz -> Just matriz
                        Nothing -> try xs
                | otherwise = try xs

-- Leitura da entrada
readMatriz :: Int -> IO (Matriz String)
readMatriz n = sequence [ fmap words getLine | _ <- [1..n] ]

parseState :: Int -> [[String]] -> [[String]] -> State
parseState n valStr regStr =
    let valMatriz = map (map read) valStr :: Matriz Int
        regMatriz = map (map head) regStr
        coords = [ (i,j) | i <- [0..n-1], j <- [0..n-1] ]
        regMap = foldr
                    (\(i,j) acc ->
                        let r = (regMatriz !! i) !! j
                        in M.insertWith (++) r [(i,j)] acc)
                    M.empty
                    coords
    in State n valMatriz regMatriz regMap

main :: IO ()
main = do
    nLine <- getLine
    let n = read nLine
    valStrs <- readMatriz n
    regStrs <- readMatriz n
    let st = parseState n valStrs regStrs
    case solve st of
        Just solution ->
            forM_ solution $ \row -> putStrLn $ unwords (map show row)
        Nothing -> putStrLn "Sem solução."