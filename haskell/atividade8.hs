-- Definindo a estrutura da Fila usando duas listas:
-- 'frente' guarda os elementos mais antigos,
-- 'tras' guarda os mais recentes, em ordem reversa
data Queue a = Queue [a] [a] deriving (Show)

-- Cria uma fila vazia
emptyQueue :: Queue a
emptyQueue = Queue [] []

-- Adiciona um elemento à fila (enqueue)
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue frente tras) = Queue frente (x : tras)

-- Remove o primeiro elemento da fila (dequeue)
dequeue :: Queue a -> Queue a
dequeue (Queue [] []) = Queue [] []  -- Fila vazia
dequeue (Queue (_:xs) tras) = Queue xs tras
dequeue (Queue [] tras) = dequeue (Queue (reverse tras) [])

-- Retorna o primeiro elemento da fila, se existir
first :: Queue a -> Maybe a
first (Queue [] []) = Nothing
first (Queue (x:_) _) = Just x
first (Queue [] tras) = first (Queue (reverse tras) [])

-- Função principal para demonstrar o uso da fila
main :: IO ()
main = do
    -- Criando fila vazia
    let fila0 = emptyQueue :: Queue Int

    -- Enfileirando elementos
    let fila1 = enqueue 10 fila0
    let fila2 = enqueue 20 fila1
    let fila3 = enqueue 30 fila2

    putStrLn "Fila após enfileirar 10, 20, 30:"
    print fila3

    -- Mostrando o primeiro elemento da fila
    putStrLn "Primeiro elemento da fila:"
    print (first fila3)

    -- Desenfileirando um elemento
    let fila4 = dequeue fila3
    putStrLn "Fila após desenfileirar um elemento:"
    print fila4

    -- Mostrando novo primeiro elemento
    putStrLn "Novo primeiro elemento:"
    print (first fila4)
