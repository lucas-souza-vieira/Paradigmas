type Nome = String
type Idade = Int
type Linguagem = String
type Pessoa = (Nome, Idade, Linguagem)


pessoa :: Int -> Pessoa
pessoa 1 = ("Bob", 25, "Haskell")
pessoa 2 = ("Tom", 22, "LISP")

getNome :: Pessoa -> Nome
getNome (n, _, _) = n

data Forma = Circulo Float | Retangulo Float Float

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = b * a

minhaForma :: Forma
minhaForma = (Retangulo 4 6)


data Arvore = Null | No Int Arvore Arvore
minhaArvore :: Arvore
minhaArvore = No 5 (No 3 Null Null) (No 8 (No 7 Null Null) (No 9 Null Null))

somaElementos :: Arvore -> Int
somaElementos Null = 0
somaElementos (No n esq dir) = n + (somaElementos esq) + (somaElementos dir)

buscaElemento :: Arvore -> Int -> Bool
buscaElemento Null _ = False
buscaElemento (No n esq dir) x
    | (n == x) = True
    | otherwise = (buscaElemento esq x) || (buscaElemento dir x)


limiteSup :: Int
limiteSup = 1000 --Define um limite superior para o maior número

minimo :: Int -> Int -> Int
minimo x y | (x < y) = x
    | otherwise = y

minimoElemento :: Arvore -> Int
minimoElemento Null = limiteSup
minimoElemento (No n esq dir) = minimo n (minimo (minimoElemento esq) (minimoElemento dir))


type NomeAluno = String
type Disciplina = String
type Nota = Int
type Notas = (Nota, Nota, Nota)
type Aluno = (NomeAluno, Disciplina, Notas)


aluno :: Int -> Aluno
aluno 1 = ("Lucas", "Mat", (10, 9, 8))
aluno 2 = ("Nicoly", "Bio", (10, 10, 10))

getAluno :: Aluno -> NomeAluno
getAluno (n, _, _) = n

getnotas :: Aluno -> Notas
getnotas (_, _, n) = n

calcMedia :: Notas -> Int
calcMedia (x, y, z) = (x + y + z) `div` 3

getMedia :: Int -> Int
getMedia x = calcMedia (getnotas (aluno x))

mediaTurmaAux :: Int -> Int -> Int -> Int
mediaTurmaAux atual totalId somaMedias
  | atual > totalId = somaMedias `div` totalId
  | otherwise       = mediaTurmaAux (atual + 1) totalId (somaMedias + getMedia atual)

mediaTurma :: Int
mediaTurma = mediaTurmaAux 1 2 0  


main = do 
    print (mediaTurma)