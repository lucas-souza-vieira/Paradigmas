alunos :: [(Int, String, Float)]
alunos = [(1, "Ana", 3.4), (2, "Bob", 6.7), (3, "Tom", 7.6)]

getNome :: (Int, String, Float) -> String
getNome (a, b, c) = b

getNota :: (Int, String, Float) -> Float
getNota (a, b, c) = c

aprovado :: (Int, String, Float) -> Bool
aprovado (a, b, c) = c >= 6

aprovados :: [(Int, String, Float)] -> [String]
aprovados lista = map getNome (filter aprovado lista)


aprovados2 :: [(Int, String, Float)] -> [String]
aprovados2 lista = [getNome aluno | aluno <- lista, getNota aluno >= 6]

getPrimeiroAluno :: [(Int, String, Float)] -> (Int, String, Float)
getPrimeiroAluno (a:_) = a


listaNome :: [(Int, String, Float)] -> [String]
listaNome alunos = map getNome alunos

gerarPares :: [String] -> [String] -> [(String,String)] 
gerarPares l1 l2 = [(a,b) | a <- l1, b <- l2, a /= b]

main = do
    print (gerarPares (listaNome alunos) (listaNome alunos))