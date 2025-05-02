-- Polimorfismo paramétrico: tipos genéricos.
-- A função aceita duas listas de qualquer tipo e gera uma lista de pares (produto cartesiano).
gerarPares :: [t] -> [u] -> [(t, u)]
gerarPares l1 l2 = [(a, b) | a <- l1, b <- l2]

-- Polimorfismo ad-hoc: coerção implícita de tipos (ex: Int + Float).
soma :: Float -> Float -> Float
soma x y = x + y + 1
-- print (soma 21 10)

{- Classe personalizada EqCustom
   Um tipo 'a' é instância de EqCustom se suportar o operador ===.
   Por exemplo, para Integer ser instância de EqCustom, deve-se
   definir (===) :: Integer -> Integer -> Bool.
-}
class EqCustom a where
    (===) :: a -> a -> Bool

-- Integer como instância da classe EqCustom
instance EqCustom Integer where
    x === y = x == y  -- Usa o operador padrão do Prelude

{- Classe EqCustom2
   Extende a EqCustom, adicionando o operador /== (diferente),
   definido como a negação do operador ===.
-}
class EqCustom2 a where
    (===), (/==) :: a -> a -> Bool
    x /== y = not (x === y)

{- Classe OrdCustom
   Representa tipos totalmente ordenáveis.
   É uma subclasse de Eq, ou seja, depende que 'a' também seja instância de Eq.
   Fornece operadores relacionais e funções de mínimo e máximo.
-}
class (Eq a) => OrdCustom a where
    (<.), (<=.), (>=.), (>.) :: a -> a -> Bool
    maxC, minC :: a -> a -> a

{- Classe RealCustom
   Representa números reais como subclasse de Num e Ord.
   Define o método toRationalCustom que converte para número racional.
-}
class (Num a, Ord a) => RealCustom a where
    toRationalCustom :: a -> Rational

-- Classe MeuFloat com operações personalizadas
class (Num a) => MeuFloat a where
    (+++) :: a -> a -> a
    (***) :: a -> a -> a
    x *** y = x * x * y  -- implementação padrão

-- Instância de MeuFloat para Double
instance MeuFloat Double where
    x +++ y = 2 * x + y

-- Instância de MeuFloat para Integer
instance MeuFloat Integer where
    x +++ y = 10 * x + y

{- Classe MeuInt
   Define operações para tipos inteiros com funções de comparação.
-}
class (Integral x) => MeuInt x where
    -- Retorna o maior dos dois valores
    bigger   :: x -> x -> x
    
    -- Retorna o menor dos dois valores
    smaller  :: x -> x -> x

    -- Retorna se o número é par
    par      :: x -> Bool
    par x = x `mod` 2 == 0

    -- Retorna se o número é ímpar
    impar    :: x -> Bool
    impar x = not (par x)

    -- Retorna se o número é primo
    primo    :: x -> Bool
    primo x
        | x <= 1    = False
        | x == 2    = True
        | otherwise = not (any (\y -> x `mod` y == 0) [2..x-1])

    -- Máximo divisor comum
    mdc      :: x -> x -> x
    mdc a 0 = abs a
    mdc a b = mdc b (a `mod` b)

    -- Operador de proximidade: diferença ≤ 1
    (===)    :: x -> x -> Bool
    a === b = abs (a - b) <= 1

    -- Método adicional: retorna o fatorial
    fatorial :: x -> x
    fatorial 0 = 1
    fatorial n = n * fatorial (n - 1)

    -- Operador adicional: ^+^ (soma e dobra o resultado)
    (^+^) :: x -> x -> x
    a ^+^ b = 2 * (a + b)

-- Instância para Integer
instance MeuInt Integer where
    bigger a b = if a > b then a else b
    smaller a b = if a == bigger a b then b else a

-- Instância para Int
instance MeuInt Int where
    bigger a b = if a > b then a else b
    smaller a b = if a == bigger a b then b else a


-- Função principal de teste
main :: IO ()
main = do
    print (soma 21 10)
