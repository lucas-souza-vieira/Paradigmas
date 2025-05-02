potencia :: Int -> Int -> Int
potencia x y = x^y

n_abs :: Int -> Int
n_abs x
    | x >= 0 = x
    | otherwise = -x

area_triangulo :: Float -> Float -> Float
area_triangulo x y = (x*y)/2

xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not(x && y)

media :: Float -> Float -> Float -> String
media x y z = 
    let soma_m = (x + y + z)/3
    in if soma_m >= 6.0 then
        "Aprovado"
    else
        "Reprovado"

ehTriangulo :: Float -> Float -> Float -> Bool
ehTriangulo x y z =
    if x + y > z then 
        if y + z > x then
            if z + x > y then
                True
            else 
                False
        else
            False
    else
        False
    
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x = fib(x-1) + fib(x-2)

bhaskara :: Float -> Float -> Float -> (Float, Float)
bhaskara x y z = 
    let discriminante = (y^2) - 4*x*z
    in ((-y + sqrt discriminante) / (2 * x), (-y - sqrt discriminante) / (2 * x))

distancia :: Float -> Float -> Float -> Float -> Float -> Float -> Float
distancia x1 y1 z1 x2 y2 z2 =
    sqrt ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)

maior :: Float -> Float -> Float -> Float
maior a b c
    | a >= b && a >= c = a
    | b >= a && b >= c = b
    | otherwise = c

euclides :: Int -> Int -> Int
euclides x 0 = x
euclides x y = euclides y (x `mod` y)

mdc3 :: Int -> Int -> Int -> Int
mdc3 x y z = euclides (euclides x y) z

mmc :: Int -> Int -> Int
mmc x y = (x*y) `div` euclides x y

coprimo :: Int -> Int -> Bool
coprimo x y = euclides x y == 1

totienteAux :: Int -> Int -> Int
totienteAux n 0 = 0
totienteAux n r
    | coprimo n r = 1 + totienteAux n (r - 1)
    | otherwise   = totienteAux n (r - 1)

totiente :: Int -> Int
totiente n = totienteAux n (n - 1)

ehDivisivel :: Int -> Int -> Bool
ehDivisivel x y = x `mod` y == 0

ehPrimoAux :: Int -> Int -> Bool
ehPrimoAux n 1 = True
ehPrimoAux n d
    | n `mod` d == 0 = False
    | otherwise = ehPrimoAux n (d - 1)

ehPrimo :: Int -> Bool
ehPrimo n
    | n < 2 = False
    | otherwise = ehPrimoAux n (n - 1)

calcula :: Char -> Float -> Float -> Float
calcula op x y
    | op == '+' = x + y
    | op == '-' = x - y
    | op == '*' = x * y
    | op == '/' = x / y
    | otherwise = error "Operador inválido"

goldbachAux :: Int -> Int -> Int
goldbachAux n d
    | ehPrimo d && ehPrimo (n - d) = d 
    | otherwise = goldbachAux n (d - 1)

goldbach :: Int -> Int
goldbach n
    | n `mod` 2 /= 0 || n < 4 = error "Número inválido. A Conjectura de Goldbach só se aplica a números pares maiores que 2."
    | otherwise = goldbachAux n (n `div` 2)

main :: IO()
main = do
    x1 <- readLn
    y1 <- readLn
--    z1 <- readLn
    
--    x2 <- readLn
--    y2 <- readLn
--    z2 <- readLn
    
    print(coprimo x1 y1)
    