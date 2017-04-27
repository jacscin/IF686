data Expr = Literal Int
            | Soma Expr Expr
            | Subtrai Expr Expr
            | Variavel String

avaliar :: [(String, Int)] -> Expr -> Int
avaliar l (Literal x) = x
avaliar ((a,b):l) (Variavel x) | x == a = b
                               | otherwise = avaliar l (Variavel x)
avaliar l (Soma a b) = (+) (avaliar l a) (avaliar l b)
avaliar l (Subtrai a b) = (-) (avaliar l a) (avaliar l b)

--avaliar [("x",10),("y",20)] (Soma (Soma (Variavel "x") (Subtrai (Variavel "y") (Literal 33))) (Soma (Literal 22) (Variavel "x")))

type Chave = [(Char, Char)]
rot13parcial :: Chave
rot13parcial = [('a', 'n'), ('b', 'o'), ('c', 'p'), ('d', 'q'), ('e', 'r'), ('f', 's'), ('g', 't'), ('h', 'u'), ('i', 'v'), ('j', 'w'), ('k', 'x'), ('l', 'y'), ('m', 'z')]

find :: Chave -> Char -> Char
find [] x = x
find ((a,b):l) x | x == a = b
                 | otherwise = find l x

inverteChave :: Chave -> Chave
inverteChave [] = []
inverteChave ((a,b):l) = (b,a):(inverteChave l)

cipher :: Chave -> String -> String
cipher keys [] = ""
cipher keys (x:xs) = (find keys x):(cipher keys xs)

--cipher rot13parcial "hello*hello"

type FuncaoChave = (Char -> Char)
trocaSoLetraL :: FuncaoChave
trocaSoLetraL 'l' = 'b'
trocaSoLetraL c = c

cipherf :: FuncaoChave -> String -> String
cipherf f x = map f x

--cipherf trocaSoLetraL "hello*hello"
