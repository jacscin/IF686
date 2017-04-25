type Ponto = (Float, Float, Float)
distancia :: Ponto -> Ponto -> Float
distancia (x1, y1, z1) (x2, y2, z2) =
    sqrt((x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2)

type Nome = String
type Tamanho = Int
data Diretorio = Arquivo Nome Tamanho |
                 Pasta Nome [Diretorio]

size :: Diretorio -> (Tamanho, Int)
size (Arquivo n t) = (t, 1)
size (Pasta n ds) = foldl sumTuple (0,0) (map size ds)
                    where sumTuple = \(a,b) (c,d) -> (a+c,b+d)

data Complexo = Zezinho Float Float
instance Show Complexo where
    show (Zezinho a b) = show a ++ signal ++ show b ++ ".j"
        where signal = if(b>=0) then "+" else ""


