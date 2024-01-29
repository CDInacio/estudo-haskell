data Foo = Foo {x:: Integer, str:: String}
instance Eq Foo where
        (Foo x1 str1) == (Foo x2 str2) = (x1==x2) && (str1 == str2)

-- Assim, podemos fazer:
-- Foo 1 "ola" /= Foo 1 "hello"
-- True

-- Foo 1 "comida" == Foo 1 "carro"
-- False


-- Derivação
data Foo2 = Foo2 {num:: Integer, st:: String} deriving (Eq, Show, Ord)

-- Verificar se dois circulos são iguais

data Forma = Circulo Float Float Float | Retangulo Float Float Float Float deriving (Eq, Show, Ord)

-- let circulo1 = Circulo 1.2 1.2 1.3
-- let circulo2 = Circulo 1.2 1.2 1.4
-- > circulo1 == circulo2
-- False
