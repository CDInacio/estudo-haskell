-- data significa que estamos definindo um novo tipo de dados
data Bool = False | True
-- A parte antes de = denota o tipo, as partes depois de = são construtores de valor, eles especificam os diferentes valores que esse tipo pode ter
--Ex:
data Forma = Circulo Float Float Float | Retangulo Float Float Float Float

superficie :: Forma -> Float
superficie (Circulo _ _ r) = Pi * r^2
superficie (Retangulo x1 x2 y1 y2) = (abs $ x2-x1) * (abs $ y2-y1)