-- data significa que estamos definindo um novo tipo de dados
data Bool = False | True
-- A parte antes de = denota o tipo, as partes depois de = são construtores de valor, eles especificam os diferentes valores que esse tipo pode ter
--Ex:
data Forma = Circulo Float Float Float | Retangulo Float Float Float Float deriving

superficie :: Forma -> Float
superficie (Circulo _ _ r) = pi * r^2
superficie (Retangulo x1 x2 y1 y2) = (abs $ x2-x1) * (abs $ y2-y1)


-- Podemos usar o comando data para definir tipos como registros, por exemplo:
data Data = Data {dia::Int,mes::Int,ano::Int}
-- Podemos definir os valores de cada campo como:
calendario = Data 31 01 2024
-- ou
caledario = Data {dia = 31, mes = 01, ano = 2024}