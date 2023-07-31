-- Final 17/12 2022 ParteA
data Producto = Prod {
    nombre :: String,
    precio :: Int
}

type Restriccion = Producto -> Bool

-- reestriccionesDeCompraTipo :: Restriccion
barato :: Producto -> Bool
barato = (<50) . precio

queEmpieceConX :: Char -> Producto -> Bool
queEmpieceConX caracter = (== caracter) . head . nombre 

reestriccionesDeCompra :: Char -> [Restriccion]
reestriccionesDeCompra caracter = [queEmpieceConX caracter,barato] 
-- No se exactamente cual es el concepto teorico relacionado, pero la funcion cambia ya que ahora debe recibir el Char. 

verificaRestricciones :: [Restriccion] -> [Producto] -> [Producto]
verificaRestricciones unasRestricciones = filter (cumpleRestriccion unasRestricciones) 

cumpleRestriccion :: [Restriccion] -> Producto -> Bool
cumpleRestriccion unasRestricciones unProducto = all (cumple unProducto) unasRestricciones 

cumple :: Producto -> Restriccion -> Bool
cumple unProducto unaRestriccion = unaRestriccion unProducto
-- conceptos utilizados (aunque quiza no sea lo mas funcional del mundo) :
-- Orden superior en all y filter con listas, tambien se utilizo algo mas teorico pero que a mi me gusta tener mucho en cuenta, y es la delegacion de tareas o funciones. 

-- Otro final 07/09 2022 -- Parte C
-- Para cada medicamento
type Medicamento = [String] -> [String]

cura :: String -> [String] -> [String]

cura sintoma = filter (/= sintoma) -- Se le saca de la lista la enfermedad por que la curo

bicarbonato :: Medicamento 
bicarbonato = cura "picazon"

amoxicilina :: Medicamento 
amoxicilina = cura "infeccion"

sugestion :: Medicamento
sugestion _ = []

ibuprofeno :: Int -> Medicamento
ibuprofeno miligramos enfermedades 
    | miligramos > 500 = (cura "Dolor Agudo" . cura "hinchazon") enfermedades
    | otherwise = cura "Dolor moderado" enfermedades

-- Un concepto importante utilizado es el de composicion, 

listaMedicamentos :: [Medicamento]
listaMedicamentos = [ibuprofeno 400, amoxicilina]

-- Para cada enfermedad / conjunto de sintomas
malMovimiento :: [String]
malMovimiento = ["dolor Agudo", "hinchazon"]

varicela :: [String]
varicela = repeat "Picazon"

-- mejorMedicamentoPara :: [String] -> 
mejorMedicamentoPara :: [String] -> [Medicamento] -> Medicamento
mejorMedicamentoPara sintomas = head . filter (idealPara sintomas)

-- En este caso, el orden superior es muy util ya que filter toma una lista (de medicamentos) que dada la composicion queda a la espera de, luego toma de a uno los medicamentos y evalua a cada uno para ver si son ideales para una serie de enfermedades, que lo son si lograr curarlo. 

idealPara :: [String] -> Medicamento -> Bool
idealPara sintomas medicamento = medicamento sintomas == []

-- Punto 4 
-- Para el punto a, la funcion quedara evaluando infinitamente ya que idealPara va ir tomando infinitos medicamentos y devolviendo infinitos booleanos. Filter queda iterando infinitamente y nunca se puede aplicar la funcion head, ya que nunca recibe la lista. 
-- Para esta funcion (b) va a fallar cuando analize amoxicilina, ya que quedara buscando infinitamente al string picazon, con el objetivo de sacarlo y no lo va a lograr encontrar, en cambio, en bicarbonato, lo va a encontrar y lo va a sacar. Algo similar con sugestion, pero este devuelve la lista vacia. 

-- FINAL 23/05
-- ¿Que es aplicacion parcial y dar un ejemplo?
-- La definicion es mas que obvia, pero quiza si estaria bueno dar un ejemplo

esDivisorDe :: Int -> Int -> Bool
esDivisorDe n n2 = div n n2 == 0

-- donde n2 es el valor que divide (el que yo quiero saber si es divisor) y 

esDivisorDe2 :: Int -> Int -> Bool
esDivisorDe2 78 n2 = div 78 n2 == 0 -- Se aplica parcialmente el valor n