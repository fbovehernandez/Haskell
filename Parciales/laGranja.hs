import System.Win32 (SYSTEM_INFO(siProcessorLevel))
-- Parcial laGranja Miercoles Noche !!

data Animal = UnAnimal {
    nombre :: String,
    tipo :: String, -- r
    peso :: Int,
    edad :: Int,
    visitasMedicas :: [Consulta],
    estaEnfermo :: Bool -- r
} deriving (Eq, Show)

data Consulta = UnaConsulta {
    diasDeRecuperacion :: Int,
    cobro :: Int
} deriving (Eq, Show)

-- Tipo de consulta orientativo 
dolorEstomacal :: Consulta 
dolorEstomacal = UnaConsulta 14 110

-- Modelado de un animal
doroti :: Animal
doroti = UnAnimal "Dorothy" "Vaca" 190 24 [dolorEstomacal] True

type Granja = [Animal]

-- Funciones Utiles 
cambiarPeso :: (Int -> Int) -> Animal -> Animal
cambiarPeso f animal = animal{peso = f $ peso animal}

cambiarVisitasMedicas :: ([Consulta] -> [Consulta]) -> Animal -> Animal
cambiarVisitasMedicas f animal = animal{visitasMedicas = f $ visitasMedicas animal}

cambiarEdad :: (Int -> Int) -> Animal -> Animal
cambiarEdad f animal = animal{edad = f $ edad animal}

-- Punto 1
laPasoMal :: Animal -> Bool
laPasoMal animal = any((>30) . diasDeRecuperacion) (visitasMedicas animal)

nombreFalopa :: Animal -> Bool
nombreFalopa = (== 'i') . last . nombre

-- Punto 2
type Actividad = Animal -> Animal

engorde :: Int -> Actividad
engorde kilos = cambiarPeso(+ min (div kilos 2) 5) 

revisacion :: Consulta -> Actividad
revisacion consulta = cambiarVisitasMedicas (consulta :) . cambiarPeso (+2)

festejoCumple :: Actividad
festejoCumple = cambiarEdad (+1) . cambiarPeso(+1)

---------------- Chequeo Peso, quiza no este del todo bien
chequeoPeso :: Int -> Actividad
chequeoPeso pesoEsperado animal 
    | pesoEsperado < peso animal = cambiarEstadoDeEnfermedad animal
    | otherwise = animal

cambiarEstadoDeEnfermedad :: Animal -> Animal
cambiarEstadoDeEnfermedad animal = animal{estaEnfermo = True}

-- Punto 3
type Actividades = [Actividad]

proceso :: Actividades -> Animal -> Animal
proceso actividades animal = foldr ($) animal actividades

-- proceso [festejoCumple, engorde 7, revisacion dolorEstomacal, chequeoPeso 140] "Dorothy" -- Habria que definir a dorothy

-- Punto 4
mejora :: Animal -> Actividades -> Bool
mejora animal [] = True
mejora animal (x:xs:xss)
    = ((peso . x) animal >= (peso . xs) animal && diferenciaPositiva((peso . x) animal) ((peso . xs) animal)) && mejora animal (xs:xss)

diferenciaPositiva :: Int -> Int -> Bool
diferenciaPositiva peso1 peso2 = peso1 - peso2 <=3

-- Otra forma, la anterior creo que no esta muy bien
mejoraV2 :: Animal -> Actividades -> Bool
mejoraV2 animal [] = True
mejoraV2 animal (actividad : actividad2 : actividades)
     = diferenciaPositivaV2 (peso (actividad animal)) (peso animal) && mejoraV2 animal (actividad2 : actividades)

diferenciaPositivaV2 :: Int -> Int -> Bool
diferenciaPositivaV2 pesoNuevo pesoViejo = pesoNuevo > pesoViejo && pesoNuevo - pesoViejo <= 3

-- Punto 5
-- a
primeros3Falopa :: Granja -> Granja
primeros3Falopa  = take 3 . filter nombreFalopa

infinitosRechi :: [Animal]
infinitosRechi = repeat doroti

-- b
-- Seria posible, ya que por lazy evaluation llegara un punto, siempre y 
-- cuando la lista tenga 3 nombres falopa, en los que el take podra tomarlos y armar 
-- la nueva lista. Es importante que converga, y que esos 3 valores esten. 