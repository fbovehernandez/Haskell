-- Parcial Heroes De Leyenda. !!! 
-- Punto 1
data Heroe = UnHeroe {
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto], 
    tareas :: [Tarea]
}

type Tarea = Heroe -> Heroe

data Artefacto = UnArtefacto { 
    artefacto :: String,
    rareza :: Int
}

-- Funciones utiles in the future
cambiarArtefacto :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
cambiarArtefacto f heroe = heroe {artefactos = f $ artefactos heroe}

cambiarRareza :: (Int -> Int) -> Artefacto -> Artefacto
cambiarRareza f artefacto = artefacto {rareza = f $ rareza artefacto}

cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto f heroe = heroe {epiteto = f}

cambiarReconocimiento :: (Int -> Int) -> Heroe -> Heroe 
cambiarReconocimiento f heroe = heroe {reconocimiento = f $ reconocimiento heroe}

-- Punto 2
pasaALaHistoria :: Heroe -> Heroe
pasaALaHistoria heroe
    | (>1000) . reconocimiento $ heroe = cambiarEpiteto "El Mitico" heroe -- Descubrimiento la de funcion const, no sabia ni que existia
    | (>500) . reconocimiento $ heroe = cambiarEpiteto "El Magnifico" . cambiarArtefacto (lanzaDelOlimpo : ) $ heroe
    | (>100) . reconocimiento $ heroe = cambiarEpiteto "Hoplita" . cambiarArtefacto (xiphos :) $ heroe
    | otherwise = heroe

lanzaDelOlimpo :: Artefacto 
lanzaDelOlimpo = UnArtefacto "LanzaDelOlimpo" 100 

xiphos :: Artefacto 
xiphos = UnArtefacto "Xiphos" 50
 
-- Punto 3
encontrarArtefacto :: Artefacto -> Heroe -> Heroe
encontrarArtefacto artefacto = cambiarReconocimiento(+ rareza artefacto) . cambiarArtefacto(artefacto : )

escalarElOlimpo :: Heroe -> Heroe
escalarElOlimpo =  cambiarArtefacto((relampagoDeZeus :) . filter ((>1000) . rareza) . map triplicarRareza) . cambiarReconocimiento(+500)

triplicarRareza :: Artefacto -> Artefacto
triplicarRareza artefacto = artefacto{rareza = 3* rareza artefacto}

relampagoDeZeus :: Artefacto
relampagoDeZeus = UnArtefacto "Relampago De Zeus" 500

ayudarACruzar :: Int -> Heroe -> Heroe
ayudarACruzar n = cambiarEpiteto ("Gros" ++ replicate n 'o') -- Se podria implementar con un map 

matarALaBestia :: Bestia -> Heroe -> Heroe
matarALaBestia bestia heroe 
    | debilidad bestia heroe = cambiarEpiteto ("El asesino de la" ++ nombre bestia) heroe
    | otherwise = cambiarArtefacto tail . cambiarEpiteto "El cobarde" $ heroe

data Bestia = Bestia{
    nombre :: String,
    debilidad :: Heroe -> Bool
}

-- Punto 4
heracles :: Heroe
heracles = UnHeroe "Guardian Del Olimpo" 700 [relampagoDeZeus, pistolaRara] [matarANemea]

pistolaRara :: Artefacto
pistolaRara = UnArtefacto "Pistola" 1000

-- Punto 5
nemea :: Bestia
nemea = Bestia "Leon De Nemea" ((>20) . length . epiteto)

matarANemea :: Tarea
matarANemea = matarALaBestia nemea

-- Punto 6
hacerTarea :: Tarea -> Heroe -> Heroe
hacerTarea tarea  = tarea . agregarTarea tarea 

agregarTarea :: Tarea -> Heroe -> Heroe
agregarTarea tarea heroe = heroe{tareas = tarea : tareas heroe}

-- Punto 7
compararHeroes :: Heroe -> Heroe -> (Heroe, Heroe)
compararHeroes heroe1 heroe2
    | leGana heroe1 heroe2 = (heroe1, heroe2)
    | leGana heroe2 heroe1 = (heroe2, heroe1)
    | otherwise = compararHeroes (realizarTareas heroe1 (tareas heroe2)) (realizarTareas heroe2 (tareas heroe1))

realizarTareas :: Heroe -> [Tarea] -> Heroe
realizarTareas = foldr hacerTarea 

leGana :: Heroe -> Heroe -> Bool
leGana heroe1 heroe2 = mayorReconocimiento heroe1 heroe2 || reconocimiento heroe1 == reconocimiento heroe2 && compararRarezas heroe1 heroe2

mayorReconocimiento :: Heroe -> Heroe -> Bool
mayorReconocimiento heroe1 heroe2 = reconocimiento heroe1 > reconocimiento heroe2

sumatoriaDeLasRarezas :: Heroe -> Int
sumatoriaDeLasRarezas heroe = sum . map rareza $ artefactos heroe

compararRarezas :: Heroe -> Heroe -> Bool
compararRarezas heroe1 heroe2 = sumatoriaDeLasRarezas heroe1 > sumatoriaDeLasRarezas heroe2

-- 8 
-- Queda evaluando infinitamente, ya que ambos tienen el mismo reconocimiento, la misma cantidad de artefactos,
-- y cuando quiera evaluar las tareas lo voy a poder hacer ya que el elemento base es la lista vacia, 
-- devolviendo asi el mismo heroe y entrando infinitamente en un loop por la recursividad. 

-- 9 
type Labor = [Tarea]
-- Es la funcion utilizada en el punto recursivo llamada realizarTareas. 

-- 10 
-- No, ya que el foldr nunca terminaria de evaluar las tareas de forma individual en el heroe. 