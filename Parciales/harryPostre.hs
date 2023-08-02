-- Parcial Harry Postre 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
import Data.List(genericLength)

data Postre = Postre {
    peso :: Float,
    temperatura :: Float,
    sabores :: [String]
} deriving (Eq, Show)

type Hechizo = Postre -> Postre

-- Funciones utiles para los proximos puntos
cambiarPeso :: (Float -> Float) -> Postre -> Postre
cambiarPeso f postre = postre {peso = f $ peso postre}

cambiarSabores :: ([String]-> [String]) -> Postre -> Postre
cambiarSabores f postre = postre {sabores = f $ sabores postre}

cambiarTemperatura :: (Float -> Float) -> Postre -> Postre
cambiarTemperatura f postre = postre {temperatura = f $ temperatura postre}

-- Modelado de hechizos
incendio :: Hechizo
incendio = cambiarPeso (*0.95) . cambiarTemperatura (+1) 

inmobulus :: Hechizo
inmobulus = cambiarTemperatura(*0)

wingardiumLeviosa :: Hechizo
wingardiumLeviosa  = cambiarSabores ("concentrado" :) . cambiarPeso (*0.9)

diffindo :: Float -> Hechizo
diffindo porcentaje = cambiarPeso(*(porcentaje/100))

riddikulus :: String -> Hechizo
riddikulus sabor = cambiarSabores(reverse sabor :)

avadaKedavra :: Hechizo
avadaKedavra = saboresVacios . inmobulus
-- avadaKedavra postre = cambiarSabores (drop (length (sabores postre))) . inmobulus $ postre 

saboresVacios :: Postre -> Postre
saboresVacios postre = postre {sabores = []}

type Mesa = [Postre]

estanListos :: Hechizo -> [Postre] -> Bool
estanListos hechizo = all (esteListo . hechizo)

esteListo :: Postre -> Bool
esteListo postre = peso postre > 0 && not (estaCongelado postre) && tieneMasDeUnSabor postre

tieneMasDeUnSabor :: Postre -> Bool
tieneMasDeUnSabor postre = length (sabores postre) >= 1

estaCongelado :: Postre -> Bool
estaCongelado postre = temperatura postre == 0

-- Aca lo pense de una forma un tanto distinta que me hubiese restado puntos, ya que al promedio le pasaba el postre para calcualar el length del mismo y poder dividrlo 
pesoPromedio :: [Postre] -> Float
pesoPromedio = promedio . map peso . filter esteListo 

promedio :: [Float] -> Float
promedio lista = sum lista / genericLength lista 

-- Parte 2 MAGOS A

data Mago = Mago {
    hechizos :: [Hechizo], 
    horrorcruxes :: Int
} 
 
asistirAClaseDeDefensa :: Hechizo -> Postre -> Mago -> Mago
asistirAClaseDeDefensa hechizo postre = esAvadaKedavra postre hechizo . cambiarHechizo (hechizo :) 

esAvadaKedavra :: Postre -> Hechizo -> Mago -> Mago
esAvadaKedavra postre hechizo mago 
    | hechizo postre == avadaKedavra postre = cambiarHorrocrux (+1) mago
    | otherwise = mago

cambiarHorrocrux :: (Int -> Int) -> Mago -> Mago
cambiarHorrocrux f mago = mago {horrorcruxes = f $ horrorcruxes mago}

cambiarHechizo :: ([Hechizo] -> [Hechizo]) -> Mago -> Mago
cambiarHechizo f mago = mago {hechizos = f $ hechizos mago}

-- B 
-- Version Forma recursiva 
mejorHechizoDelMago :: Postre -> Mago -> Hechizo
mejorHechizoDelMago postre mago = mejorHechizo postre (hechizos mago)

mejorHechizo :: Postre -> [Hechizo] -> Hechizo
mejorHechizo postre [x] = x -- Caso base recursivo -> Tengo un solo hechizo
mejorHechizo postre (x:xs:xss) 
    | (length . sabores . x) postre > (length . sabores . xs) postre = mejorHechizo postre (x:xss)
    | otherwise = mejorHechizo postre (xs:xss)

-- Version foldl1 
mejorHechizoV2 :: Postre -> [Hechizo] -> Hechizo
mejorHechizoV2 postre = foldl1 (hechizoMasFuerte postre)

hechizoMasFuerte :: Postre -> (Hechizo -> Hechizo -> Hechizo) -- Al recibir el postre parcialmente aplicado, la funcion queda de a -> a -> a 
hechizoMasFuerte postre h1 h2 
    | (length . sabores . h1) postre > (length . sabores . h2) postre = h1
    | otherwise = h2

-- 3 a 

postresInfinitos :: Postre -> [Postre]
postresInfinitos postre = postre : postresInfinitos postre

postresInfinitosV2 :: Postre -> [Postre]
postresInfinitosV2 = repeat


