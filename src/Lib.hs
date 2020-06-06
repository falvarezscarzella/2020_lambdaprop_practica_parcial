module Lib where
import Text.Show.Functions

laVerdad = True

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Int,
  superficie :: Int,
  precio :: Int,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a] 
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio . filter (not . criterio x)) xs ++ [x] ++ (ordenarSegun criterio . filter (criterio x)) xs

between :: (Ord a) => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor = valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta"]

depto1 =  Depto 3 80 7500 "Palermo"

--Punto 1

--a)
mayorEntreDosF:: (Ord b) => (a -> b) -> a -> a -> Bool
mayorEntreDosF funcion valor1 valor2 = funcion valor1 > funcion valor2

menorEntreDosF :: (Ord b) => (a -> b) -> a -> a -> Bool
menorEntreDosF funcion valor1 = not . mayorEntreDosF funcion valor1

--b)
-- >ordenarSegun (mayorEntreDosF length) ["Hola", "Ricardo", "X"]
-- ["Ricardo", "Hola", "X"]

--Punto 2 
--a)
ubicadoEn :: [Barrio] -> Requisito
ubicadoEn listaDeBarrios depto = any (barrio depto ==) listaDeBarrios

--b)
cumpleRango :: (Num a, Ord a) => (Depto -> a) -> a -> a -> Requisito
cumpleRango funcion valorInf valorSup = between valorInf valorSup . funcion

--Punto 3
--a)
type CriterioOrdDeptos = (Depto -> Depto -> Bool)

cumpleBusqueda ::  Depto -> Busqueda -> Bool
cumpleBusqueda depto = all (==True) . map (aplicarRequisitos depto)

aplicarRequisitos :: Depto -> Requisito -> Bool
aplicarRequisitos depto requisito = requisito depto

 --b)
buscar :: Busqueda -> CriterioOrdDeptos -> [Depto] -> [Depto]
buscar requisitosBusqueda criterio listaDeptos = (ordenarSegun criterio . filter (flip cumpleBusqueda requisitosBusqueda)) listaDeptos

ejemploBusqueda = [ubicadoEn ["Palermo", "Recoleta"], cumpleRango ambientes 1 2, (<6000).precio]

--c)

--Lib>buscar ejemploBusqueda (mayorEntreDosF superficie) deptosDeEjemplo
--[Depto {ambientes = 2, superficie = 50, precio = 5000, barrio = "Palermo"}
--,Depto {ambientes = 1, superficie = 45, precio = 5500, barrio = "Recoleta"}]

--Punto 4

mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas depto = map mail . filter (estaInteresada depto) 

estaInteresada :: Depto -> Persona -> Bool
estaInteresada depto = any (cumpleBusqueda depto) . busquedas


