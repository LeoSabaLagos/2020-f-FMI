module Lib where
import Text.Show.Functions

laVerdad = True

--------------------------------------------------------------------------------------------------------
-- Punto 1

--Parte a

type Recurso = String

data Pais = Pais{
    ingresoPerCapita :: Float,
    poblacionPublico :: Float,
    poblacionPrivado :: Float,
    recursosNaturales :: [Recurso],
    deuda :: Float
}deriving(Show,Eq)

-- Parte b
namibia :: Pais
namibia = Pais 4140 400000 650000 ["Mineria","Ecoturismo"] 50000000

--Paises de ejemplo
argentina :: Pais
argentina = Pais 6000 500 500 ["Turismo","Petroleo","Mineria"] 300
--------------------------------------------------------------------------------------------------------
-- Punto 2

type Estrategia = Pais -> Pais

prestarDolares :: Float -> Estrategia
prestarDolares prestamo = aumentarDeuda prestamo 

aumentarDeuda :: Float -> Pais -> Pais
aumentarDeuda prestamo pais = pais{deuda = (deuda pais) + prestamo * 1.5}

------

reducirSectorPublico :: Float -> Estrategia
reducirSectorPublico cantPuestosAReducir = (reducirIngresoPerCapita cantPuestosAReducir).(sacarPuestosPublicos cantPuestosAReducir)

sacarPuestosPublicos :: Float -> Pais -> Pais
sacarPuestosPublicos cantPuestosAReducir pais = 
    pais{poblacionPublico = (poblacionPublico pais) - cantPuestosAReducir}

reducirIngresoPerCapita :: Float -> Pais -> Pais
reducirIngresoPerCapita cantPuestosAReducir pais
    | cantPuestosAReducir > 100 = reducirIPC 20 pais
    | otherwise = reducirIPC 15 pais

reducirIPC :: Float -> Pais -> Pais
reducirIPC porcentaje pais = 
    pais{ingresoPerCapita = (ingresoPerCapita pais) - (ingresoPerCapita pais) * (porcentaje/100)}

-----

entregarRecursoNatural :: Recurso -> Estrategia
entregarRecursoNatural recursoNatural = (sacarleRecursoAlPais recursoNatural).(reducirDeuda 2000000)

reducirDeuda :: Float -> Pais -> Pais
reducirDeuda plataADescontar pais = 
    pais{deuda = (deuda pais) - plataADescontar}

sacarleRecursoAlPais :: Recurso -> Pais -> Pais
sacarleRecursoAlPais recurso pais = pais{recursosNaturales = sacarRecurso recurso pais}
    
sacarRecurso :: Recurso -> Pais -> [Recurso]
sacarRecurso recurso = (filter (/= recurso)).recursosNaturales

--

establecerBlindaje :: Estrategia
establecerBlindaje pais = ((sacarPuestosPublicos 500).(prestarDolares ((mitadDe.calcularPBI) pais))) pais
-- No se puede hacer con point free

calcularPBI :: Pais -> Float
calcularPBI pais = (ingresoPerCapita pais) * (poblacionActiva pais)

poblacionActiva :: Pais -> Float
poblacionActiva pais = poblacionPublico pais + poblacionPrivado pais

mitadDe :: Float -> Float
mitadDe = (/2.0)

------
aplicarEstrategia :: Estrategia -> Pais -> Pais
aplicarEstrategia estrategia = estrategia 

--------------------------------------------------------------------------------------------------------
-- Punto 3
type Receta = [Estrategia]

-- Parte a
receta3a = [prestarDolares 200000000,entregarRecursoNatural "Mineria"]

--Parte b

aplicarReceta :: Pais -> Receta -> Pais
aplicarReceta pais receta = foldr aplicarEstrategia pais receta

{- namibia :: Pais
namibia = Pais 4140 400000 650000 ["mineria","ecoturismo"] 50000000 -}
--------------------------------------------------------------------------------------------------------
-- Punto 4
-- Parte a

quienesZafan :: [Pais] -> [Pais]
quienesZafan = (filter ((any (=="Petroleo")).recursosNaturales)) -- <---- aplique parcialmente la lista de paises

{- quienesZafan :: [Pais] -> [Pais]
quienesZafan paises = filter tienePetroleo paises

tienePetroleo :: Pais -> Bool
tienePetroleo = (any (=="Petroleo")).recursosNaturales  -}

-- Parte b
totalDeudaAFavor :: [Pais] -> Float
totalDeudaAFavor = sum.(map deuda) -- <---- aplique parcialmente la lista de paises
-- -------------------| aplicacion parcial
--------------------------------------------------------------------------------------------------------
-- Punto 5

ordenadasDePeorAMejor :: Pais -> [Receta] -> Bool
ordenadasDePeorAMejor pais = (chequearOrden).(calcularPosiblesPBI).(aplicarleRecetasAlPais pais)
    --((chequearOrden).(calcularPosiblesPBI).(aplicarleRecetasAlPais pais)) recetas

aplicarleRecetasAlPais :: Pais -> [Receta] -> [Pais]
aplicarleRecetasAlPais pais recetas = map (aplicarReceta pais) recetas

calcularPosiblesPBI :: [Pais] -> [Float]
calcularPosiblesPBI posiblesPaises = map calcularPBI posiblesPaises

chequearOrden :: [Float] -> Bool
chequearOrden [pbi] = True
chequearOrden (pbi1 : pbi2 :colaPbi)
    | pbi1 < pbi2 = False
    | otherwise = chequearOrden ([pbi2] ++ colaPbi) 
--------------------------------------------------------------------------------------------------------
-- Punto 6
-- Parte a
{-
Se puede ejecutar, pero nunca se terminaria de evaluar por ser infinita la lista, la funcion 4a agarraria
recurso por recurso para analizar si es "Petroleo" (por el lazy evaluation)
-}

-- Parte b
{-
Se puede ejecutar y evaluar tranquilamente la suma total de la deuda a favor del FMI, ya que no se
analizan elementos infinito
-}
--------------------------------------------------------------------------------------------------------
