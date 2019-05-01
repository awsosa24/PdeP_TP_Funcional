--Pdep Lunes mañana 2019 1C
--Integrantes:
--Mauro Corengia mauro_corengia@hotmail.com
--Andrew Sosa andrew.wsosa@gmail.com

--Cambios V2

--Definicion de tipos
type Truco = Auto -> Auto

type CantidadVueltas = Int
type LongitudVuelta = Float
type IntegrantesPublico = [String]
type Trampa = Carrera -> Carrera
type Participantes = [Auto]

type NivelDeNafta = Int


data Auto = Auto {
    nombre :: String,
    nivelDeNafta :: NivelDeNafta,
    velocidad :: Int,
    nombreEnamorade :: String,
    truco :: Truco
}

--V2.0 Punto 3.1
data Carrera = Carrera {
    cantidadVueltas :: CantidadVueltas,
    longitudVuelta :: LongitudVuelta,
    integrantesPublico :: IntegrantesPublico,
    trampa :: Trampa,
    participantes :: Participantes
}
--V2.0 Punto 3.1
potreroFunes = Carrera {
    cantidadVueltas = 3,
    longitudVuelta = 5.0,
    integrantesPublico = ["Ronco", "Tinch", "Dodain"],
    trampa = sacarAlPistero,
    participantes = [rochaMcQueen, biankerr, gushtav, rodra] 
} 

--Modelado de trampas --V2.0 Punto 3.1
sacarAlPistero :: Trampa
sacarAlPistero unaCarrera = unaCarrera {participantes = (primerParticipanteDescalificado.participantes) unaCarrera}

primerParticipanteDescalificado :: Participantes -> Participantes
primerParticipanteDescalificado participantes = tail participantes  
--Para probar este punto usar informacionCarrera.sacarAlPistero$ potreroFunes 


lluvia :: Trampa
lluvia unaCarrera = unaCarrera {participantes = (map (modificarVelocidadAuto 10).participantes) unaCarrera}
--tambien se puede escribir asi {participantes = map (modificarVelocidadAuto 10) (participantes unaCarrera)}

neutralizarTrucos :: Trampa
neutralizarTrucos unaCarrera = unaCarrera {participantes = (map (modificarTrucoAuto inutilidad).participantes) unaCarrera}

modificarTrucoAuto :: Truco -> Auto -> Auto
modificarTrucoAuto nuevoTruco unAuto = unAuto {truco = nuevoTruco}

inutilidad :: Truco
inutilidad unAuto = unAuto

--pocaReserva
pocaReserva :: Trampa
pocaReserva unaCarrera = unaCarrera {participantes = (pocoNivelNafta.participantes)unaCarrera}

pocoNivelNafta :: [Auto] -> [Auto]
pocoNivelNafta  = filter ((< 30).nivelDeNafta)

--naftaMenorA30 :: [NivelDeNafta] -> [NivelDeNafta]
--naftaMenorA30= filter (>30) 

--podio
podio :: Trampa
podio unaCarrera = unaCarrera {participantes = (primeros3Participantes.participantes) unaCarrera}

primeros3Participantes :: Participantes -> Participantes
primeros3Participantes (x:xs) = take 3 xs  

--Trucos V1.x
modificarVelocidadAuto :: Int -> Auto -> Auto
modificarVelocidadAuto speed unAuto = unAuto { velocidad = ((+speed).velocidad) unAuto}

deReversaRocha :: Truco
deReversaRocha  unAuto = unAuto { nivelDeNafta = ((+200).nivelDeNafta) unAuto}

deReversa :: Truco
deReversa  unAuto = unAuto { nivelDeNafta = ((div 5).velocidad) unAuto} --V2.0 Punto 3.0

impresionar :: Truco
impresionar unAuto = unAuto { velocidad = (velocidad.modificarVelocidadAuto (velocidad unAuto) ) unAuto}

nitro :: Truco
nitro unAuto = unAuto { velocidad = (velocidad.modificarVelocidadAuto 15) unAuto}

fingirAmor :: String -> Truco
fingirAmor nombreNueveEnamorade  unAuto =   unAuto { nombreEnamorade = nombreNueveEnamorade}

rochaMcQueen = Auto {
    nombre = "RochaMcQueen",
    nivelDeNafta = 300,
    velocidad = 0,
    nombreEnamorade = "Ronco",
    truco = deReversaRocha
}

biankerr = Auto {
    nombre = "Biankerr",
    nivelDeNafta = 500,
    velocidad = 20,
    nombreEnamorade = "Tinch",
    truco = impresionar
}

gushtav = Auto {
    nombre = "Gushtav",
    nivelDeNafta = 200,
    velocidad = 130,
    nombreEnamorade = "PetiLaLinda",
    truco = nitro
}

rodra = Auto {
    nombre = "Rodra",
    nivelDeNafta = 0,
    velocidad = 50,
    nombreEnamorade = "Taisa",
    truco = fingirAmor "Petra"
}

--3.2

esVocal :: Char -> Bool

esVocal 'a' = True
esVocal 'e' = True
esVocal 'i' = True
esVocal 'o' = True
esVocal 'u' = True
esVocal _ = False


cantidadDeVocales :: String -> Int
cantidadDeVocales  = length.filter esVocal


velocidadDelIncremento :: Int -> Int
velocidadDelIncremento vocales | vocales > 4 = 30
                             | vocales >= 3 = 20
                             | vocales >= 1 = 15
                             | otherwise = 0


incrementarVelocidad :: Truco
incrementarVelocidad unAuto =  
    unAuto {velocidad= (velocidad.modificarVelocidadAuto (velocidadDelIncremento.cantidadDeVocales$ nombreEnamorade unAuto) ) unAuto }
--unAuto {velocidad= (velocidad unAuto + (velocidadDelIncremento.cantidadDeVocales$ nombreEnamorade unAuto))}--Cambios V1.2

--3.3

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco auto = ((>0).nivelDeNafta) auto && ((<100).velocidad) auto 
--puedeRealizarTruco auto = nivelDeNafta auto > 0 && velocidad auto < 100



--3.4

comboLoco :: Truco
comboLoco = deReversaRocha.nitro

queTrucazo :: String -> Truco
queTrucazo nombreNueveEnamorade unAuto= (incrementarVelocidad.fingirAmor nombreNueveEnamorade) unAuto

turbo :: Truco
turbo unAuto = unAuto {nivelDeNafta = 0} {velocidad = (velocidad.modificarVelocidadAuto ((*10).nivelDeNafta$ unAuto) ) unAuto}
--turbo unAuto = unAuto {nivelDeNafta = 0} {velocidad = velocidad unAuto + ((*10).nivelDeNafta) unAuto}--Cambios V1.2


----------------------------------------------------------------------------------------------------------
-----------
----------------------------------------------------------------------------------------------------------
--Entrega 2




-----
--------------------------------
-----------------------------------------------
-----------------------------------------------------------
-----------------------------------------------
--------------------------------
-----

--Casos de Prueba

--Poner el texto en la consola para realizar los casos de pruebas

informacionAuto :: Auto -> String
informacionAuto auto = "El auto " ++ nombre auto  ++ " tiene "  ++ show (nivelDeNafta auto) ++ " litros, su velocidad es " ++ show (velocidad auto) ++ " y su enamorade es " ++ nombreEnamorade auto

informacionCarrera :: Carrera -> String
--informacionCarrera carrera = " y sus competidores son " ++ show (map nombre (participantes carrera))
informacionCarrera carrera = "La carrera "  ++ " tiene "  ++ show (cantidadVueltas carrera) ++ " vueltas, su longitud es " ++ show (longitudVuelta carrera) ++ ",sus participantes son" ++ show (integrantesPublico carrera)++" y sus competidores son " ++ show (map nombre (participantes carrera))



--3.1
--(informacionAuto.deReversaRocha) rochaMcQueen
--(informacionAuto.impresionar) biankerr
--(informacionAuto.nitro) gushtav
--(informacionAuto.truco rodra) rodra


--3.2
--(informacionAuto.incrementarVelocidad) rochaMcQueen
--(informacionAuto.incrementarVelocidad) biankerr
--(informacionAuto.incrementarVelocidad) gushtav
--(informacionAuto.incrementarVelocidad) rodra

--3.23
--puedeRealizarTruco rochaMcQueen
--puedeRealizarTruco gushtav
--puedeRealizarTruco rodra

--3.4
--(informacionAuto.comboLoco) rochaMcQueen
--(informacionAuto.queTrucazo "Murcielago") rodra
--(informacionAuto.turbo) gushtav
--(informacionAuto.turbo) rodra