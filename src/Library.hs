module Library where
import PdePreludat

data Persona = Persona {
    felicidonios :: Number,
    sueños :: Number,
    edad :: Number,
    habilidades :: [Habilidad]
} deriving (Show, Eq)
type Habilidad = String
data Felicidad = Alta | Media | Baja deriving (Show, Eq)

nivelDeFelicidad :: Persona -> Felicidad
nivelDeFelicidad persona    
    |felicidonios persona <= 50 = Baja
    |felicidonios persona > 50 && felicidonios persona <= 100 = Media
    |felicidonios persona > 100 = Alta

muyFeliz :: Persona -> Bool
muyFeliz persona = nivelDeFelicidad persona == Alta

moderadamenteFeliz :: Persona -> Bool
moderadamenteFeliz persona = nivelDeFelicidad persona == Media

pocoFeliz :: Persona -> Bool
pocoFeliz persona = nivelDeFelicidad persona == Baja

coeficienteDeSatisfaccion :: Persona -> Number
coeficienteDeSatisfaccion persona
    | pocoFeliz persona = felicidonios persona `div` 2
    | moderadamenteFeliz persona = felicidonios persona * sueños persona
    | muyFeliz persona = felicidonios persona * edad persona

gradoDeAmbicion :: Persona -> Number
gradoDeAmbicion persona
    | moderadamenteFeliz persona = edad persona * sueños persona
    | muyFeliz persona = felicidonios persona *sueños persona
    | pocoFeliz persona = 2 * sueños persona

--Alternativas:
-- coeficienteDeSatisfaccion :: Persona -> Number
-- coeficienteDeSatisfaccion persona
--     |nivelDeFelicidad persona == Baja = felicidonios persona `div` 2
--     |nivelDeFelicidad persona == Media = felicidonios persona * sueños persona
--     |nivelDeFelicidad persona == Alta = felicidonios persona * edad persona

-- coeficienteDeSatisfaccion' :: Persona -> Number
-- coeficienteDeSatisfaccion persona =
--     coeficienteDeSatisfaccionSegunFelicidad (nivelDeFelicidad persona) persona

-- coeficienteDeSatisfaccionSegunFelicidad :: Felicidad -> Persona -> Number
-- coeficienteDeSatisfaccionSegunFelicidad Alta persona =  felicidonios persona * edad persona
-- coeficienteDeSatisfaccionSegunFelicidad Media persona = felicidonios persona * sueños persona
-- coeficienteDeSatisfaccionSegunFelicidad Baja persona = felicidonios persona `div` 2

-- coeficienteDeSatisfaccion'' :: Persona -> Number
-- coeficienteDeSatisfaccion persona = case (nivelDeFelicidad persona) of
--     Baja -> felicidonios persona `div` 2
--     Media -> felicidonios persona * sueños persona
--     Alta -> felicidonios persona * edad persona


-- c) Para los tests que hayan escrito para a y b, explicar por qué eligieron los casos de prueba que eligieron respecto de los felicidonios.
-- Es decir, si en mi grupo hiciésemos 2 tests para el punto a) y eligiéramos  una persona con 60 felicidonios para un test y una con 30 para el otro, explicar por qué elegimos 60 y 30.

-- Elegimos un valor de ejemplo que representa a cada clase de equivalencia (muy feliz, poco feliz y moderadamente feliz). También, en el primero elegimos el 50 porque es un caso borde. 


-- recibirseDeUnaCarrera:: Habilidad->Persona->Persona
-- recibirseDeUnaCarrera habilidad (Persona felicidonios sueños edad habilidades)
--     |any (==habilidad) habilidades = Persona felicidonios sueños edad habilidades
--     |otherwise = Persona (felicidonios+1000*(length habilidad)) sueños edad (habilidad:habilidades)


recibirseDeUnaCarrera:: Habilidad->Persona->Persona
recibirseDeUnaCarrera habilidad (Persona felicidonios sueños edad habilidades)
    |elem habilidad habilidades = Persona felicidonios sueños edad habilidades
    |otherwise = Persona (felicidonios+1000*(length habilidad)) sueños edad (habilidad:habilidades)
