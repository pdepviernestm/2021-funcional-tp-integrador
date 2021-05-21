module Library where
import PdePreludat

data Persona = Persona {
    felicidonios :: Number,
    sueños :: Number,
    edad :: Number
}

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

-- Anoten el nombre:

-- Juan
-- Tomi
-- Ciro
-- Valentina
-- Juani
-- Maxi