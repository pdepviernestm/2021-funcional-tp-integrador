module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

unaPersona :: Persona
unaPersona = Persona { felicidonios = 10, sueños = 1, edad = 20, habilidades= [] }

correrTests :: IO ()
correrTests = hspec $ do
   describe "TP Integrador" $ do
     describe "coeficiente de satisfaccion" $ do
       it "si es poco feliz (tiene 50 o menos felicidonios) es los felicidonios divido 2" $ do
         coeficienteDeSatisfaccion (unaPersona { felicidonios = 10 }) `shouldBe` 5
         coeficienteDeSatisfaccion (unaPersona { felicidonios = 50}) `shouldBe` 25
       it "si es moderadamente feliz (tiene mas de 50 y menor o igual a 100 felicidonios) es la cantidad de sueños por los felicidonios" $ do
         coeficienteDeSatisfaccion (unaPersona {felicidonios = 60, sueños= 2}) `shouldBe` 120
       it "si es muy feliz (más de 100 felicidonios) es el producto entre los felicidonios y la edad" $ do
         coeficienteDeSatisfaccion (unaPersona {felicidonios = 110, edad = 20}) `shouldBe` 2200
     describe "Grado de ambicion" $ do
       it "si es muy feliz es los felicidonios multiplicado por la cantidad de sueños" $ do
         gradoDeAmbicion (unaPersona {felicidonios = 150, sueños = 4}) `shouldBe` 600
       it "si es moderadamente feliz es el producto entre la edad y la cantidad de sueños" $ do
         gradoDeAmbicion (unaPersona {edad = 20, sueños = 3, felicidonios = 70}) `shouldBe` 60
       it "si es poco feliz es el doble de la cantidad de sueños" $ do
         gradoDeAmbicion (unaPersona{felicidonios=10, sueños=3}) `shouldBe` 6
     describe "Los sueños sueños son..." $ do
       describe "recibirse de una carrera" $ do  
        it "aumenta sus felicidonios en 1000 * la longitud del nombre de la carrera, y agrega la carrera como habilidad" $ do
          recibirseDeUnaCarrera "Arquitectura" (unaPersona{felicidonios = 0, habilidades = []}) `shouldBe` unaPersona{felicidonios = 12000, habilidades = ["Arquitectura"]}
        it "si la persona ya tiene esa carrera, al recibirse queda igual" $ do
          recibirseDeUnaCarrera "Arquitectura" (unaPersona{felicidonios = 0,habilidades =["Arquitectura"]}) `shouldBe`(unaPersona{felicidonios = 0,habilidades =["Arquitectura"]})

escribime :: Expectation
escribime = implementame

