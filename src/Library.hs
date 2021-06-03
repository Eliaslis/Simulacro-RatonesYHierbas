module Library where
import PdePreludat

data Raton = UnRaton {
    nombre :: String,
    edad :: Number,
    peso :: Number,
    enfermedades :: [String]
} deriving (Eq,Show)

cerebro = UnRaton{
    nombre = "Cerebro",
    edad = 9,
    peso = 0.2,
    enfermedades = ["brucelosis","sarampion","tuberculosis"]
}

bicenterrata = UnRaton{
    nombre = "Bicenterrata",
    edad = 256,
    peso = 0.2,
    enfermedades = []
}

huesudo = UnRaton{
    nombre = "Huesudo",
    edad = 4,
    peso = 10,
    enfermedades = ["alta obesidad","sinusitis"]
}

type Hierba = Raton -> Raton

hierbaBuena :: Hierba
hierbaBuena raton = modificarEdad sqrt raton

modificarEdad :: (Number -> Number) -> Raton -> Raton
modificarEdad funcion raton = raton {edad = funcion(edad raton) }

hierbaVerde :: String -> Hierba
hierbaVerde = eliminarEnfermedadesTerminadasCon 

eliminarEnfermedadesTerminadasCon :: String -> Raton -> Raton 
eliminarEnfermedadesTerminadasCon busqueda raton = raton {enfermedades = filter (noTerminaCon busqueda) (enfermedades raton)}

noTerminaCon :: String -> String -> Bool
noTerminaCon busqueda = not.(terminaCon busqueda)

terminaCon :: String -> String -> Bool
terminaCon busqueda enfermedad = take (length busqueda) (reverse enfermedad) == reverse(busqueda)

alcachofa :: Hierba  
alcachofa raton 
    |   peso raton > 2 = modificarPeso 0.1 raton
    |   otherwise = modificarPeso 0.05 raton

modificarPeso :: Number -> Raton -> Raton 
modificarPeso porcentaje raton = raton {peso = peso raton - (peso raton)*porcentaje}

hierbaZort :: Hierba
hierbaZort = pinky

pinky :: Raton -> Raton
pinky = modificarEdad (*0) . vaciarListaRaton 

vaciarListaRaton :: Raton -> Raton
vaciarListaRaton raton = raton{enfermedades=[]} 

hierbaDelDiablo :: Hierba
hierbaDelDiablo = noDisminuirDeCero 0.1 . eliminarEnfermedadesMenos10Letras 

eliminarEnfermedadesMenos10Letras :: Raton -> Raton 
eliminarEnfermedadesMenos10Letras raton = raton {enfermedades = tienenMas10Letras (enfermedades raton)}

tienenMas10Letras :: [String] -> [String]
tienenMas10Letras lista = filter mas10Letras lista

mas10Letras :: String -> Bool
mas10Letras palabra = length palabra >= 10

noDisminuirDeCero :: Number -> Raton -> Raton
noDisminuirDeCero porcentaje raton
    |   peso(modificarPeso porcentaje raton) >= 0 = modificarPeso porcentaje raton
    |   otherwise = raton

type Medicamento = [Hierba]

pondsAntiAge :: Medicamento
pondsAntiAge = [potenciarHierba 3 hierbaBuena,alcachofa]

consumirHierba :: Raton -> Hierba -> Raton 
consumirHierba raton hierba = hierba raton

administrarMedicamento :: Raton -> Medicamento -> Raton 
administrarMedicamento raton medicamento = foldl consumirHierba raton medicamento

reduceFatFast :: Number -> Medicamento
reduceFatFast potencia = [hierbaVerde "obesidad" , potenciarHierba potencia alcachofa]

potenciarHierba :: Number -> Hierba -> Hierba
potenciarHierba potencia hierba raton = iterate hierba raton !! potencia 

sufijosInfecciosas :: [String]
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

pdepCilina :: Medicamento
pdepCilina = map hierbaVerde sufijosInfecciosas

nro :: Number
nro = 1

cantidadIdeal :: (Number -> Bool) -> Number
cantidadIdeal condicion = head (take 1 (filter condicion [1..]))

lograEstabilizar :: Medicamento -> [Raton] -> Bool 
lograEstabilizar medicamento ratones = comunidadSinSobrepeso medicamento ratones && comunidadConMenosDeTresEnfermedades medicamento ratones

comunidadSinSobrepeso :: Medicamento -> [Raton] -> Bool
comunidadSinSobrepeso medicamento = all (==True) . map eliminaSobrepeso . administrarMedicamentoAComunidadFlipeada medicamento

comunidadConMenosDeTresEnfermedades :: Medicamento -> [Raton] -> Bool
comunidadConMenosDeTresEnfermedades medicamento = all (==True) . map menosDeTresEnfermedades. administrarMedicamentoAComunidadFlipeada medicamento

administrarMedicamentoAComunidadFlipeada :: Medicamento -> [Raton] -> [Raton]
administrarMedicamentoAComunidadFlipeada medicamento = map (flip administrarMedicamento medicamento)

eliminaSobrepeso :: Raton -> Bool
eliminaSobrepeso raton = peso raton < 1

menosDeTresEnfermedades :: Raton -> Bool
menosDeTresEnfermedades raton = length (enfermedades raton) < 3

{-


5) No se podría saber si un medicamento logra estabilizar a una comunidad infinita de ratones
ya que se deberían analizar todos los ratos y son infitos por lo cual la funcion "lograEstabilizar"
jamás terminaría de ejecutarse.

a) Verdadero, se cumplen las dos condicones de estabilizar para todos los ratones de la comunidad.

b) Falso, una de las condiciones de es que todos los ratones tengan menos de 3 enfermedades.



--EJERCICIO 6



a) Para agregar una nueva hierba, solo habría que definirla y luego definir un nuevos medicamento o
modificar alguno de los anteriores agregandole ésta nueva hierba. No es necesario modificar ninguna
funcion existente.

b)

c) El peso está definido en "Float" por lo tanto las magnitudes están a interpretacion del usuario.
No afecta al código existente

-}