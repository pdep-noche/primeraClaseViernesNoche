module Library where
import PdePreludat

doble :: Number -> Number
doble numero = 2 * numero

siguiente :: Number -> Number
siguiente nro = nro + 1

calcular :: Number -> Number
calcular nro | even nro = siguiente nro
             | otherwise = doble nro


año :: (Number, Number, Number) -> Number
año (_, _, unAño) = unAño

calcular' :: (Number, Number) -> (Number, Number)
calcular' (unNum,otroNum) | even unNum && even otroNum = (doble unNum, otroNum)
                           | even unNum && odd otroNum = (doble unNum, siguiente otroNum)
                           | odd unNum && odd otroNum = (unNum, siguiente otroNum)
                           | otherwise = (unNum, otroNum)

calcular'' :: (Number, Number) -> (Number, Number)
calcular'' (unNum, otroNum) = (calcularPrimerElem unNum, calcularSegElem otroNum)

calcularPrimerElem :: Number -> Number
calcularPrimerElem numero | even numero = doble numero
                          | otherwise = numero

calcularSegElem :: Number -> Number
calcularSegElem numero | odd numero = siguiente numero
                       | otherwise = numero

and' :: Bool -> Bool -> Bool
and' unBool otroBool | unBool = otroBool
                     | otherwise = False

and'' :: Bool -> Bool ->Bool
and'' True otroBool = otroBool
and'' _  _ = False

or' :: Bool -> Bool -> Bool
or'  False False =False
or'  _ _ = True

or'' :: Bool -> Bool -> Bool
or'' True _ = True
or'' _  unBool = unBool

type Alumno =  (String, Nota, Nota, Nota)
type Nota = Number

notaMaxima :: Alumno -> Nota
notaMaxima (_, nota1, nota2, nota3) =  nota1 `max` (nota2 `max` nota3)