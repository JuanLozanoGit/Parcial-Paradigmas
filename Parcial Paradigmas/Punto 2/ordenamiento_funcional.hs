import Data.List (sortBy)
import Data.Ord (comparing)

-- Definir tipo de dato para los estudiantes
type Estudiante = (String, Int)

-- Función de ordenamiento
ordenarEstudiantes :: [Estudiante] -> [Estudiante]
ordenarEstudiantes = sortBy (\(nombreA, notaA) (nombreB, notaB) ->
    comparing (negate . snd) (nombreA, notaA) (nombreB, notaB) <> comparing fst (nombreA, notaA) (nombreB, notaB)
    )

-- Ejemplo
estudiantes :: [Estudiante]
estudiantes =
    [ ("Ana", 85)
    , ("Luis", 90)
    , ("Carlos", 85)
    , ("Sofía", 92)
    , ("María", 90)
    ]

main :: IO ()
main = print (ordenarEstudiantes estudiantes)
