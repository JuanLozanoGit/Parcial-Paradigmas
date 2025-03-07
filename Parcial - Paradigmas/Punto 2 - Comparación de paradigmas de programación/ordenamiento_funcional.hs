import Data.List (sortBy)
import Data.Ord (comparing)

type Estudiante = (String, Int)
obtenerEstudiantes :: IO [Estudiante]
obtenerEstudiantes = do
    putStrLn "Ingrese el número de estudiantes: "
    n <- readLn
    let leerEstudiante 0 acc = return acc
        leerEstudiante m acc = do
            putStrLn "Nombre del estudiante: "
            nombre <- getLine
            putStrLn "Calificación del estudiante: "
            calificacion <- readLn
            leerEstudiante (m-1) ((nombre, calificacion) : acc)
    leerEstudiante n []
ordenarEstudiantes :: [Estudiante] -> [Estudiante]
ordenarEstudiantes = sortBy (comparing (negate . snd) <> comparing fst)

main :: IO ()
main = do
    estudiantes <- obtenerEstudiantes
    print (ordenarEstudiantes estudiantes)
