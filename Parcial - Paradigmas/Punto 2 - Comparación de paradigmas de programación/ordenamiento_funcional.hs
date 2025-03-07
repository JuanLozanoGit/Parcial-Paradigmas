-- Importamos funciones necesarias para ordenar la lista
import Data.List (sortBy)  -- sortBy nos permite ordenar según un criterio personalizado
import Data.Ord (comparing) -- comparing nos facilita la comparación basada en campos específicos

-- Definimos el tipo Estudiante como una tupla (Nombre, Calificación)
type Estudiante = (String, Int)

-- Función para obtener la lista de estudiantes desde la entrada del usuario
obtenerEstudiantes :: IO [Estudiante]
obtenerEstudiantes = do
    putStrLn "Ingrese el número de estudiantes: "  -- Pedimos la cantidad de estudiantes
    n <- readLn  -- Leemos la cantidad de estudiantes como un número entero

    -- Función interna para leer los datos de cada estudiante
    let leerEstudiante 0 acc = return acc  -- Caso base: cuando ya no hay más estudiantes por leer
        leerEstudiante m acc = do
            putStrLn "Nombre del estudiante: "  
            nombre <- getLine  -- Leemos el nombre del estudiante
            putStrLn "Calificación del estudiante: "
            calificacion <- readLn  -- Leemos la calificación como un número entero
            leerEstudiante (m-1) ((nombre, calificacion) : acc)  -- Llamada recursiva agregando el nuevo estudiante

    leerEstudiante n []  -- Iniciamos la lectura con una lista vacía

-- Función para ordenar la lista de estudiantes
ordenarEstudiantes :: [Estudiante] -> [Estudiante]
ordenarEstudiantes = sortBy (comparing (negate . snd) <> comparing fst)
-- Explicación:
-- 1. `comparing (negate . snd)`: Ordena por calificación en orden descendente (mayor a menor).
-- 2. `<> comparing fst`: Si las calificaciones son iguales, ordena por nombre en orden ascendente (alfabético).

-- Función principal del programa
main :: IO ()
main = do
    estudiantes <- obtenerEstudiantes  -- Obtenemos la lista de estudiantes ingresada por el usuario
    print (ordenarEstudiantes estudiantes)  -- Mostramos la lista ordenada
