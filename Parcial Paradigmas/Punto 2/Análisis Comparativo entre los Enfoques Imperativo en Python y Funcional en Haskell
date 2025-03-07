1. Introducción
Este documento ofrece un estudio comparativo de dos métodos de programación aplicados a la resolución del problema de clasificar 
una lista de estudiantes en función de sus calificaciones y, en caso de empate, de acuerdo con el orden alfabético. 
Se han desarrollado dos soluciones: una en Python que emplea un enfoque imperativo y otra en Haskell que adopta un paradigma 
funcional.
2. Implementación en Python (Paradigma Imperativo)
- Se utilizan estructuras de control iterativas como bucles for.
- Se modifican directamente los valores en memoria.
- Se sigue un procedimiento paso a paso para el intercambio de elementos en la lista (Bubble Sort).
Este enfoque hace que el código sea intuitivo y fácil de seguir, pero puede ser menos eficiente y más propenso a errores debido a 
la manipulación directa de datos.

3. Implementación en Haskell (Paradigma Funcional)
En esta implementación:
- Se utiliza sortBy y comparing para definir criterios de ordenamiento.
- No se modifican estructuras de datos en memoria, sino que se crean nuevas versiones ordenadas.
- Se describe *qué* se desea lograr en lugar de *cómo* hacerlo.
Este enfoque produce un código más conciso y declarativo, lo que facilita la mantenibilidad y evita efectos secundarios.

4. Comparación de Enfoques
| Característica         | Python (Imperativo)                            | Haskell (Funcional)                      |
|                        |                                                |                                          |
| Estilo de programación | Paso a paso, basado en estado                  | Declarativo, basado en funciones         |
| Mutabilidad            | Modifica estructuras en memoria                | Inmutable, crea nuevas estructuras       |
| Expresividad           | Verboso, requiere control detallado            | Conciso y abstracto                      |
| Reutilización          | Bajo nivel de reutilización                    | Alto nivel de reutilización de funciones |
| Manejo de errores      | Puede introducir errores por cambios en estado | Seguridad gracias a la inmutabilidad     |

5. Conclusión
La programación imperativa en Python ofrece un enfoque claro y controlado para el proceso de ordenamiento, mientras que Haskell 
facilita una implementación más declarativa y sucinta. La selección del paradigma adecuado dependerá del contexto y de las 
necesidades específicas de cada problema: si se requiere flexibilidad y un entorno familiar, Python resulta ser una opción 
adecuada; en cambio, si se priorizan la claridad, la seguridad y la reutilización, Haskell presenta ventajas notables.
