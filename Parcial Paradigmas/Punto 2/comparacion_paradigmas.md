# Comparación de Enfoques de Programación: Imperativo vs Funcional  

## 1. Introducción  
Este documento presenta un análisis comparativo entre dos enfoques de programación aplicados a la clasificación de una lista de estudiantes según sus calificaciones y, en caso de empate, por orden alfabético.  

Se han desarrollado dos soluciones:  
- Una implementación en **Python**, basada en el paradigma **imperativo**.  
- Una implementación en **Haskell**, que sigue el paradigma **funcional**.  

Cada enfoque presenta ventajas y desventajas en términos de mutabilidad, expresividad y reutilización del código.  

---

## 2. Implementación en Python (Paradigma Imperativo)  
En este enfoque:  
✔️ Se emplean estructuras de control iterativas como bucles `for`.  
✔️ Se manipulan directamente los valores en memoria.  
✔️ Se implementa el algoritmo **Bubble Sort** para ordenar la lista paso a paso.  

🔴 **Desventajas:**  
- Puede ser menos eficiente debido a las modificaciones directas en memoria.  
- Mayor riesgo de errores por efectos secundarios.  
- Código más detallado y menos reutilizable.  

---

## 3. Implementación en Haskell (Paradigma Funcional)  
En esta implementación:  
✔️ Se usa `sortBy` y `comparing` para definir criterios de ordenamiento.  
✔️ No se modifican estructuras de datos en memoria; en su lugar, se generan nuevas versiones ordenadas.  
✔️ Se describe *qué* se quiere lograr en lugar de *cómo* hacerlo, haciendo el código más declarativo.  

🟢 **Ventajas:**  
- Código más conciso y expresivo.  
- Mayor seguridad gracias a la inmutabilidad.  
- Facilita la reutilización y el mantenimiento.  

---

## 4. Comparación de Enfoques  

| **Característica**       | **Python (Imperativo)**                         | **Haskell (Funcional)**                  |
|-------------------------|-----------------------------------------------|------------------------------------------|
| **Estilo de programación** | Paso a paso, basado en estado                  | Declarativo, basado en funciones         |
| **Mutabilidad**          | Modifica estructuras en memoria                | Inmutable, crea nuevas estructuras       |
| **Expresividad**         | Verboso, requiere control detallado            | Conciso y abstracto                      |
| **Reutilización**        | Bajo nivel de reutilización                    | Alto nivel de reutilización de funciones |
| **Manejo de errores**    | Puede introducir errores por cambios en estado | Seguridad gracias a la inmutabilidad     |

---

## 5. Conclusión  
El enfoque imperativo en **Python** es intuitivo y fácil de comprender para quienes están acostumbrados a la programación tradicional, pero puede ser propenso a errores y menos eficiente en términos de reutilización.  

Por otro lado, el enfoque funcional en **Haskell** ofrece un código más limpio y seguro, reduciendo efectos secundarios y mejorando la mantenibilidad, aunque puede tener una curva de aprendizaje más pronunciada.  

Ambos paradigmas tienen su lugar dependiendo del contexto y las necesidades del proyecto. 🚀  
