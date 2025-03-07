#  Proyecto de Paradigmas de Programación

Este repositorio contiene tres ejercicios que abordan distintos paradigmas de programación mediante la resolución de problemas concretos en diferentes lenguajes de programación.

## 👥 Información del Grupo
- **Nombre(s):** Juan Lozano, Julio Florez, Andres Espitia, Maria Parra.
- **Códigos:** [1014990137,1030535199 , , 1072960488]
- **Asignatura:** Paradigmas de Programación
- **Periodo:** 2025-1

## 📂 Estructura del Repositorio
El repositorio se divide en tres carpetas, cada una con un ejercicio distinto:

```
📂 Ejercicio_1  (Comparación de enfoques para el factorial en C)
 ├── factorial_iterativo.c
 ├── factorial_recursivo.c
 ├── informe_comparativo.md
 ├── analisis_funcional.md
 ├── README.md

📂 Ejercicio_2  (Comparación de paradigmas con ordenamiento en Python y Haskell)
 ├── ordenamiento_imperativo.py
 ├── ordenamiento_funcional.hs
 ├── comparacion_paradigmas.md
 ├── README.md

📂 Ejercicio_3  (Gestión dinámica de memoria en C)
 ├── gestion_estudiantes.c
 ├── informe_comparativo.md
 ├── README.md
```

## 📌 Enunciados de los Ejercicios

### 📝 **Ejercicio 1: Comparación de Enfoques para el Cálculo del Factorial**
📍 **Lenguaje utilizado:** C  
**Descripción:** Se debe implementar el cálculo del factorial de un número entero mediante dos enfoques diferentes:
- **Iterativo**
- **Recursivo**

Posteriormente, se comparará el desempeño de ambas soluciones en términos de tiempo de ejecución y uso de memoria. Además, se realizará un análisis sobre cómo un paradigma funcional podría optimizar la solución.

**Entregables:**
- Implementaciones en C.
- Informe comparativo de rendimiento.
- Análisis del paradigma funcional.

### 📝 **Ejercicio 2: Comparación de Paradigmas de Programación con Ordenamiento**
📍 **Lenguajes utilizados:** Python y Haskell  
**Descripción:** Se debe ordenar una lista de estudiantes según su calificación en orden descendente. En caso de empate, se ordenará alfabéticamente por nombre.

- **Enfoque Imperativo (Python):** Implementación usando el algoritmo de ordenamiento por burbuja (Bubble Sort).
- **Enfoque Declarativo/Funcional (Haskell):** Implementación utilizando funciones de alto nivel, sin manipular explícitamente las estructuras de datos.

Se elaborará un análisis comparativo entre los dos paradigmas abordando aspectos como claridad del código, mutabilidad, eficiencia y facilidad de mantenimiento.

**Entregables:**
- Implementaciones en Python y Haskell.
- Informe comparativo de los paradigmas.

### 📝 **Ejercicio 3: Gestión Dinámica y Optimización de Memoria**
📍 **Lenguaje utilizado:** C  
**Descripción:** Se debe desarrollar un sistema de gestión de estudiantes optimizando el uso de memoria. Cada estudiante tendrá:
- Nombre y apellido (uso de cadenas dinámicas)
- Edad (uso optimizado de bits si aplica)
- ID
- Lista dinámica de calificaciones

El programa deberá permitir agregar, actualizar y eliminar registros, asegurando la correcta liberación de memoria para evitar fugas y fragmentación.

Se comparará el uso de memoria antes y después de aplicar optimizaciones.

**Entregables:**
- Implementación en C con gestión dinámica de memoria.
- Informe de comparación de desempeño.

## ▶️ **Instrucciones de Ejecución**

### 🖥️ **Ejercicio 1 (Factorial en C)**
1. Abre una terminal y navega hasta la carpeta `Ejercicio_1`.
2. Compila los programas:
   ```bash
   gcc factorial_iterativo.c -o factorial_iterativo
   gcc factorial_recursivo.c -o factorial_recursivo
   ```
3. Ejecuta cada versión:
   ```bash
   ./factorial_iterativo
   ./factorial_recursivo
   ```

### 🖥️ **Ejercicio 2 (Ordenamiento en Python y Haskell)**
**Python (Imperativo)**
1. Abre una terminal y navega hasta la carpeta `Ejercicio_2`.
2. Ejecuta el código:
   ```bash
   python3 ordenamiento_imperativo.py
   ```

**Haskell (Declarativo/Funcional)**
1. Abre una terminal y navega hasta la carpeta `Ejercicio_2`.
2. Ejecuta el programa en GHCi:
   ```bash
   ghci ordenamiento_funcional.hs
   ```

### 🖥️ **Ejercicio 3 (Gestión de memoria en C)**
1. Abre una terminal y navega hasta la carpeta `Ejercicio_3`.
2. Compila el programa:
   ```bash
   gcc gestion_estudiantes.c -o gestion_estudiantes
   ```
3. Ejecuta el programa:
   ```bash
   ./gestion_estudiantes
   ```
