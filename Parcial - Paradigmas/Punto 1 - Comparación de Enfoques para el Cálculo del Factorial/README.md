# Proyecto: Cálculo del Factorial en C - Enfoques Iterativo y Recursivo

## Descripción
Este proyecto implementa el cálculo del factorial de un número entero utilizando dos enfoques en lenguaje de programación C: un enfoque **iterativo** y un enfoque **recursivo**. Además, se realiza una **comparación de desempeño** entre ambas implementaciones y un **análisis del paradigma funcional** aplicado al problema.

## Objetivos
- Implementar el cálculo del factorial utilizando una versión **iterativa**.
- Implementar el cálculo del factorial utilizando una versión **recursiva**.
- Comparar ambas versiones en términos de **tiempo de ejecución** y **uso de memoria**.
- Explorar cómo el **paradigma funcional** puede mejorar la eficiencia de este cálculo.

## Contenido del Repositorio
- `factorial_iterativo.c`: Implementación iterativa del cálculo factorial en C.
- `factorial_recursivo.c`: Implementación recursiva del cálculo factorial en C.
- `informe_comparativo.md`: Análisis comparativo de tiempo de ejecución y uso de memoria entre ambas implementaciones.
- `analisis_funcional.md`: Análisis sobre cómo el paradigma funcional puede mejorar el cálculo del factorial.
- `README.md`: Información general del proyecto.

## Instrucciones de Uso
### Compilar y ejecutar la versión iterativa
1. Abre una terminal en el directorio del proyecto.
2. Compila el programa:
   ```bash
   gcc factorial_iterativo.c -o factorial_iterativo
   ```
3. Ejecuta el programa:
   ```bash
   ./factorial_iterativo
   ```
4. Ingresa un número entero no negativo cuando se te solicite.

### Compilar y ejecutar la versión recursiva
1. Abre una terminal en el directorio del proyecto.
2. Compila el programa:
   ```bash
   gcc factorial_recursivo.c -o factorial_recursivo
   ```
3. Ejecuta el programa:
   ```bash
   ./factorial_recursivo
   ```
4. Ingresa un número entero no negativo cuando se te solicite.

## Comparación de Desempeño
Se realizaron pruebas comparando el tiempo de ejecución y el uso de memoria entre ambas versiones. Puedes encontrar los detalles en el archivo `informe_comparativo.md`.

## Análisis del Paradigma Funcional
El archivo `analisis_funcional.md` proporciona una discusión sobre cómo se podría mejorar la implementación del cálculo factorial utilizando un enfoque funcional, incluyendo conceptos como **recursión de cola** y **reducción (fold)**.

## Requisitos
- Un compilador de C (como `gcc`)
- Sistema operativo Linux o Windows (con herramientas como MinGW para compilar)

## Autor
Este proyecto fue desarrollado como un ejercicio práctico de comparación de algoritmos y paradigmas de programación. Si tienes preguntas o sugerencias, no dudes en contactar.

## Licencia
Este proyecto está bajo la Licencia MIT. Puedes consultar el archivo `LICENSE` para más detalles.

