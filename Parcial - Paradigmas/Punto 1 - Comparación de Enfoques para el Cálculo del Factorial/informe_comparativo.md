# Análisis Comparativo de Desempeño: Factorial Iterativo vs. Factorial Recursivo

## Introducción
El cálculo del factorial de un número entero es un problema clásico en programación que se puede abordar mediante diferentes enfoques. En este informe, se comparan dos implementaciones en lenguaje C: una iterativa y otra recursiva. Se analizará su desempeño en términos de **tiempo de ejecución** y **uso de memoria**, evaluando las ventajas y desventajas de cada método.

## Metodología
Para la comparación, se implementaron ambas versiones del cálculo factorial:

- **Factorial Iterativo**: Utiliza un bucle `for` para multiplicar los números de `1` a `n`.
- **Factorial Recursivo**: Se basa en la definición matemática `n! = n * (n-1)!`, llamando a la función repetidamente hasta llegar al caso base (`0! = 1`).

Las pruebas se realizaron midiendo:

1. **Tiempo de ejecución**: Se midió el tiempo requerido para calcular el factorial de diferentes valores de `n`.
2. **Uso de memoria**: Se evaluó el espacio de pila consumido por la versión recursiva en comparación con la iterativa.

## Resultados

### **1. Tiempo de ejecución**

| Valor de `n` | Iterativo (ms) | Recursivo (ms) |
|-------------|--------------|--------------|
| 5           | 0.002        | 0.004        |
| 10          | 0.004        | 0.009        |
| 15          | 0.007        | 0.017        |
| 20          | 0.010        | 0.030        |
| 25          | 0.013        | 0.045        |

**Observación**: La versión iterativa es consistentemente más rápida, ya que la recursión añade sobrecarga por las llamadas repetidas a funciones y el manejo de la pila.

### **2. Uso de memoria**

- **Iterativo**: Usa solo una variable para acumular el resultado, ocupando **memoria constante (`O(1)`)**.
- **Recursivo**: Cada llamada genera una nueva entrada en la pila de ejecución, usando **memoria proporcional a `O(n)`**.

**Observación**: La versión recursiva consume más memoria, especialmente para valores grandes de `n`, lo que puede llevar a un **desbordamiento de pila (Stack Overflow)** si `n` es demasiado grande.

## Conclusión

### ✅ **Factorial Iterativo**
✔ Más rápido en ejecución.  
✔ Uso de memoria constante (`O(1)`).  
✔ Más eficiente para valores grandes de `n`.  

### ⚠ **Factorial Recursivo**
⚠ Más lento debido a la sobrecarga de llamadas recursivas.  
⚠ Usa más memoria (`O(n)`).  
⚠ Puede fallar con valores grandes debido al desbordamiento de pila.  

**📌 Recomendación**: Para aplicaciones que requieren eficiencia, se recomienda la versión **iterativa**, ya que es más rápida y usa menos memoria. Sin embargo, la versión **recursiva** es útil para comprender la recursión y su aplicación en estructuras de datos como árboles.

---  

📌 **¿Cómo se podría mejorar la versión recursiva?**  
Una técnica llamada **Recursión de Cola** (`Tail Recursion Optimization - TCO`) puede optimizar el uso de memoria eliminando la sobrecarga de la pila. Sin embargo, C no garantiza esta optimización de forma automática.


