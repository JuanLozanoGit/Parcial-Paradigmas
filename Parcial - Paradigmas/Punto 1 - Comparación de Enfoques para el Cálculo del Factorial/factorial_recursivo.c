#include <stdio.h>

// Función recursiva para calcular el factorial de un número
long long factorial_recursivo(int n) {
    if (n == 0 || n == 1) { // Caso base: el factorial de 0 y 1 es 1
        return 1;
    }
    return n * factorial_recursivo(n - 1); // Llamada recursiva
}

int main() {
    int numero;
    
    // Pedir al usuario un número
    printf("Ingrese un número entero positivo: ");
    scanf("%d", &numero);
    
    // Verificar si el número es negativo
    if (numero < 0) {
        printf("No se puede calcular el factorial de un número negativo.\n");
    } else {
        // Llamar a la función y mostrar el resultado
        long long resultado = factorial_recursivo(numero);
        printf("El factorial de %d es: %lld\n", numero, resultado);
    }
    
    return 0;
}
