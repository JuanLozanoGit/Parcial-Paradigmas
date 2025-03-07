#include <stdio.h>

// Función para calcular el factorial de un número de manera iterativa
long long factorial_iterativo(int n) {
    long long resultado = 1; // Se inicia el resultado en 1
    
    for (int i = 1; i <= n; i++) { // Se multiplica desde 1 hasta n
        resultado = resultado * i;
    }
    
    return resultado; // Se devuelve el resultado final
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
        long long resultado = factorial_iterativo(numero);
        printf("El factorial de %d es: %lld\n", numero, resultado);
    }
    
    return 0;
}
