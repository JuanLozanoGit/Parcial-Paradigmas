#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Estructura optimizada para estudiante
typedef struct {
    char *nombre;
    char *apellido;
    uint8_t edad;
    char id[9];  // Longitud fija para evitar asignación dinámica
    float *calificaciones;
    uint8_t num_materias;
} Estudiante;

// Lista de estudiantes
Estudiante *estudiantes = NULL;
size_t num_estudiantes = 0;
size_t memoria_utilizada = 0;

void agregar_estudiante() {
    char nombre[50], apellido[50], id[9];
    uint8_t edad, num_materias;
    
    printf("Ingrese nombre: ");
    scanf("%49s", nombre);
    printf("Ingrese apellido: ");
    scanf("%49s", apellido);
    printf("Ingrese edad: ");
    scanf("%hhu", &edad);
    printf("Ingrese ID: ");
    scanf("%8s", id);
    printf("Ingrese número de materias: ");
    scanf("%hhu", &num_materias);
    
    float *calificaciones = malloc(num_materias * sizeof(float));
    for (uint8_t i = 0; i < num_materias; i++) {
        printf("Ingrese calificación %d: ", i + 1);
        scanf("%f", &calificaciones[i]);
    }
    
    estudiantes = realloc(estudiantes, (num_estudiantes + 1) * sizeof(Estudiante));
    Estudiante *nuevo = &estudiantes[num_estudiantes];
    
    nuevo->nombre = malloc(strlen(nombre) + 1);
    nuevo->apellido = malloc(strlen(apellido) + 1);
    nuevo->calificaciones = malloc(num_materias * sizeof(float));
    
    strcpy(nuevo->nombre, nombre);
    strcpy(nuevo->apellido, apellido);
    strcpy(nuevo->id, id);
    nuevo->edad = edad;
    nuevo->num_materias = num_materias;
    
    for (uint8_t i = 0; i < num_materias; i++) {
        nuevo->calificaciones[i] = calificaciones[i];
    }
    
    free(calificaciones);
    
    memoria_utilizada += (strlen(nombre) + 1) + (strlen(apellido) + 1) + (num_materias * sizeof(float)) + sizeof(Estudiante);
    num_estudiantes++;
    
    printf("Estudiante \"%s %s\" agregado correctamente. Memoria utilizada: %zu bytes.\n", nombre, apellido, memoria_utilizada);
}

void eliminar_estudiante() {
    char id[9];
    printf("Ingrese el ID del estudiante a eliminar: ");
    scanf("%8s", id);
    
    for (size_t i = 0; i < num_estudiantes; i++) {
        if (strcmp(estudiantes[i].id, id) == 0) {
            memoria_utilizada -= (strlen(estudiantes[i].nombre) + 1) + (strlen(estudiantes[i].apellido) + 1) + (estudiantes[i].num_materias * sizeof(float)) + sizeof(Estudiante);
            free(estudiantes[i].nombre);
            free(estudiantes[i].apellido);
            free(estudiantes[i].calificaciones);
            
            estudiantes[i] = estudiantes[num_estudiantes - 1];
            num_estudiantes--;
            estudiantes = realloc(estudiantes, num_estudiantes * sizeof(Estudiante));
            
            printf("Estudiante con ID %s eliminado correctamente. Memoria utilizada: %zu bytes.\n", id, memoria_utilizada);
            return;
        }
    }
    printf("Estudiante con ID %s no encontrado.\n", id);
}

void mostrar_estudiantes() {
    printf("\nLista de Estudiantes:\n");
    for (size_t i = 0; i < num_estudiantes; i++) {
        printf("ID: %s, Nombre: %s %s, Edad: %d, Materias: %d\n", 
               estudiantes[i].id, estudiantes[i].nombre, estudiantes[i].apellido, 
               estudiantes[i].edad, estudiantes[i].num_materias);
    }
}

void liberar_memoria() {
    for (size_t i = 0; i < num_estudiantes; i++) {
        free(estudiantes[i].nombre);
        free(estudiantes[i].apellido);
        free(estudiantes[i].calificaciones);
    }
    free(estudiantes);
    printf("Memoria liberada correctamente.\n");
}

int main() {
    int opcion;
    do {
        printf("\n1. Agregar estudiante\n2. Eliminar estudiante\n3. Mostrar estudiantes\n4. Salir\nSeleccione una opción: ");
        scanf("%d", &opcion);
        switch (opcion) {
            case 1: agregar_estudiante(); break;
            case 2: eliminar_estudiante(); break;
            case 3: mostrar_estudiantes(); break;
            case 4: liberar_memoria(); break;
            default: printf("Opción no válida.\n");
        }
    } while (opcion != 4);
    return 0;
}
