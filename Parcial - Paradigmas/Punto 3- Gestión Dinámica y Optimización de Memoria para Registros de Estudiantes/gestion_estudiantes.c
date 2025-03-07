#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Definición de la estructura para almacenar la información de un estudiante
typedef struct {
    char *nombre;
    char *apellido;
    uint8_t edad;
    char id[9];  // Longitud fija para evitar asignación dinámica innecesaria
    float *calificaciones;
    uint8_t num_materias;
} Estudiante;

// Variables globales para manejar la lista de estudiantes
typedef struct {
    Estudiante *lista;
    size_t cantidad;
    size_t memoria_utilizada;
} GestionEstudiantes;

GestionEstudiantes gestion = {NULL, 0, 0};

// Función para agregar un estudiante a la lista
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
    if (!calificaciones) {
        printf("Error de asignación de memoria para calificaciones.\n");
        return;
    }
    
    for (uint8_t i = 0; i < num_materias; i++) {
        printf("Ingrese calificación %d: ", i + 1);
        scanf("%f", &calificaciones[i]);
    }
    
    // Redimensionar la lista de estudiantes
    gestion.lista = realloc(gestion.lista, (gestion.cantidad + 1) * sizeof(Estudiante));
    if (!gestion.lista) {
        printf("Error de asignación de memoria para la lista de estudiantes.\n");
        free(calificaciones);
        return;
    }
    
    // Asignar los valores al nuevo estudiante
    Estudiante *nuevo = &gestion.lista[gestion.cantidad];
    nuevo->nombre = strdup(nombre);
    nuevo->apellido = strdup(apellido);
    nuevo->calificaciones = calificaciones;
    strcpy(nuevo->id, id);
    nuevo->edad = edad;
    nuevo->num_materias = num_materias;
    
    // Actualizar el uso de memoria y la cantidad de estudiantes
    gestion.memoria_utilizada += (strlen(nombre) + 1) + (strlen(apellido) + 1) + (num_materias * sizeof(float)) + sizeof(Estudiante);
    gestion.cantidad++;
    
    printf("Estudiante \"%s %s\" agregado correctamente. Memoria utilizada: %zu bytes.\n", nombre, apellido, gestion.memoria_utilizada);
}

// Función para eliminar un estudiante por su ID
void eliminar_estudiante() {
    char id[9];
    printf("Ingrese el ID del estudiante a eliminar: ");
    scanf("%8s", id);
    
    for (size_t i = 0; i < gestion.cantidad; i++) {
        if (strcmp(gestion.lista[i].id, id) == 0) {
            // Liberar la memoria del estudiante eliminado
            gestion.memoria_utilizada -= (strlen(gestion.lista[i].nombre) + 1) + (strlen(gestion.lista[i].apellido) + 1) + (gestion.lista[i].num_materias * sizeof(float)) + sizeof(Estudiante);
            free(gestion.lista[i].nombre);
            free(gestion.lista[i].apellido);
            free(gestion.lista[i].calificaciones);
            
            // Reemplazar el estudiante eliminado con el último de la lista
            gestion.lista[i] = gestion.lista[gestion.cantidad - 1];
            gestion.cantidad--;
            gestion.lista = realloc(gestion.lista, gestion.cantidad * sizeof(Estudiante));
            
            printf("Estudiante con ID %s eliminado correctamente. Memoria utilizada: %zu bytes.\n", id, gestion.memoria_utilizada);
            return;
        }
    }
    printf("Estudiante con ID %s no encontrado.\n", id);
}

// Función para mostrar la lista de estudiantes registrados
void mostrar_estudiantes() {
    if (gestion.cantidad == 0) {
        printf("No hay estudiantes registrados.\n");
        return;
    }
    printf("\nLista de Estudiantes:\n");
    for (size_t i = 0; i < gestion.cantidad; i++) {
        printf("ID: %s, Nombre: %s %s, Edad: %d, Materias: %d\n", 
               gestion.lista[i].id, gestion.lista[i].nombre, gestion.lista[i].apellido, 
               gestion.lista[i].edad, gestion.lista[i].num_materias);
    }
}

// Función para liberar la memoria utilizada al salir del programa
void liberar_memoria() {
    for (size_t i = 0; i < gestion.cantidad; i++) {
        free(gestion.lista[i].nombre);
        free(gestion.lista[i].apellido);
        free(gestion.lista[i].calificaciones);
    }
    free(gestion.lista);
    printf("Memoria liberada correctamente.\n");
}

// Función principal del programa
int main() {
    int opcion;
    do {
        printf("\n1. Agregar estudiante\n2. Eliminar estudiante\n3. Mostrar estudiantes\n4. Salir\nSeleccione una opción: ");
        if (scanf("%d", &opcion) != 1) {
            printf("Entrada inválida. Intente de nuevo.\n");
            while (getchar() != '\n'); // Limpiar el búfer de entrada
            continue;
        }
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
