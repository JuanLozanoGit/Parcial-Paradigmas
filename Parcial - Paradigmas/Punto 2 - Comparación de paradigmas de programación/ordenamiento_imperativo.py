# Función que implementa el algoritmo de ordenamiento Bubble Sort
def bubble_sort(estudiantes):
    n = len(estudiantes)  # Número total de estudiantes en la lista

    # Recorremos la lista varias veces para ordenar los elementos
    for i in range(n):
        for j in range(0, n - i - 1):
            # Comparamos primero por calificación (orden descendente)
            # Si las calificaciones son iguales, ordenamos por nombre en orden ascendente
            if (estudiantes[j][1] < estudiantes[j + 1][1]) or \
               (estudiantes[j][1] == estudiantes[j + 1][1] and estudiantes[j][0] > estudiantes[j + 1][0]):
                # Intercambiamos los elementos si están en el orden incorrecto
                estudiantes[j], estudiantes[j + 1] = estudiantes[j + 1], estudiantes[j]

    return estudiantes  # Devolvemos la lista ordenada


# Función para obtener la lista de estudiantes ingresada por el usuario
def obtener_estudiantes():
    estudiantes = []  # Lista vacía para almacenar los estudiantes

    # Pedimos el número de estudiantes
    n = int(input("Ingrese el número de estudiantes: "))

    # Recolectamos la información de cada estudiante
    for _ in range(n):
        nombre = input("Nombre del estudiante: ")  # Pedimos el nombre
        calificacion = int(input("Calificación del estudiante: "))  # Pedimos la calificación
        estudiantes.append((nombre, calificacion))  # Agregamos la tupla (nombre, calificación) a la lista

    return estudiantes  # Devolvemos la lista de estudiantes


# Obtenemos la lista de estudiantes desde la entrada del usuario
estudiantes = obtener_estudiantes()

# Aplicamos el algoritmo de ordenamiento Bubble Sort
resultado = bubble_sort(estudiantes)

# Mostramos la lista de estudiantes ordenada según los criterios establecidos
print("Lista ordenada:", resultado)
