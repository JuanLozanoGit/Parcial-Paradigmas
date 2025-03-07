
def bubble_sort(estudiantes):
    n = len(estudiantes)
    for i in range(n):
        for j in range(0, n - i - 1):
            if (estudiantes[j][1] < estudiantes[j + 1][1]) or \
               (estudiantes[j][1] == estudiantes[j + 1][1] and estudiantes[j][0] > estudiantes[j + 1][0]):
                estudiantes[j], estudiantes[j + 1] = estudiantes[j + 1], estudiantes[j]
    return estudiantes
def obtener_estudiantes():
    estudiantes = []
    n = int(input("Ingrese el número de estudiantes: "))
    for _ in range(n):
        nombre = input("Nombre del estudiante: ")
        calificacion = int(input("Calificación del estudiante: "))
        estudiantes.append((nombre, calificacion))
    return estudiantes
estudiantes = obtener_estudiantes()
resultado = bubble_sort(estudiantes)
print("Lista ordenada:", resultado)
