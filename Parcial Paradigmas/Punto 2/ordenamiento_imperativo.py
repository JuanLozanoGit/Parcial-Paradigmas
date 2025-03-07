def bubble_sort(estudiantes):
    n = len(estudiantes)
    for i in range(n):
        for j in range(0, n - i - 1):
            # Ordenar por calificación descendente
            if estudiantes[j][1] < estudiantes[j + 1][1] or \
               (estudiantes[j][1] == estudiantes[j + 1][1] and estudiantes[j][0] > estudiantes[j + 1][0]):
                estudiantes[j], estudiantes[j + 1] = estudiantes[j + 1], estudiantes[j]
    return estudiantes

# Ejemplo de uso
estudiantes = [
    ("Ana", 85),
    ("Luis", 90),
    ("Carlos", 85),
    ("Sofía", 92),
    ("María", 90)
]

resultado = bubble_sort(estudiantes)
print(resultado)
