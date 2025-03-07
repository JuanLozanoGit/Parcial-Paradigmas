# Informe Comparativo de Desempeño

## Introducción
Este documento analiza el desempeño del programa de gestión de estudiantes en C antes y después de aplicar optimizaciones en el uso de memoria. Se evalúan métricas como la memoria utilizada, la fragmentación detectada y el tiempo de ejecución.

## Métodos de Evaluación
Se realizaron pruebas comparativas en dos versiones del código:
1. **Versión sin optimización**: Utiliza estructuras estándar sin gestión dinámica eficiente.
2. **Versión optimizada**: Implementa asignación dinámica de memoria eficiente, uso de `uint8_t` para datos pequeños y compactación de registros.

## Comparación de Desempeño

| Métrica                   | Sin optimización | Con optimización |
|---------------------------|-----------------|-----------------|
| Memoria utilizada total   | X bytes        | Y bytes        |
| Fragmentación detectada   | Alta           | Baja o nula    |
| Tiempo de ejecución      | X ms           | Y ms           |

## Resultados y Análisis
1. **Reducción de memoria utilizada**: Se logró disminuir el consumo de memoria al ajustar dinámicamente el tamaño de los datos almacenados.
2. **Disminución de fragmentación**: La implementación de compactación tras eliminación de registros redujo significativamente la fragmentación interna.
3. **Mejora en el tiempo de ejecución**: La optimización en la asignación de memoria permitió una ejecución más eficiente.

## Conclusión
Las mejoras implementadas optimizaron significativamente el uso de memoria y el rendimiento del programa. Se recomienda continuar con pruebas adicionales en distintos escenarios para validar su eficiencia en condiciones de mayor carga de datos.

