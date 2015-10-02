## Procesamiento de datos de exportaciones

Este repo contiene varios scripts para procesar los archivos planos de exportaciones del DANE.

El orden de la ejecución es la siguiente:

1. Correr el script lecturaArchivos.R. Este script lee los archivos planos del DANE y devuelve un dataframe con los datos de cada mes
2. Correr el script Consolidacion.sh. Mueve todos los archivos procesados a una carpeta.
