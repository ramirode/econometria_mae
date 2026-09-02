# Tamaño de clase y notas (California Test Score Data)

Ejemplo de la clase 2 (Esperanza condicional y modelo lineal de regresión):
gráfico de dispersión entre el ratio alumnos/profesor y el puntaje promedio de
examen de un distrito escolar, con la recta de regresión lineal simple
superpuesta.

Es el ejemplo que ilustra el modelo de regresión como mejor predictor lineal.

## Archivos

- `input/caschool.dta` — "California Test Score Data Set" (Stock & Watson), 420
  distritos escolares de California, año escolar 1998-1999. Dataset clásico del
  libro de texto (también conocido como `CASchools`), de uso público. Variables
  usadas: `str` (student-teacher ratio) y `testscr` (puntaje promedio del
  distrito).
- `code/01_class_size.R` — genera `output/test_score_class_size.png`, la figura
  de la diapositiva "Ejemplo: Tamaño de clase y notas".

## Cómo correrlo

1. Instalar los paquetes (una sola vez):

   ```r
   install.packages(c("tidyverse", "haven"))
   ```

2. Abrir `code/01_class_size.R` y **editar la línea** `dir_proyecto <- "..."` con
   la ruta a esta carpeta en tu computadora. Es lo único que hay que cambiar.

3. Correr el script entero (Source). `output/` se crea sola.

Es el más corto de los tres ejemplos (58 líneas): sirve para verificar que tenés
R y los paquetes bien instalados antes de pasar a los otros.

## Qué deberías ver

En la consola, `N distritos: 420`. En el gráfico, la recta tiene pendiente
negativa: cursos más grandes se asocian con puntajes más bajos.

Sin R instalado, ver [cómo correrlo en el navegador](../README.md#sin-r-instalado-correrlo-en-el-navegador).
