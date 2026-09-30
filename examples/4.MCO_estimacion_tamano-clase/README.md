# Estimación por MCO: tamaño de clase y notas (California Test Score Data)

Ejemplo de la clase 4 (Modelo lineal de regresión: estimación por MCO): la
regresión del puntaje promedio de examen sobre el ratio alumnos/profesor, con
errores estándar robustos, y la progresión de controles que ilustra el sesgo de
variable omitida.

## Archivos

| Archivo | Qué es |
|---|---|
| `input/caschool.dta` | "California Test Score Data Set" (Stock & Watson): 420 distritos escolares de California, año escolar 1998-1999. Dataset clásico del libro de texto (`CASchools`), de uso público. |
| `code/01_class_size_ols.R` | El script: genera todo lo de `output/`. |
| `output/desc_stats.tex` / `.csv` | Media, desvío estándar y percentiles de `str`, `el_pct` y `testscr`. |
| `output/scatter_str_testscr.png` | Dispersión de notas contra ratio alumnos/profesor. |
| `output/ols_str_testscr.png` | La misma dispersión con la recta de regresión. |
| `output/reg_model1.tex` / `.csv` | `testscr ~ str`, errores robustos (HC1). |
| `output/reg_model2.tex` / `.csv` | `testscr ~ str + el_pct`. |
| `output/reg_5models.tex` / `.csv` | Cinco modelos con controles (`el_pct`, `meal_pct`, `calw_pct`). |
| `output/ovb_check.csv` | Correlación entre `str` y `el_pct` y cambio del coeficiente de `str`. |
| `output/info_ejercicio.csv` | Medias y coeficientes que se le dan a los estudiantes en el ejercicio de efectos marginales. |

Cada tabla se guarda en `.tex` (para las diapositivas) y en `.csv` (para abrirla
en Excel o Google Sheets).

## Cómo correrlo

1. Instalar los paquetes (una sola vez):

   ```r
   install.packages(c("tidyverse", "haven", "sandwich", "modelsummary", "kableExtra"))
   ```

2. Abrir `code/01_class_size_ols.R` y **editar la línea** `dir_proyecto <- "..."`
   con la ruta a esta carpeta en tu computadora. Es lo único que hay que cambiar.

3. Correr el script entero (Source). `output/` se crea sola.

## Qué deberías ver

En la consola:

```
N distritos: 420
Coef. de str sin controles: -2.28
Coef. de str controlando por el_pct: -1.1
Correlacion entre str y el_pct: 0.188
```

El coeficiente de `str` se achica de −2.28 a −1.10 al controlar por `el_pct`:
`el_pct` tiene efecto negativo sobre las notas y correlación positiva con `str`,
así que la regresión simple sobrestima el efecto negativo del tamaño de clase.

Sin R instalado, ver [cómo correrlo en el navegador](../README.md#sin-r-instalado-correrlo-en-el-navegador).
