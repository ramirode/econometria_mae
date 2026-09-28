# Servicio militar e ingresos (Angrist 1998)

Ejemplo empírico que acompaña al deck **"Servicio militar e ingresos"**
(`slides/extra/Veteranos`). Aplica lo de la clase 4 (Estimación por MCO):
regresión simple, regresión múltiple, sesgo de variable omitida y su
descomposición.

Población: hombres que solicitaron ingresar a las fuerzas armadas de EE.UU.
entre 1979 y 1982 (Angrist 1998, *Econometrica*). La pregunta es cuánto aumenta
el ingreso 1988-1991 por haber servido (`dvet = 1`). Las fuerzas armadas
seleccionan a los solicitantes por su puntaje en el test AFQT y su escolaridad,
así que la diferencia simple de ingresos sobrestima el efecto.

## Simplificación

Cada fila de la base es una **celda** de solicitantes con las mismas
características, y `earnvar` es el ingreso promedio de la celda. Angrist pondera
por el tamaño de la celda; acá todas las celdas pesan lo mismo, para que las
regresiones sean `lm()` sin pesos. Las magnitudes difieren de las del artículo
(por ejemplo, la diferencia simple para blancos es 787 acá y 1,233 con pesos),
pero el signo del sesgo y la lección son los mismos.

## Archivos

| Archivo | Qué es |
|---|---|
| `input/veteran.dta` | Datos de Angrist (1998), 7,904 celdas, con las variables usadas: `earnvar`, `dvet`, `dnwhite`, `afqtgrp`, `edgrp`, `dobyy`, `transyy`, `id`. |
| `code/01_veteranos_ols.R` | El script: genera todo lo de `output/`. |
| `output/desc_stats.tex` / `.csv` | Ingreso y distribución del grupo AFQT por raza y condición de veterano. |
| `output/reg_simple.tex` / `.csv` | `earnvar ~ dvet` para todos, blancos y no blancos (errores robustos HC1). |
| `output/reg_progression.tex` / `.csv` | Efecto de `dvet` al agregar controles: AFQT, escolaridad, cohorte y año de solicitud, celda de covariables. |
| `output/progression_plot.png` | El mismo resultado en un gráfico. |
| `output/ovb_decomp.tex` / `.csv` | Descomposición del sesgo: `γ₁ = β₁ + Σ βⱼ δⱼ`. |
| `output/info_ejercicios.csv` | Media y coeficientes de los ejercicios de dummies e interacciones del deck de la clase 4. |

## Cómo correrlo

1. Instalar los paquetes (una sola vez):

   ```r
   install.packages(c("tidyverse", "haven", "sandwich", "modelsummary", "kableExtra"))
   ```

2. Abrir `code/01_veteranos_ols.R` y **editar la línea** `dir_proyecto <- "..."`
   con la ruta a esta carpeta en tu computadora. Es lo único que hay que cambiar.

3. Correr el script entero (Source). `output/` se crea sola.

## Qué deberías ver

En la consola:

```
N celdas: 7904
Efecto de dvet, regresion simple (todos): 1101.5
Blancos: simple 786.9 -> con controles 508.3
No blancos: simple 1415.8 -> con controles 1080.7
```

El efecto estimado baja al agregar controles: los veteranos tienen mayor puntaje
AFQT y el puntaje aumenta los ingresos, así que omitirlo genera un sesgo positivo.

Sin R instalado, ver [cómo correrlo en el navegador](../README.md#sin-r-instalado-correrlo-en-el-navegador).
