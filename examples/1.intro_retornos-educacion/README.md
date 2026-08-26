# Retornos a la educación (CASEN 2024)

Ejemplo usado al final de la clase 1 (Introducción): efecto de la escolaridad
sobre el ingreso del trabajo, usando datos de la encuesta CASEN 2024.

- `input/casen_2024_subset.rds` — subconjunto de CASEN 2024 ya procesado, con
  solo las variables usadas en el ejemplo (esc, ytrabajocor, edad, sexo, area,
  region). No incluye comuna ni otras variables identificatorias.
- `code/01_analysis.R` — corre la regresión y genera tablas (`output/*.tex`)
  y gráficos (`output/*.png`); `output/` se crea automáticamente al correrlo.

## Cómo correrlo

1. Instalar los paquetes de R que usa el script (una sola vez):

   ```r
   install.packages(c("here", "tidyverse", "sandwich", "haven", "modelsummary", "kableExtra"))
   ```

2. Abrir `code/01_analysis.R` en RStudio o VSCode y correrlo entero (Source /
   Run). El script usa el paquete `here` para ubicar `input/` y `output/`
   automáticamente, así que no hace falta fijar el directorio de trabajo a mano.
