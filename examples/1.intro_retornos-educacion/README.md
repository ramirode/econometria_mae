# Retornos a la educación (CASEN 2024)

Ejemplo usado al final de la clase 1 (Introducción): efecto de la escolaridad
sobre el ingreso del trabajo, usando datos de la encuesta CASEN 2024.

El script estima una regresión de Mincer con una progresión de controles
(esc → +experiencia → +mujer → +rural → +región) para mostrar cómo se mueve el
coeficiente de escolaridad ante el sesgo por variable omitida.

Población: personas entre 30 y 65 años que trabajan.

## Archivos

- `input/casen_2024_subset.rds` — subconjunto de CASEN 2024 ya procesado, con
  solo las variables usadas en el ejemplo (esc, ytrabajocor, edad, sexo, area,
  region). No incluye comuna ni otras variables identificatorias.
- `code/01_analysis.R` — genera todo lo que hay en `output/`.

Genera:

| Archivo | Qué es |
|---|---|
| `desc_stats.tex` / `.csv` | Estadísticas descriptivas de la muestra |
| `regression_table.tex` / `.csv` | Regresión principal, con errores estándar robustos (HC1) |
| `esc_coef_progression.tex` / `.csv` | Los cinco modelos de la progresión de controles, lado a lado |
| `esc_by_model.csv` | El coeficiente de escolaridad de cada modelo, que alimenta el gráfico |
| `scatter_esc_lwage.png` | Dispersión entre escolaridad y log del ingreso |
| `esc_coef_progression_plot.png` | Cómo cae el coeficiente de escolaridad al agregar controles |

## Cómo correrlo

1. Instalar los paquetes (una sola vez):

   ```r
   install.packages(c("tidyverse", "haven", "sandwich", "modelsummary", "kableExtra"))
   ```

   `kableExtra` no se carga con `library()` pero hace falta: es el motor que usa
   `modelsummary` para escribir las tablas en LaTeX.

2. Abrir `code/01_analysis.R` y **editar la línea** `dir_proyecto <- "..."` con la
   ruta a esta carpeta en tu computadora. Es lo único que depende de la máquina;
   todo lo demás en el script usa rutas relativas (`input/...`, `output/...`).

3. Correr el script entero (Source). `output/` se crea sola.

## Qué deberías ver

En la consola, `N muestra: 74517` y un coeficiente de escolaridad de 0,116 en el
modelo simple. Si te dan esos números, corrió bien.

El mensaje de inicio de tidyverse (`dplyr::filter() masks stats::filter()`) es
normal, no es un error.

Sin R instalado, ver [cómo correrlo en el navegador](../README.md#sin-r-instalado-correrlo-en-el-navegador).
