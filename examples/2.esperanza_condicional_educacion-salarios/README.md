# Educación y salarios en Chile (CASEN 2024)

Ejemplo de la clase 2 (Esperanza condicional y modelo lineal de regresión):
la esperanza condicional del log del salario dados los años de escolaridad, y
qué tan bien la aproximan una regresión lineal y una cuadrática.

Población: hombres entre 30 y 65 años que trabajan.

## Archivos

- `input/casen_2024_subset.rds` — subconjunto de CASEN 2024 ya procesado, con
  solo las variables usadas (esc, ytrabajocor, edad, sexo). No incluye comuna ni
  otras variables identificatorias.
- `code/01_conditional_expectation.R` — genera todo lo que hay en `output/`.

Genera:

| Archivo | Qué es |
|---|---|
| `cond_expec_wage_educ1.png` | Esperanza condicional sola |
| `cond_expec_wage_educ2.png` | + regresión lineal |
| `cond_expec_wage_educ3.png` | + regresión cuadrática |
| `tabla_cef_universitario.tex` | Esperanza condicional del salario según si completó la universidad; es la tabla que usan las diapositivas de la ley de esperanzas iteradas y de modelos saturados |
| `tabla_cef_universitario.csv` | La misma tabla, para abrir en Excel |

## Cómo correrlo

1. Instalar los paquetes (una sola vez):

   ```r
   install.packages(c("tidyverse", "haven", "kableExtra"))
   ```

2. Abrir `code/01_conditional_expectation.R` y **editar la línea**
   `dir_proyecto <- "..."` con la ruta a esta carpeta en tu computadora. Es lo
   único que hay que cambiar.

3. Correr el script entero (Source). `output/` se crea sola.

## Qué deberías ver

En la consola, `N muestra (hombres 30-65, esc<=21): 39962`, y debajo la tabla de
esperanza condicional: salario medio 769 para no universitarios y 1961 para
universitarios, con probabilidades 0,834 y 0,166. Si te dan esos números, corrió
bien.

El mensaje de inicio de tidyverse (`dplyr::filter() masks stats::filter()`) es
normal, no es un error.

Sin R instalado, ver [cómo correrlo en el navegador](../README.md#sin-r-instalado-correrlo-en-el-navegador).
