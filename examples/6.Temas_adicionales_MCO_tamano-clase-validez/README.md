# Tamaño de clase, forma funcional y validez interna y externa

Ejemplo de la clase 6 (Temas adicionales de MCO). Estima el efecto del tamaño de clase sobre
el puntaje en tres estudios y los compara: California y Massachusetts (distritos escolares, datos
observacionales) y el experimento STAR de Tennessee. Todos los errores estándar son robustos (HC1).

## Archivos

| Archivo | Qué es |
|---|---|
| `input/caschool.dta` | California Test Score Data Set (Stock y Watson): 420 distritos escolares, 1998-1999. |
| `input/mcas.dta` | Massachusetts Test Score Data Set (Stock y Watson): 220 distritos escolares, 1997-1998. |
| `input/star_sw.dta` | Proyecto STAR (Stock y Watson): estudiantes de kinder a 3er grado en Tennessee. |
| `code/01_tamano_clase.R` | El script: genera todo lo de `output/`. |
| `output/reg_ca_formas.tex` / `.csv` | California, 4 modelos: controles, ingreso en logaritmos, interacción y polinomio cúbico en `str`. |
| `output/me_ca.tex` / `.csv` | Efecto marginal promedio de `str` en cada modelo de California. |
| `output/reg_ca_interaccion.tex` / `.csv` | Modelo con la interacción centrada: el coeficiente de `str` es el efecto en la media de `el_pct`. |
| `output/reg_ma_formas.tex` / `.csv`, `output/me_ma.tex` / `.csv` | Lo mismo para Massachusetts (5 modelos). |
| `output/test_conjunto_formas.tex` / `.csv` | Test F robusto de H₀: `str²` = `str³` = 0. |
| `output/desc_ca_ma.tex` / `.csv` | Medias y desviaciones estándar, California vs Massachusetts. |
| `output/efecto_2_estudiantes.tex` / `.csv` | Efecto de reducir 2 estudiantes por profesor, en puntos y en desviaciones estándar. |
| `output/reg_star.tex` / `.csv` | STAR: puntaje en kinder, 1er, 2do y 3er grado. |
| `output/comparacion_star.tex` / `.csv` | Efecto estimado en desviaciones estándar: STAR, California y Massachusetts. |
| `output/fig_ingreso_*.pdf` | Puntaje vs ingreso (ajustes lineal, nivel-log y cuadrático), regresión particionada y residuos. |

Las desviaciones estándar del puntaje entre estudiantes de California (38.0) y Massachusetts (39.0)
en `comparacion_star` son las que reportan Stock y Watson; no se pueden calcular con datos por distrito.

## Cómo correrlo

1. Instalar los paquetes (una sola vez): `install.packages(c("haven", "sandwich", "car", "ggplot2", "knitr"))`.
2. Abrir `code/01_tamano_clase.R` y editar la línea `dir_proyecto <- "..."` con la ruta a esta carpeta
   en tu computadora. Es lo único que hay que cambiar.
3. Correr el script entero (Source). `output/` se crea sola.
