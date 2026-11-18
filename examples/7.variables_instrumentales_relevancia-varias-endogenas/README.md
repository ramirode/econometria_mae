# Relevancia con dos variables endógenas: F condicional y Cragg–Donald

Ejemplo de la clase 7 (Variables instrumentales), sección Relevancia de los instrumentos.
Simula $N=500$ observaciones de $y=1+x_1+x_2+w+u$ ($\beta_1=\beta_2=1$) con dos
endógenas y dos instrumentos, en dos escenarios:

| Escenario | Primeras etapas |
|---|---|
| A | $x_1$ depende de $z_1$ y $x_2$ de $z_2$ |
| B | $x_1=0.5z_1+0.5z_2+\dots$ y $x_2=0.5z_1+0.6z_2+\dots$ (casi proporcionales) |

Para cada escenario calcula el $F$ usual de cada primera etapa, el $F$ condicional de
Sanderson–Windmeijer, el estadístico de Cragg–Donald (errores homoscedásticos) y $\hat\beta_1$
con su error estándar. No usa datos reales: no hay carpeta `input/`.

## Archivos

| Archivo | Qué es |
|---|---|
| `code/01_relevancia_varias_endogenas.R` | El script: genera todo lo de `output/`. |
| `output/relevancia_varias_endogenas.tex`, `.csv` | La tabla de la lámina. |

## Cómo correrlo

1. Instalar los paquetes (una sola vez): `install.packages(c("fixest", "lfe"))`.
2. Editar la línea `dir_proyecto <- "..."` del script con la ruta a esta carpeta.
3. Correr el script entero.

El script también calcula el $F$ condicional a mano (`f_cond_mano`) y lo compara con `condfstat`.

Se usan `fixest::fitstat(~ ivf1 + cd)` para el $F$ usual y Cragg–Donald, y `lfe::condfstat` para
el $F$ condicional. El test de Kleibergen–Paap no está en el script: en Stata lo reporta `ivreg2`.
