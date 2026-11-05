# Simulación de Monte Carlo: MCO vs. IV con instrumentos débiles o inválidos, y test de Anderson-Rubin

Ejemplo de la clase 7 (Variables instrumentales), sección Instrumentos débiles.
Simula 500 muestras de tamaño N=500 del modelo $y = 1 + x + u$ ($\beta_1 = 1$),
con $(u, z, x)$ normal estándar conjunta, y compara la distribución de
$\hat\beta_1$ estimado por MCO y por IV en cinco escenarios:

| Escenario | $\rho_{u,x}$ (exogeneidad de $x$) | $\rho_{u,z}$ (exogeneidad de $z$) | $\rho_{z,x}$ (relevancia de $z$) |
|---|---|---|---|
| 1. $x$ exógena | 0 | 0 | 0.20 |
| 2. Instrumento válido | 0.80 | 0 | 0.20 |
| 3. Instrumento endógeno | 0.80 | 0.20 | 0.20 |
| 4. Instrumento irrelevante | 0.80 | 0 | 0 |
| 5. Como el 2, más 20 instrumentos irrelevantes (2SLS) | 0.80 | 0 | 0.20 |

No usa datos reales: los datos se simulan adentro del script, no hay carpeta `input/`.

## Archivos

| Archivo | Qué es |
|---|---|
| `code/01_montecarlo_instrumentos_debiles.R` | El script: genera todo lo de `output/`. |
| `output/density_ols_iv<k>.png` | Densidad de $\hat\beta_1$ por MCO y por IV en el escenario $k$, con una línea en $\beta_1 = 1$. |
| `output/estimaciones.csv` | Las 500 estimaciones de MCO y de IV de cada escenario (los datos de los gráficos). |
| `output/resumen.csv` | Mediana y percentiles 5 y 95 de cada estimador en cada escenario. |
| `output/cobertura.tex`, `.csv` (y `cobertura_2sls.tex`) | Con $x$ endógena y distintos grados de relevancia de $z$ (2.000 simulaciones por fila): $F$ robusto mediano de la primera etapa, mediana de $\hat\beta_{1,IV}$, y cobertura de los intervalos de confianza al 95% de 2SLS y de Anderson-Rubin. |

## Cómo correrlo

1. Instalar el paquete (una sola vez):

   ```r
   install.packages("ggplot2")
   ```

2. Abrir `code/01_montecarlo_instrumentos_debiles.R` y **editar la línea**
   `dir_proyecto <- "..."` con la ruta a esta carpeta en tu computadora. Es lo
   único que hay que cambiar.

3. Correr el script entero (Source). `output/` se crea sola.

## Qué deberías ver

En la consola:

```
 escenario estimador    p05 mediana   p95
         1       MCO  0.936   1.001 1.069
         1        IV  0.604   0.993 1.348
         2       MCO  1.758   1.805 1.844
         2        IV  0.494   0.991 1.287
         3       MCO  1.755   1.800 1.842
         3        IV  1.795   2.004 2.263
         4       MCO  1.762   1.799 1.842
         4        IV -1.498   1.767 6.421
         5       MCO  1.758   1.802 1.844
         5        IV  1.195   1.410 1.585
 rho_zx instrumentos F_mediano mediana_b_iv cobertura_2sls cobertura_ar
   0.20            1      21.2        1.014          0.927        0.948
   0.10            1       5.1        1.009          0.907        0.940
   0.05            1       1.3        1.228          0.901        0.951
   0.00            1       0.5        1.804          0.871        0.954
   0.20           21       2.1        1.409          0.128        0.908
Listo. Figuras y tablas en output/
```

MCO se concentra en $1 + \rho_{u,x} = 1.8$ cuando $x$ es endógena. IV se centra
en $\beta_1 = 1$ con un instrumento válido (escenario 2), en
$1 + \rho_{u,z}/\rho_{z,x} = 2$ con el instrumento endógeno (escenario 3), y se
acerca a MCO cuando se agregan instrumentos irrelevantes (escenario 5).

La segunda tabla muestra la cobertura de los intervalos de confianza: el de 2SLS
contiene $\beta_1$ en menos del 95% de las simulaciones cuando el instrumento es
débil (13% con 21 instrumentos), mientras que el de Anderson-Rubin se mantiene
cerca del 95% con un instrumento, sea cual sea su relevancia.

Sin R instalado, ver [cómo correrlo en el navegador](../README.md#sin-r-instalado-correrlo-en-el-navegador).
