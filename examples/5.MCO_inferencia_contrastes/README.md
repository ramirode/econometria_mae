# Inferencia por MCO: contrastes de hipótesis e intervalos de confianza

Ejemplo de la clase 5 (Modelo lineal de regresión: inferencia por MCO). Calcula,
con errores estándar robustos, todos los contrastes de las diapositivas: el test
t y el intervalo de confianza para el tamaño de clase, el test de Wald de una y
de varias restricciones lineales para los retornos de la educación, y el método
delta y el test de Wald no lineal para el precio de las viviendas.

Los estadísticos de Wald se calculan "a mano", con las mismas fórmulas de las
diapositivas, para que se vea qué hace cada paso.

## Archivos

| Archivo | Qué es |
|---|---|
| `input/caschool.dta` | California Test Score Data Set (Stock y Watson): 420 distritos escolares de California, 1998-1999. |
| `input/wage2.dta` | `WAGE2` (Wooldridge): 935 hombres de la NLSY, salarios en 1980 (Blackburn y Neumark, QJE 1992). |
| `input/hprice2.dta` | `HPRICE2` (Wooldridge): 506 comunidades del área de Boston en 1970 (Harrison y Rubinfeld, JEEM 1978). |
| `code/01_contrastes.R` | El script: genera todo lo de `output/`. |
| `output/reg_ca.tex` / `.csv` | `testscr ~ str + el_pct`: coeficientes, errores estándar, t, p-valor e intervalo de confianza al 95%. |
| `output/reg_nlsy.tex` / `.csv` | `log(wage) ~ educ + IQ + exper + tenure + married + south + urban`. |
| `output/vcov_nlsy.tex` / `.csv` | Matriz de varianzas y covarianzas estimada de `educ`, `IQ`, `exper` y `tenure` (multiplicada por 10⁶). |
| `output/test_exper_tenure.tex` / `.csv` | Test de Wald de H₀: β_exper = β_tenure. |
| `output/test_educ_iq.tex` / `.csv` | Test de Wald de H₀: β_educ = β_IQ = 0. |
| `output/test_global.tex` / `.csv` | Test de significatividad global (todas las pendientes son cero). |
| `output/reg_hprice.tex` / `.csv` | `log(price) ~ log(nox) + rooms + stratio`. |
| `output/delta_rooms.tex` / `.csv` | (exp(β_rooms) − 1) × 100 y su error estándar por el método delta. |
| `output/wald_invariancia.tex` / `.csv` | La misma hipótesis escrita de dos formas da dos estadísticos de Wald distintos. |
| `output/local_power_tstat.pdf` | Poder asintótico local del test t en función de δ, para α = 0.10, 0.05 y 0.01. |

Cada tabla se guarda en `.tex` (para las diapositivas) y en `.csv` (para abrirla
en Excel o Google Sheets).

## Cómo correrlo

1. Instalar los paquetes (una sola vez):

   ```r
   install.packages(c("haven", "sandwich", "knitr"))
   ```

2. Abrir `code/01_contrastes.R` y **editar la línea** `dir_proyecto <- "..."`
   con la ruta a esta carpeta en tu computadora. Es lo único que hay que cambiar.

3. Correr el script entero (Source). `output/` se crea sola.

## Qué deberías ver

En la consola:

```
CA: coef. de str -1.101 | e.e. 0.433 | IC 95% (normal): [ -1.95 , -0.253 ]
NLSY: t de exper - tenure: 0.505 | p-valor (normal): 0.614 
NLSY: W de educ = IQ = 0: 168.23 
NLSY: inversa de la submatriz de educ e IQ:
       educ      IQ
educ  26228  109006
IQ   109006 1605875
Boston: r(b) = 29.269 | Avar(r) = 10.776 | W no lineal = 79.5 | W lineal = 102.2 
Listo. Tablas y grafico en output/.
```

La última línea de Boston es el punto del final de la clase: β_rooms = 0 y
(exp(β_rooms) − 1) × 100 = 0 son la misma hipótesis, pero el estadístico de Wald
vale 102.2 en una formulación y 79.5 en la otra.

Sin R instalado, ver [cómo correrlo en el navegador](../README.md#sin-r-instalado-correrlo-en-el-navegador).
