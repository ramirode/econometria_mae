# Formas funcionales y efectos marginales

Ejemplo de la clase 4 (Modelo lineal de regresión: estimación por MCO). Calcula
las medias muestrales y los coeficientes que se dan como información en los
ejercicios de efectos marginales de la clase: modelos lineal, cuadrático, con
interacciones, con logaritmos y con variables binarias.

## Archivos

| Archivo | Qué es |
|---|---|
| `input/wage2.dta` | Wooldridge: salarios mensuales, educación, experiencia e IQ de 935 hombres. |
| `input/hprice2.dta` | Wooldridge: precios de viviendas y contaminación (`nox`) en 506 comunidades del área de Boston. |
| `input/cps92_08.dta` | Stock & Watson: ingreso por hora de trabajadores de 25 a 34 años con secundaria o título universitario, CPS 1992 y 2008 (15,316 observaciones, dólares corrientes de cada año). |
| `input/cps_education_2004.rds` | Stock & Watson, tal como se distribuye en el paquete [AER](https://cran.r-project.org/package=AER) (`CPSSWEducation`): ingreso por hora, educación y sexo, CPS de marzo de 2005, trabajadores de 29 a 30 años (2,950 observaciones). |
| `code/01_formas_funcionales.R` | El script. |
| `output/info_ejercicios.csv` | Una fila por cantidad (ejercicio, cantidad, valor). |

Las medias y coeficientes de California (ejercicio de la slide de tamaño de
clase) salen del ejemplo [`4.MCO_estimacion_tamano-clase`](../4.MCO_estimacion_tamano-clase).

## Cómo correrlo

1. Instalar el paquete (una sola vez):

   ```r
   install.packages("haven")
   ```

2. Abrir `code/01_formas_funcionales.R` y **editar la línea** `dir_proyecto <- "..."`
   con la ruta a esta carpeta en tu computadora.

3. Correr el script entero (Source). `output/` se crea sola.

## Qué deberías ver

La consola imprime cada cantidad. Algunas de referencia:

```
wage2 nivel                  beta_educ             22.7924
hprice2 log-log              beta_log(nox)         -0.6453
hprice2 cambio exacto        pct_rooms             29.2692
cps 2008                     beta_bachelor          7.5766
cps 1992-2008 ($ 2008)       beta_bachelor:d2008    1.1067
```

En el ejemplo de CPS 1992-2008 el ingreso de 1992 se expresa en dólares de 2008
(multiplicado por 215.2 / 140.3, el cociente de IPC), porque el archivo trae
dólares corrientes de cada año.

Sin R instalado, ver [cómo correrlo en el navegador](../README.md#sin-r-instalado-correrlo-en-el-navegador).
