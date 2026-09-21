# Discriminación en el mercado laboral: experimento vs. datos observacionales

Ejemplo empírico que acompaña al deck **"Discriminación en el mercado laboral"**
(`slides/extra/`). Se usa como aplicación de sesgo de variable omitida y del
supuesto de media condicional cero (clase 4, Estimación por MCO).

La idea es correr **dos regresiones que en el papel se ven idénticas** —variable
dependiente binaria, una dummy racial, una progresión de controles— y mostrar
que se interpretan de manera completamente distinta, porque en una la variable
de interés fue asignada al azar y en la otra no.

**Parte A — El experimento.** Bertrand y Mullainathan (2004) mandaron 4.870
currículos ficticios a avisos de trabajo reales en Boston y Chicago, asignando
**al azar** a cada uno un nombre típicamente blanco (Emily, Greg) o típicamente
afroamericano (Lakisha, Jamal). Población: postulaciones a avisos de empleo de
entrada en ventas y apoyo administrativo, 2001–2002.

**Parte B — Los datos observacionales.** Current Population Survey (CPS) 2024,
la encuesta de hogares de Estados Unidos. Población: adultos de 22 a 40 años,
blancos o negros no hispanos. Misma forma de regresión, pero acá la raza no se
asignó al azar.

El contraste es el punto del ejemplo:

| | Sin controles | Con todos los controles | Cuánto se mueve |
|---|---|---|---|
| **A. Experimento** (llamado para entrevista) | −0,0320 | −0,0312 | **2,5 %** |
| **B. CPS 2024** (estar empleado) | −0,0778 | −0,0358 | **54,0 %** |

## Archivos

| Archivo | Qué es |
|---|---|
| `output/callback_rates.png` | Tasa de llamados por grupo de nombre, con intervalo de confianza al 95 % |
| `output/callback_rates.csv` | Los números de esa figura |
| `output/balance_audit.tex` / `.csv` | Tabla de balance: medias de las 11 características del currículo en cada grupo, la diferencia y su error estándar. Es la verificación de que la aleatorización funcionó |
| `output/reg_audit.tex` / `.csv` | Parte A: coeficiente de la dummy racial en cuatro especificaciones, errores robustos (HC1) |
| `output/desc_cps.tex` / `.csv` | Parte B: descriptivas del CPS por grupo racial |
| `output/reg_cps.tex` / `.csv` | Parte B: coeficiente de la dummy racial en cuatro especificaciones, errores robustos (HC1) |
| `output/coef_comparison.png` | La figura que resume todo: los dos paneles lado a lado, con línea punteada en la estimación sin controles |
| `output/coef_comparison.csv` | Los ocho coeficientes con su intervalo de confianza |
| `output/coef_change.csv` | Cuánto se mueve el coeficiente entre el modelo (1) y el (4), en porcentaje |
| `code/01_analysis.R` | El script que hay que correr |

## Cómo correrlo

1. Instalar los paquetes, una sola vez:

   ```r
   install.packages(c("tidyverse", "sandwich", "modelsummary", "kableExtra"))
   ```

2. Abrir `code/01_analysis.R` en RStudio y **editar una sola línea**, la que dice
   `dir_proyecto <- "..."`, poniendo la ruta a esta carpeta en tu computadora.

3. Correr el script entero (Source). La carpeta `output/` se crea sola.

## Qué deberías ver

En la consola, al principio y al final:

```
Experimento: N = 4870 curriculos enviados
CPS: N = 46885 personas de 22 a 40 anios
```

```
  panel                                    coef_1  coef_4 cambio_pct
1 A. Experimento: llamado para entrevista -0.0320 -0.0312       2.53
2 B. CPS 2024: estar empleado             -0.0778 -0.0358      54.0
```

Esa última tabla es el ejemplo entero en cuatro números: en el experimento
agregar quince controles mueve el coeficiente un 2,5 %; en el CPS lo mueve un
54 %.

## Sin R instalado

Se puede correr en el navegador con Posit Cloud: ver la sección correspondiente
del [README de los ejemplos](../README.md).

## Fuentes de los datos

- **Experimento**: Bertrand, M. y S. Mullainathan (2004), "Are Emily and Greg
  More Employable than Lakisha and Jamal? A Field Experiment on Labor Market
  Discrimination", *American Economic Review* 94(4): 991–1013. Los datos se
  distribuyen dentro del paquete [AER](https://cran.r-project.org/package=AER)
  de CRAN, como `ResumeNames`.
- **Observacional**: CPS Merged Outgoing Rotation Group 2024, descargado del
  [NBER](https://data.nber.org/morg/annual/).

Los `.rds` de `input/` traen solo las variables que usa el ejemplo y ya vienen
armados: no hace falta bajar nada. Se generan con un script aparte,
`00_download_and_subset.R`, que baja las dos fuentes crudas (el `.tar.gz` de AER
y el `morg24.dta` de 44 MB del NBER) y queda en la carpeta de trabajo del curso,
fuera del repositorio.
