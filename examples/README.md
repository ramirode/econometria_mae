# Ejemplos en R

Los ejemplos que se usan en clase, con los datos y el código que genera cada
tabla y cada figura de las diapositivas. Cada carpeta es autocontenida: se puede
descargar sola y correr.

| Ejemplo | Clase | Qué genera |
|---|---|---|
| [`1.intro_retornos-educacion`](1.intro_retornos-educacion) | Clase 1 | Regresión de Mincer con una progresión de controles (sesgo de variable omitida) |
| [`2.esperanza_condicional_educacion-salarios`](2.esperanza_condicional_educacion-salarios) | Clase 2 | Esperanza condicional del log salario dada la escolaridad, con regresión lineal y cuadrática |
| [`2.esperanza_condicional_tamano-clase`](2.esperanza_condicional_tamano-clase) | Clase 2 | Dispersión entre tamaño de clase y puntaje, con la recta de regresión |
| [`3.teoria_asintotica_simulacion-bernoulli`](3.teoria_asintotica_simulacion-bernoulli) | Clase 3 | Simulación de Monte Carlo: distribución muestral de la media y su versión estandarizada, para una Bernoulli |
| [`4.MCO_estimacion_tamano-clase`](4.MCO_estimacion_tamano-clase) | Clase 4 | Regresión de notas sobre tamaño de clase con errores robustos, cinco modelos con controles (sesgo de variable omitida), descriptivas y figuras |
| [`4.MCO_estimacion_formas-funcionales`](4.MCO_estimacion_formas-funcionales) | Clase 4 | Medias y coeficientes de los ejercicios de efectos marginales (niveles, logaritmos, dummies, interacciones) con datos de Wooldridge y Stock & Watson |
| [`4.MCO_estimacion_veteranos-angrist`](4.MCO_estimacion_veteranos-angrist) | Clase 4 (complementaria) | Efecto del servicio militar sobre ingresos (Angrist 1998): regresión simple, múltiple, sesgo de variable omitida y su descomposición |
| [`extra_discriminacion-mercado-laboral`](extra_discriminacion-mercado-laboral) | Complementaria | Discriminación por nombre: el mismo modelo estimado sobre un experimento y sobre datos observacionales, para ver cuándo los controles importan |

## Estructura de cada carpeta

```
<ejemplo>/
  code/     el script de R
  input/    los datos de entrada (no todos los ejemplos lo tienen -- el de simulación no usa datos reales)
  output/   tablas (.tex y .csv) y/o figuras (.png) -- se crea sola al correr el script
```

Cada tabla se guarda dos veces: en `.tex` (para `\input{}` en las diapositivas)
y en `.csv` (para abrirla en Excel o Google Sheets sin compilar LaTeX).

## Cómo correr cualquiera de ellos

1. Instalar los paquetes una sola vez. Estos cinco cubren todos los ejemplos:

   ```r
   install.packages(c("tidyverse", "haven", "sandwich", "modelsummary", "kableExtra"))
   ```

2. Abrir el script de `code/` en RStudio y **editar una sola línea**, la que dice
   `dir_proyecto <- "..."`, poniendo la ruta a la carpeta del ejemplo en tu
   computadora. Es lo único que depende de la máquina.

3. Correr el script entero (Source). La carpeta `output/` se crea sola.

## Sin R instalado: correrlo en el navegador

En [Posit Cloud](https://posit.cloud) (requiere cuenta gratuita):
*New Project → New Project from Git Repository* y pegar
`https://github.com/ramirode/econometria_mae.git`.

Instalar los paquetes del paso 1 —conviene hacerlo antes de la clase, `tidyverse`
tarda varios minutos en una instancia gratuita— y usar como ruta
`/cloud/project/examples/<nombre-del-ejemplo>`. Los datos vienen con el
repositorio, no hay que subir nada.

## Nota sobre los datos

Los subconjuntos de CASEN 2024 incluidos aquí traen solo las variables que usa
cada ejemplo y no incluyen comuna ni otras variables identificatorias. La CASEN
completa se descarga del
[Observatorio Social](https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen).

Los ejemplos de la clase 4 usan datos de uso público: los datos de Angrist (1998) sobre
solicitantes a las fuerzas armadas, el California Test Score Data Set (Stock y Watson),
`WAGE2` y `HPRICE2` (Wooldridge), y las bases CPS de Stock y Watson
(`cps92_08`, y `CPSSWEducation` del paquete
[AER](https://cran.r-project.org/package=AER)).

El ejemplo de discriminación usa dos fuentes de Estados Unidos: los datos del
experimento de Bertrand y Mullainathan (2004), distribuidos en el paquete
[AER](https://cran.r-project.org/package=AER) de CRAN, y la Current Population
Survey 2024, del [NBER](https://data.nber.org/morg/annual/).
