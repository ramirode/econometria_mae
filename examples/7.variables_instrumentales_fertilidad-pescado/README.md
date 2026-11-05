# Variables instrumentales y 2SLS: fertilidad y oferta laboral, y demanda de pescado

Ejemplo de la clase 7 (Variables instrumentales y mínimos cuadrados en dos etapas).
Calcula todas las tablas de las diapositivas: el modelo simple de Angrist y Evans
(primera etapa, forma reducida, mínimos cuadrados indirectos, 2SLS "a mano" y
estimador de Wald, e intervalo de confianza de Anderson-Rubin), el modelo con
controles y dos instrumentos, y la demanda de pescado de Graddy (relevancia de los
instrumentos, intervalo de Anderson-Rubin, test de sobreidentificación y test de
endogeneidad).

El estimador 2SLS y su matriz de varianzas y covarianzas robusta se calculan "a
mano", con las mismas fórmulas de las diapositivas: no hace falta ningún paquete
de variables instrumentales.

## Archivos

| Archivo | Qué es |
|---|---|
| `input/angrist_evans.rds` | Censo de EE.UU. de 1980 (Angrist y Evans, AER 1998): 262,793 mujeres casadas de 21 a 35 años con al menos dos hijos. |
| `input/fish.rds` | Ventas diarias de merluza de un mayorista del mercado de Fulton, Nueva York, del 2 de diciembre de 1991 al 8 de mayo de 1992 (Graddy, RAND 1995): 97 días. |
| `code/01_variables_instrumentales.R` | El script: genera todo lo de `output/`. |
| `code/02_comandos_paquetes.R` | Los comandos de las láminas "En R: ..." de la clase, en el mismo orden: MCO robusto, 2SLS, $F$ efectivo, intervalo de Anderson-Rubin, test de relevancia y tests de Sargan y Wu-Hausman, con los paquetes `sandwich`, `lmtest`, `car`, `ivreg` e `ivDiag`. Solo imprime en la consola. |
| `output/ae_desc` | Estadísticas descriptivas de Angrist y Evans. |
| `output/ae_mco`, `ae_primera`, `ae_reducida` | MCO de `hourswm` sobre `morekids`, primera etapa (`morekids` sobre `samesex`) y forma reducida (`hourswm` sobre `samesex`). |
| `output/ae_2sls_mano`, `ae_2sls` | 2SLS "a mano" (dos regresiones por MCO, con errores estándar mal calculados) y 2SLS con la fórmula correcta. |
| `output/ae_wald` | Medias de `hourswm` y `morekids` según `samesex` (estimador de Wald). |
| `output/ae_ar` | Intervalo de confianza al 95% de 2SLS y de Anderson-Rubin para el efecto de `morekids`. |
| `output/aeg_mco`, `aeg_primera`, `aeg_2sls` | El mismo ejemplo con controles y dos instrumentos (`boys2`, `girls2`). |
| `output/aeg_iv_pred`, `aeg_dos_mco` | IV con la predicción de la primera etapa como instrumento (errores estándar correctos) y dos veces MCO (incorrectos). |
| `output/fish_mco` | Demanda de pescado por MCO: `ltotqty` sobre `lavgprc` y días de la semana. |
| `output/fish_primera4`, `fish_relevancia`, `fish_primera2` | Primera etapa con los cuatro instrumentos de clima, tests F de relevancia, y primera etapa solo con `wave2` y `wave3`. |
| `output/fish_2sls`, `fish_2sls_w23` | 2SLS con los cuatro instrumentos (especificación base) y solo con `wave2` y `wave3` (alternativa). |
| `output/fish_2sls_wave2`, `fish_2sls_wave3` | Los dos modelos exactamente identificados con `wave2` o `wave3`. |
| `output/fish_overid_mano`, `fish_overid` | Test de sobreidentificación: regresión de los residuos de 2SLS (cuatro instrumentos) sobre las exógenas, y los estadísticos de Sargan (residuos de 2SLS) y $J$ de Hansen (residuos de GMM eficiente), con cuatro y con dos instrumentos. |
| `output/fish_cf` | Test de endogeneidad por función de control: MCO controlando por los residuos de la primera etapa con los cuatro instrumentos (`vhat`). |
| `output/fish_ar` | Intervalo de confianza al 95% de 2SLS y de Anderson-Rubin para el efecto de `lavgprc`, con los cuatro instrumentos y solo con `wave2` y `wave3`. |

Cada tabla se guarda en `.tex` (para las diapositivas) y en `.csv` (para abrirla
en Excel o Google Sheets).

## Cómo correrlo

1. Instalar los paquetes (una sola vez):

   ```r
   install.packages(c("sandwich", "knitr"))
   ```

2. Abrir `code/01_variables_instrumentales.R` y **editar la línea**
   `dir_proyecto <- "..."` con la ruta a esta carpeta en tu computadora. Es lo
   único que hay que cambiar.

3. Correr el script entero (Source). `output/` se crea sola.

## Qué deberías ver

En la consola:

```
Angrist-Evans: N = 262793
  MCO morekids: -3.687 (0.073) | primera etapa: 0.0670 | forma reducida: -0.3624 | ILS: -5.409
  2SLS: -5.409 (1.066) | a mano: -5.409 (1.070)
  Con controles: MCO -5.956 | 2SLS -4.426 (0.998) | dos veces MCO e.e. 1.010 | F primera etapa 727.61
Graddy: N = 97 | MCO -0.525 | F primera etapa (4 instr.) 10.33 | (speed) 0.06 | (wave) 20.77
  2SLS 4 instr. -0.821 (0.323) | wave2 y wave3 -0.816 (0.323)
  Sobreid. 4 instr.: Sargan 4.652, Hansen J 4.667 (GMM -0.728) | wave2 y wave3: Sargan 0.028, Hansen J 0.026 (GMM -0.808)
  Endogeneidad (funcion de control): 4 instr. F = 1.15, p = 0.287 | wave2 y wave3 F = 1.11, p = 0.295
Anderson-Rubin: Angrist-Evans [-7.507, -3.322] | Graddy 4 instr. [-1.679, -0.168] | wave2 y wave3 [-1.859, -0.045]
Listo. Tablas en output/.
```

Dos puntos de la clase se ven en la segunda y la cuarta línea: el coeficiente de
2SLS "a mano" es el correcto (−5.409), pero su error estándar no (1.070 en lugar
de 1.066); y con controles, dos veces MCO da un error estándar de 1.010 en lugar
de 0.998. La última línea muestra los intervalos de Anderson-Rubin: con el
instrumento fuerte de Angrist y Evans, el intervalo casi coincide con el de 2SLS
([−7.499, −3.319]); en Graddy es más ancho que el de 2SLS, con cuatro
instrumentos ($F = 10.3$; 2SLS [−1.454, −0.187]) y con dos ($F = 20.8$; 2SLS
[−1.450, −0.182]).

## Comandos con paquetes (`02_comandos_paquetes.R`)

Instalar los paquetes una sola vez:

```r
install.packages(c("sandwich", "lmtest", "car", "ivreg", "ivDiag"))
```

Editar `dir_proyecto` igual que en el primer script y correrlo. Reproduce las
tablas de la clase: por ejemplo, 2SLS de Angrist y Evans −5.409 (e.e. robusto
1.066), intervalo de Anderson-Rubin `"[-7.5074, -3.3215]"`, 2SLS con controles
−4.426 (0.998), $F$ de relevancia en Graddy 10.33 y 0.06, y Sargan 4.652 con
los cuatro instrumentos. La única diferencia son los intervalos de Anderson-Rubin
de Graddy, `"[-1.6264, -0.1934]"` y `"[-1.8278, -0.0610]"` en lugar de
[−1.679, −0.168] y [−1.859, −0.045], porque `AR_test` usa otra corrección de
grados de libertad.

Sin R instalado, ver [cómo correrlo en el navegador](../README.md#sin-r-instalado-correrlo-en-el-navegador).
