# Simulación de Monte Carlo: consistencia y TCL con una Bernoulli

Ejemplo de la clase 3 (Teoría asintótica): simula 50.000 muestras independientes
de una variable Bernoulli(p=0.78), para N=2,5,25,100, y grafica la distribución
muestral de la media $\bar y_N$ y de su versión estandarizada
$\sqrt{N}(\hat p-p)/\sqrt{p(1-p)}$ — ilustrando consistencia (la primera se
concentra en $p$) y el Teorema Central del Límite (la segunda se acerca a una
Normal(0,1)).

A diferencia de los otros ejemplos del curso, este no usa datos reales: los
datos se simulan adentro del script, no hay carpeta `input/`.

## Archivos

- `code/01_montecarlo_bernoulli.R` — genera todo lo que hay en `output/`.

Genera, para cada $N \in \{2,5,25,100\}$:

| Archivo | Qué es |
|---|---|
| `sample_mean<N>.png` | Histograma de $\bar y_N$ |
| `std_sample_mean<N>.png` | Histograma de $\sqrt{N}(\hat p-p)/\sqrt{p(1-p)}$, con la densidad Normal(0,1) superpuesta |

## Cómo correrlo

1. Instalar el paquete (una sola vez):

   ```r
   install.packages("ggplot2")
   ```

2. Abrir `code/01_montecarlo_bernoulli.R` y **editar la línea**
   `dir_proyecto <- "..."` con la ruta a esta carpeta en tu computadora. Es lo
   único que hay que cambiar.

3. Correr el script entero (Source). `output/` se crea sola.

## Qué deberías ver

En la consola, `Listo. Figuras en output/`. En `output/sample_mean100.png`, un
histograma angosto y concentrado alrededor de 0.78. En
`output/std_sample_mean100.png`, un histograma que sigue de cerca la curva roja
de la Normal(0,1).

Sin R instalado, ver [cómo correrlo en el navegador](../README.md#sin-r-instalado-correrlo-en-el-navegador).
