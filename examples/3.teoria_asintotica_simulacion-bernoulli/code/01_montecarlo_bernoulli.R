# Simulacion de Monte Carlo: distribucion muestral de la media muestral
# ybar_N y de su version estandarizada sqrt(N)(phat-p)/sqrt(p(1-p)), para una
# variable Bernoulli(p=0.78), con N=2,5,25,100. Reproduce las figuras de
# consistencia (deck 3.teoria_asintotica, ejemplos de las secciones de
# Consistencia y del Teorema Central del Limite): al crecer N, la primera se
# concentra en p; la segunda se acerca a una Normal(0,1).

# --- Estructura de carpetas esperada ---
# Este script asume que existe una carpeta de proyecto con esta estructura:
#
#   3.teoria_asintotica_simulacion-bernoulli/
#     code/
#       01_montecarlo_bernoulli.R   <- este archivo
#     output/                        <- se crea sola al correr el script (figuras van aca)
#
# No hay carpeta input/: a diferencia de los otros ejemplos del curso, este no
# usa datos reales, los datos se simulan adentro del script.

# Reemplazar por la ruta a la carpeta "3.teoria_asintotica_simulacion-bernoulli" en TU computadora.
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/examples/3.teoria_asintotica_simulacion-bernoulli"
setwd(dir_proyecto) # fija la carpeta de trabajo: de aca en mas, "output/..." apunta siempre ahi

rm(list = ls()) # borra todos los objetos que pudieran quedar de una sesion anterior, para partir de cero

# library(x) carga un paquete (una libreria de funciones adicionales) para poder usarlo.
# Hay que haberlo instalado antes una vez con install.packages("x").
library(ggplot2) # graficos

dir.create("output", showWarnings = FALSE) # crea la carpeta output/ si no existe todavia

# --- parametros de la simulacion ---
sample_sizes <- c(2, 5, 25, 100) # tamanios de muestra N que vamos a comparar
n_simul <- 50000                 # cuantas muestras independientes simulamos para cada N
prob_success <- 0.78             # p verdadero de la Bernoulli (parametro poblacional, lo elegimos nosotros)
set.seed(20260907)               # fija la semilla del generador de numeros aleatorios: corriendo el script
                                  # de nuevo con la misma semilla se obtienen exactamente los mismos numeros

# for(N in sample_sizes) repite todo lo de adentro una vez por cada valor de N en sample_sizes
for (N in sample_sizes) {
  # runif(n) genera n numeros aleatorios uniformes entre 0 y 1; comparandolos con
  # prob_success obtenemos un vector de TRUE/FALSE, que es exactamente una
  # variable Bernoulli(p) (TRUE con probabilidad p). matrix(..., nrow=n_simul, ncol=N)
  # acomoda esos N*n_simul sorteos en una tabla: cada fila es una muestra
  # independiente de tamanio N.
  draws <- matrix(runif(N * n_simul) <= prob_success, nrow = n_simul, ncol = N)
  sample_mean <- rowMeans(draws) # rowMeans calcula el promedio de cada fila -> un ybar por muestra simulada
  std_sample_mean <- sqrt(N) * (sample_mean - prob_success) / sqrt(prob_success * (1 - prob_success))
  # ^ version estandarizada: usamos el p VERDADERO (no un estimado) para dividir,
  # por eso esta version converge exactamente a una Normal(0,1), no solo a "alguna" normal.

  # --- histograma de la media muestral (toma valores k/N para k=0,...,N) ---
  # center=0 hace que cada punto de soporte k/N quede en el CENTRO de su propia
  # barra, en vez de en el borde -- si quedara en el borde, la barra de k=N
  # aparece cortada por el limite del grafico (solo se ve una tira finita).
  d_mean <- data.frame(x = sample_mean)
  margin <- 0.5 / N
  p_mean <- ggplot(d_mean, aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1 / N, center = 0,
                    fill = "#4C72B0", color = "white") +
    coord_cartesian(xlim = c(-margin, 1 + margin)) +
    labs(x = paste0("media muestral, N = ", N), y = "densidad") +
    theme_minimal(base_size = 13)
  ggsave(paste0("output/sample_mean", N, ".png"), p_mean, width = 4, height = 3.2, dpi = 200)

  # --- histograma de la media estandarizada, con la densidad Normal(0,1) superpuesta ---
  # std_sample_mean vive en una grilla de puntos con espaciado exacto
  # 1/sqrt(N*p*(1-p)); el ancho y centro de las barras del histograma tienen
  # que calzar exactamente con esa grilla, si no dos puntos de soporte
  # distintos pueden caer en la misma barra y aparece un pico artificial.
  lattice_step <- 1 / sqrt(N * prob_success * (1 - prob_success))
  mode_k <- round(N * prob_success)
  mode_std <- (mode_k - N * prob_success) / sqrt(N * prob_success * (1 - prob_success))
  d_std <- data.frame(x = std_sample_mean)
  p_std <- ggplot(d_std, aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = lattice_step,
                    center = mode_std, fill = "#4C72B0", color = "white") +
    stat_function(fun = dnorm, color = "#C44E52", linewidth = 1, xlim = c(-4, 4)) + # curva Normal(0,1) de referencia
    coord_cartesian(xlim = c(-4, 4)) +
    labs(x = bquote(sqrt(N) ~ (hat(p) - p) / sqrt(p(1 - p)) *","~ N == .(N)), y = "densidad") +
    theme_minimal(base_size = 13)
  ggsave(paste0("output/std_sample_mean", N, ".png"), p_std, width = 4, height = 3.2, dpi = 200)
}

cat("Listo. Figuras en output/\n")
