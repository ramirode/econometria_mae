# Retornos a la educación (CASEN 2024): estima una regresion Mincer con una
# progresion de controles (esc -> +experiencia -> +mujer -> +rural -> +region)
# para mostrar como se mueve el coeficiente de escolaridad ante sesgo de
# variable omitida.
#
# Tablas -> codigo LaTeX (output/*.tex), para \input{} directo en el deck.
# Graficos -> imagenes (output/*.png), para \includegraphics.
#
# Poblacion: personas entre 30 y 65 anios que trabajan en Chile.
# Variables: esc (anios de escolaridad), ytrabajocor (ingreso del trabajo
# corregido), edad (para experiencia potencial), sexo, area (urbano/rural),
# region.

rm(list = ls()) # borra todos los objetos que pudieran quedar de una sesion anterior, para partir de cero

# library(x) carga un paquete (una libreria de funciones adicionales) para poder usarlo.
# Hay que haberlo instalado antes una vez con install.packages("x").
library(here)
here::i_am("code/01_analysis.R") # le dice al paquete here donde esta este script dentro del proyecto, para que here("input", ...) siempre apunte a la carpeta correcta sin importar desde donde se ejecute
library(tidyverse)  # incluye dplyr (filter, mutate, %>%), ggplot2 (graficos), tibble, etc.
library(sandwich)   # errores estandar robustos (HC1)
library(haven)      # para leer/etiquetar datos importados de Stata (as_factor)
library(modelsummary) # arma tablas de estadisticas descriptivas y de regresiones

options("modelsummary_factory_latex" = "kableExtra")   # booktabs simple, sin tabularray/siunitx
options("modelsummary_format_numeric_latex" = "plain") # sin \num{}, no requiere siunitx

# crea la carpeta output/ si todavia no existe (por ejemplo, en una copia recien clonada)
dir.create(here("output"), showWarnings = FALSE)

# readRDS() lee un archivo de datos en formato .rds (el formato nativo de R).
# here("input", "casen_2024_subset.rds") arma la ruta a ese archivo dentro de input/.
d <- readRDS(here("input", "casen_2024_subset.rds"))

# --- Muestra: personas 30-65 anios que trabajan ---
# %>% es el operador "pipe": toma lo que esta a la izquierda y lo pasa como
# primer argumento a la funcion de la derecha. Permite encadenar pasos sin
# tener que anidar funciones ni crear un objeto intermedio por cada paso.
# Aqui: d %>% filter(...) %>% mutate(...)  equivale a  mutate(filter(d, ...), ...)
sample <- d %>%
  # filter() se queda solo con las filas (personas) que cumplen todas estas condiciones:
  filter(edad >= 30, edad <= 65, !is.na(esc), !is.na(ytrabajocor), ytrabajocor > 0) %>%
  # mutate() crea columnas (variables) nuevas, una por linea, sin borrar las que ya existian
  mutate(
    lwage = log(ytrabajocor),              # logaritmo natural del ingreso (variable dependiente habitual en ecuaciones de Mincer)
    esc_lbl = "Escolaridad (esc)",
    ytrab_lbl = "Ingreso del trabajo (ytrabajocor)",
    exper = edad - esc - 6,               # experiencia potencial (proxy Mincer)
    exper2 = exper^2,                      # mismo nivel que exper, sin reescalar
    # if_else(condicion, valor_si_verdadero, valor_si_falso): crea una variable dummy (0/1)
    female = if_else(sexo == 2, 1, 0),     # sexo: 1 Hombre, 2 Mujer -> dummy mujer
    rural = if_else(area == 2, 1, 0),      # area: 1 Urbano, 2 Rural -> dummy rural
    region_f = haven::as_factor(region)    # un dummy por región (categórica, 16 niveles)
  )

cat("N muestra:", nrow(sample), "\n") # cat() imprime texto en la consola; nrow() cuenta filas (observaciones)

# --- Estadisticas descriptivas -> output/desc_stats.tex ---
# datasummary(filas ~ columnas, ...): a la izquierda de ~ van las variables a resumir
# (con un nombre "lindo" entre comillas para la tabla), a la derecha los estadisticos
# que se calculan para cada una (N, Media, Desviacion Estandar, percentiles).
desc_tex <- datasummary(
  (`Escolaridad (esc)` = esc) + (`Ingreso del trabajo (ytrabajocor)` = ytrabajocor) ~
    (`N` = N) + (`Media` = Mean) + (`DE` = SD) + (`P25` = P25) + (`Mediana` = Median) + (`P75` = P75),
  data = sample, output = "latex", fmt = 0
)
# writeLines() escribe texto (aca, el codigo LaTeX de la tabla) en un archivo de disco
writeLines(as.character(desc_tex), here("output", "desc_stats.tex"))

# --- Regresion simple (naive): log(ytrabajocor) ~ esc, errores robustos (HC1) -> output/regression_table.tex ---
# lm(y ~ x, data = ...) estima una regresion lineal (OLS) de y sobre x.
# Aca: log(ingreso) ~ escolaridad, es decir el retorno a un anio adicional de escolaridad.
ols <- lm(lwage ~ esc, data = sample)

# modelsummary() arma una tabla de resultados de la regresion.
# vcov = "HC1" pide errores estandar robustos a heterocedasticidad (en vez de los clasicos de lm()).
reg_tex <- modelsummary(
  ols, vcov = "HC1", output = "latex", stars = FALSE,
  coef_rename = c("(Intercept)" = "Constante", "esc" = "Escolaridad"), # nombres de fila mas lindos para la tabla
  gof_map = c("nobs", "r.squared") # que estadisticos de ajuste mostrar (N y R cuadrado)
)
writeLines(as.character(reg_tex), here("output", "regression_table.tex"))

# --- Progresion de modelos: naive -> +experiencia -> +mujer -> +rural -> +region ---
# Se estima la misma regresion agregando controles de a uno, para ver como
# cambia el coeficiente de escolaridad (posible sesgo de variable omitida).
m1 <- lm(lwage ~ esc, data = sample)
m2 <- lm(lwage ~ esc + exper + exper2, data = sample)
m3 <- lm(lwage ~ esc + exper + exper2 + female, data = sample)
m4 <- lm(lwage ~ esc + exper + exper2 + female + rural, data = sample)
m5 <- lm(lwage ~ esc + exper + exper2 + female + rural + region_f, data = sample)

# list(nombre = objeto, ...) agrupa los 5 modelos en una sola lista, con un
# nombre para cada uno, para poder pasarlos juntos a modelsummary().
models <- list(
  "(1) Naive" = m1,
  "(2) + Exp." = m2,
  "(3) + Mujer" = m3,
  "(4) + Rural" = m4,
  "(5) + Región" = m5
)

# Tabla de comparacion: solo el coeficiente de 'esc' (esa es la historia), errores
# estandar robustos (HC1), en output/esc_coef_progression.tex
progression_tex <- modelsummary(
  models, vcov = "HC1", output = "latex", stars = FALSE,
  coef_map = c("esc" = "Escolaridad"), # muestra solo la fila de 'esc' (no los demas controles)
  gof_map = c("nobs")
)
writeLines(as.character(progression_tex), here("output", "esc_coef_progression.tex"))

# Datos para el grafico de estabilidad del coeficiente (coeficiente e IC 95%, HC1)
# map_dfr(x, funcion) aplica la funcion a cada elemento de x (aca, a cada nombre de
# modelo) y junta los resultados en una sola tabla (data frame), fila por fila.
esc_by_model <- map_dfr(names(models), function(nm) {
  mod <- models[[nm]]
  # vcovHC(mod, type = "HC1") calcula la matriz de varianzas-covarianzas robusta;
  # ["esc", "esc"] toma la varianza del coeficiente de esc, y su raiz es el error estandar.
  se <- sqrt(vcovHC(mod, type = "HC1")["esc", "esc"])
  est <- coef(mod)["esc"] # coef(mod) extrae los coeficientes estimados del modelo; ["esc"] se queda con el de escolaridad
  # tibble() arma una fila de tabla con estas columnas; el intervalo de confianza al
  # 95% se calcula como estimacion +/- 1.96 * error estandar (aproximacion normal)
  tibble(especificacion = nm, coef_esc = est, se_robusto = se,
         ci_low = est - 1.96 * se, ci_high = est + 1.96 * se)
})
print(esc_by_model)

# Grafico: como se mueve el coeficiente de escolaridad (aspecto ancho, para caber en un slide 16:9)
# ggplot() arma graficos por capas: primero se definen los datos y los ejes (aes),
# despues se van agregando capas con + (puntos, lineas, etiquetas, estilo del grafico).
coef_plot <- ggplot(esc_by_model, aes(x = especificacion, y = coef_esc)) +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high), color = "#1F77B4", linewidth = 0.8) + # punto = coeficiente, linea = intervalo de confianza
  labs(x = NULL, y = "Coef. escolaridad (IC 95%)") + # titulos de los ejes (NULL = sin titulo en el eje x)
  theme_minimal(base_size = 13) # estilo visual del grafico

# ggsave() guarda el ultimo grafico armado (o el que se le pase en plot=) como imagen en disco
ggsave(here("output", "esc_coef_progression_plot.png"), plot = coef_plot, width = 9, height = 3.3, dpi = 200)

# --- Grafico: dispersion + recta ajustada (sin titulo, para insertar en slide) ---
scatter_plot <- ggplot(sample, aes(x = esc, y = lwage)) +
  geom_jitter(width = 0.15, alpha = 0.05, size = 0.5) +               # nube de puntos, con jitter (ruido) y transparencia para que no se amontonen
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4", linewidth = 1.2) + # recta de regresion lineal ajustada a esos mismos puntos
  labs(x = "Años de escolaridad", y = "log(Ingreso del trabajo)") +
  theme_minimal(base_size = 13)

ggsave(here("output", "scatter_esc_lwage.png"), scatter_plot, width = 7, height = 4.5, dpi = 200)

cat("Listo. Tablas y gráficos en output/.\n")
