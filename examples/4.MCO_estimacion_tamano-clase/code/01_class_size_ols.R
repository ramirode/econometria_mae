# Tamano de clase y rendimiento academico: estimacion por MCO con errores
# estandar robustos, para el deck 4.MCO_estimacion.
#
# Genera (en output/):
#   - desc_stats.tex/.csv      estadisticas descriptivas de str y testscr
#   - scatter_str_testscr.png  dispersion de notas contra ratio alumnos/profesor
#   - ols_str_testscr.png      la misma dispersion con la recta de regresion
#   - reg_model1.tex/.csv      testscr ~ str
#   - reg_model2.tex/.csv      testscr ~ str + el_pct
#   - reg_5models.tex/.csv     los cinco modelos con controles (sesgo de variable omitida)
#   - ovb_check.csv            correlacion entre str y el_pct y cambio del coeficiente de str
#   - info_ejercicio.csv       medias y coeficientes del ejercicio de efectos marginales
#
# Tablas -> codigo LaTeX (.tex), para \input{} directo en el deck.
# Graficos -> imagenes (.png), para \includegraphics.
#
# Datos: "California Test Score Data Set" (Stock & Watson), 420 distritos
# escolares de California, ano escolar 1998-1999.
# Variables: testscr (puntaje promedio), str (alumnos por profesor), el_pct
# (% de alumnos que aprenden ingles), meal_pct (% con subsidio de almuerzo),
# calw_pct (% que recibe asistencia publica), avginc (ingreso medio del distrito).

# --- Estructura de carpetas esperada ---
# Este script asume que existe una carpeta de proyecto con esta estructura:
#
#   4.MCO_estimacion_tamano-clase/
#     code/
#       01_class_size_ols.R   <- este archivo
#     input/
#       caschool.dta          <- datos de entrada
#     output/                 <- se crea sola al correr el script (tablas y graficos van aca)
#
# Si no la tenes armada: crea la carpeta "4.MCO_estimacion_tamano-clase" en tu compu,
# adentro crea "code/" e "input/", pone este script en code/ y el archivo .dta en input/.
# output/ no hace falta crearla, el script la crea sola.

# Reemplazar por la ruta a la carpeta "4.MCO_estimacion_tamano-clase" en TU computadora.
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/examples/4.MCO_estimacion_tamano-clase"
setwd(dir_proyecto) # fija la carpeta de trabajo: de aca en mas, "input/..." y "output/..." apuntan siempre ahi

rm(list = ls()) # borra todos los objetos que pudieran quedar de una sesion anterior, para partir de cero

# library(x) carga un paquete (una libreria de funciones adicionales) para poder usarlo.
# Hay que haberlo instalado antes una vez con install.packages("x").
library(tidyverse)    # incluye dplyr y ggplot2 (graficos)
library(haven)        # para leer archivos .dta (formato nativo de Stata)
library(sandwich)     # errores estandar robustos (HC1)
library(modelsummary) # arma tablas de regresiones

options("modelsummary_factory_latex" = "kableExtra")   # booktabs simple, sin tabularray/siunitx
options("modelsummary_format_numeric_latex" = "plain") # sin \num{}, no requiere siunitx

dir.create("output", showWarnings = FALSE) # crea la carpeta output/ si todavia no existe

# read_dta() lee un archivo de datos en formato .dta (el formato nativo de Stata).
d <- read_dta("input/caschool.dta")

cat("N distritos:", nrow(d), "\n")

# --- Estadisticas descriptivas: media, desvio estandar y percentiles ---
# sapply() aplica una funcion a cada elemento de una lista; aca, a cada variable.
# quantile(x, p) devuelve el percentil p de x (type = 2 es la definicion de percentil que usa Stata);
# c(...) arma un vector con varios valores.
desc_fun <- function(x) {
  c(mean(x), sd(x), quantile(x, c(0.10, 0.25, 0.50, 0.75, 0.90), type = 2))
}
desc <- t(sapply(d[, c("str", "testscr")], desc_fun)) # t() traspone: una fila por variable
desc <- data.frame(
  Variable = c("Estudiantes/profesor", "Nota en test"),
  desc,
  row.names = NULL
)
names(desc) <- c("Variable", "Media", "Desv. est.", "P10", "P25", "P50", "P75", "P90")
write.csv(desc, "output/desc_stats.csv", row.names = FALSE)

# knitr::kable() convierte un data.frame en una tabla LaTeX; booktabs = TRUE usa
# \toprule/\midrule/\bottomrule (el paquete booktabs ya esta cargado en el deck).
desc_tex <- knitr::kable(desc, format = "latex", booktabs = TRUE, digits = 1,
                         align = "lccccccc", linesep = "")
writeLines(as.character(desc_tex), "output/desc_stats.tex")

# --- Graficos: dispersion y recta de regresion ---
# aes() indica que variable va en cada eje; geom_point() dibuja los puntos;
# geom_smooth(method = "lm") superpone la recta de la regresion lineal simple.
p_scatter <- ggplot(d, aes(x = str, y = testscr)) +
  geom_point(color = "#1F77B4", size = 1.8, alpha = 0.7) +
  labs(x = "Ratio alumnos/profesor (str)", y = "Puntaje promedio de examen (testscr)") +
  theme_minimal(base_size = 13)
ggsave("output/scatter_str_testscr.png", p_scatter, width = 7, height = 4.5, dpi = 200)

p_ols <- p_scatter +
  geom_smooth(method = "lm", se = FALSE, color = "#D62728", linewidth = 0.8)
ggsave("output/ols_str_testscr.png", p_ols, width = 7, height = 4.5, dpi = 200)

# --- Regresiones ---
# lm(y ~ x1 + x2, data = ...) estima una regresion lineal (OLS) de y sobre x1 y x2.
m1 <- lm(testscr ~ str, data = d)
m2 <- lm(testscr ~ str + el_pct, data = d)
m3 <- lm(testscr ~ str + el_pct + meal_pct, data = d)
m4 <- lm(testscr ~ str + el_pct + calw_pct, data = d)
m5 <- lm(testscr ~ str + el_pct + meal_pct + calw_pct, data = d)

# Nombres de fila para las tablas
nombres <- c(
  "(Intercept)" = "Constante",
  "str"         = "Estudiantes/profesor",
  "el_pct"      = "\\% aprendices de inglés",
  "meal_pct"    = "\\% con subsidio de almuerzo",
  "calw_pct"    = "\\% con asistencia pública"
)

# Funcion que arma una tabla LaTeX (y su version data.frame) con errores robustos HC1.
# vcov = "HC1" pide errores estandar robustos a heterocedasticidad; es la version
# que Stata llama "robust". gof_map elige los estadisticos de ajuste a mostrar;
# escape = FALSE deja pasar el codigo LaTeX de los nombres (\\% y $R^2$) tal cual.
guardar_tabla <- function(modelos, archivo, gof, stars = FALSE, fmt = 3) {
  tex <- modelsummary(
    modelos, vcov = "HC1", output = "latex", stars = stars, fmt = fmt, escape = FALSE,
    coef_rename = nombres, gof_map = gof
  )
  # sub() quita el envoltorio \begin{table}...\end{table}: en las diapositivas la tabla va sola,
  # sin flotante, asi que se deja solo el entorno tabular.
  tex <- sub("^\\\\begin\\{table\\}\n\\\\centering\n", "", as.character(tex))
  tex <- sub("\n\\\\end\\{table\\}\\s*$", "", tex)
  writeLines(tex, paste0("output/", archivo, ".tex"))
  csv <- modelsummary(
    modelos, vcov = "HC1", output = "data.frame", stars = stars, fmt = fmt,
    coef_rename = nombres, gof_map = gof
  )
  write.csv(csv, paste0("output/", archivo, ".csv"), row.names = FALSE)
}

gof_una <- data.frame(
  raw   = c("nobs", "r.squared"),
  clean = c("N", "$R^2$"),
  fmt   = c(0, 3)
)
gof_cinco <- data.frame(
  raw   = c("nobs", "r.squared"),
  clean = c("N", "$R^2$"),
  fmt   = c(0, 2)
)

guardar_tabla(list("(1)" = m1), "reg_model1", gof_una)
guardar_tabla(list("(2)" = m2), "reg_model2", gof_una)
guardar_tabla(
  list("(1)" = m1, "(2)" = m2, "(3)" = m3, "(4)" = m4, "(5)" = m5),
  "reg_5models", gof_cinco, stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001), fmt = 2
)

# --- Sesgo de variable omitida: str y el_pct ---
# El coeficiente de str sin controlar por el_pct es beta1 + beta2 * Cov(str, el_pct) / Var(str)
# (en el limite). Aca se calcula el signo de cada pieza con los datos.
ovb <- data.frame(
  cor_str_elpct   = cor(d$str, d$el_pct),
  coef_str_m1     = unname(coef(m1)["str"]),
  coef_str_m2     = unname(coef(m2)["str"]),
  coef_elpct_m2   = unname(coef(m2)["el_pct"]),
  cambio_coef_str = unname(coef(m1)["str"] - coef(m2)["str"])
)
write.csv(ovb, "output/ovb_check.csv", row.names = FALSE)

# --- Datos del ejercicio de efectos marginales (modelo m2) ---
# Medias muestrales y coeficientes que se le dan a los estudiantes en la diapositiva.
info <- data.frame(
  cantidad = c("E(testscr)", "E(str)", "E(el_pct)", "beta_str", "beta_el_pct"),
  valor    = c(mean(d$testscr), mean(d$str), mean(d$el_pct), coef(m2)["str"], coef(m2)["el_pct"])
)
write.csv(info, "output/info_ejercicio.csv", row.names = FALSE)

cat("Coef. de str sin controles:", round(coef(m1)["str"], 2), "\n")
cat("Coef. de str controlando por el_pct:", round(coef(m2)["str"], 2), "\n")
cat("Correlacion entre str y el_pct:", round(cor(d$str, d$el_pct), 3), "\n")
cat("Listo. Tablas y graficos en output/.\n")
