# Tamano de clase y rendimiento academico: reproduce la figura de la slide
# "Ejemplo: Tamano de clase y notas" del deck 2.esperanza_condicional --
# grafico de dispersion entre el ratio alumnos/profesor (str) y el puntaje
# de examen (testscr) de un distrito escolar, con la recta de regresion
# lineal simple superpuesta.
#
# Datos: "California Test Score Data Set" (Stock & Watson), 420 distritos
# escolares de California, ano escolar 1998-1999. Es el dataset clasico del
# libro de texto (tambien conocido como "CASchools"), de uso publico y libre.
# Variables usadas: str (student-teacher ratio), testscr (puntaje promedio
# del distrito).

# --- Estructura de carpetas esperada ---
# Este script asume que existe una carpeta de proyecto con esta estructura:
#
#   2.esperanza_condicional_tamano-clase/
#     code/
#       01_class_size.R   <- este archivo
#     input/
#       caschool.dta        <- datos de entrada
#     output/                <- se crea sola al correr el script (el grafico va aca)
#
# Si no la tenes armada: crea la carpeta "2.esperanza_condicional_tamano-clase"
# en tu compu, adentro crea "code/" e "input/", pone este script en code/ y el
# archivo .dta en input/. output/ no hace falta crearla, el script la crea sola.

# Reemplazar por la ruta a la carpeta "2.esperanza_condicional_tamano-clase" en TU computadora.
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/examples/2.esperanza_condicional_tamano-clase"
setwd(dir_proyecto) # fija la carpeta de trabajo: de aca en mas, "input/..." y "output/..." apuntan siempre ahi

rm(list = ls()) # borra todos los objetos que pudieran quedar de una sesion anterior, para partir de cero

# library(x) carga un paquete (una libreria de funciones adicionales) para poder usarlo.
# Hay que haberlo instalado antes una vez con install.packages("x").
library(tidyverse) # incluye dplyr y ggplot2 (graficos)
library(haven)     # para leer archivos .dta (formato nativo de Stata)

dir.create("output", showWarnings = FALSE) # crea la carpeta output/ si todavia no existe

# read_dta() lee un archivo de datos en formato .dta (el formato nativo de Stata).
d <- read_dta("input/caschool.dta")

cat("N distritos:", nrow(d), "\n")

# lm(y ~ x, data=...) estima una regresion lineal (OLS) de y sobre x.
reg <- lm(testscr ~ str, data = d)

# La descripcion de la muestra ya va en el \caption{} de LaTeX debajo de la
# figura en la slide, asi que no se repite tambien adentro del grafico (por
# eso no hay title/subtitle aca, solo los nombres de los ejes).
p <- ggplot(d, aes(x = str, y = testscr)) +
  geom_point(color = "#1F77B4", size = 1.8, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#D62728", linewidth = 0.8) +
  labs(x = "Ratio alumnos/profesor (str)", y = "Puntaje promedio de examen (testscr)") +
  theme_minimal(base_size = 13)
ggsave("output/test_score_class_size.png", p, width = 7, height = 4.5, dpi = 200)

cat("Listo. Grafico en output/.\n")
