# Ayudantia 6: formas funcionales en la regresion por MCO. Interacciones entre una
# dummy y una variable continua, un termino cuadratico y efectos marginales, con
# los mismos datos de salarios de la Ayudantia 5 (CPS de EEUU).
#
# El script sigue el orden de los ejercicios del enunciado (TA6.pdf). Los
# ejercicios 1 y 2 se resuelven en la pizarra; aca estan el 3 y el 4.
#
# Genera (en output/):
#   - reg_interaccion.tex/.csv   regresiones por sexo y con interaccion (ejercicio 3)
#   - reg_edad.tex/.csv          modelo con edad y edad al cuadrado (ejercicio 3(d))
#   - simulacion_se.csv          errores estandar clasicos, robustos y de Monte Carlo (ejercicio 4)
#
# Datos: Current Population Survey, marzo de 2005 (Stock & Watson, cap. 8).
# 61,395 trabajadores de tiempo completo de entre 21 y 64 anos.
# Variables: ahe (salario promedio por hora en 2004, en dolares), yrseduc (anos de
# educacion), female (1 si es mujer), age (edad), y northeast, midwest, south,
# west (dummies de region).

# --- Estructura de carpetas esperada ---
#
#   TA6/
#     code/
#       TA6.R            <- este archivo
#     input/
#       ch8_cps.dta      <- datos de entrada (el mismo archivo de la Ayudantia 5)
#     output/            <- se crea sola al correr el script
#
# Si no la tenes armada: crea la carpeta "TA6" en tu compu, adentro crea "code/" e
# "input/", pone este script en code/ y el archivo .dta en input/.

# Reemplazar por la ruta a la carpeta "TA6" en TU computadora.
# En Windows la ruta se escribe con barras hacia adelante: "C:/Users/tu_nombre/Documents/TA6".
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/TA/TA6"
setwd(dir_proyecto) # fija la carpeta de trabajo: "input/..." y "output/..." apuntan ahi

rm(list = ls()) # borra los objetos de una sesion anterior, para partir de cero

library(tidyverse)    # dplyr (manejo de datos) y ggplot2 (graficos)
library(haven)        # para leer archivos .dta (formato de Stata)
library(sandwich)     # errores estandar robustos (HC1)
library(modelsummary) # tablas de regresiones

options("modelsummary_factory_latex" = "kableExtra")   # tabla LaTeX simple, con booktabs
options("modelsummary_format_numeric_latex" = "plain") # sin \num{}, no requiere siunitx

dir.create("output", showWarnings = FALSE)

d <- read_dta("input/ch8_cps.dta") %>%
  mutate(lahe = log(ahe))

# Estadisticos de ajuste al pie de las tablas
gof <- data.frame(
  raw   = c("nobs", "r.squared"),
  clean = c("N", "R2"),
  fmt   = c(0, 3)
)


# =====================================================================
# 3. Interaccion entre sexo y educacion
# =====================================================================

# (a) female * yrseduc incluye female, yrseduc y su producto female:yrseduc.
m_int <- lm(lahe ~ female * yrseduc, data = d)
coef(m_int)

b <- coef(m_int)
# Retorno de un ano de educacion para hombres (female = 0) y para mujeres (female = 1)
retorno_hombres <- b["yrseduc"]
retorno_mujeres <- b["yrseduc"] + b["female:yrseduc"]
c(hombres = unname(retorno_hombres), mujeres = unname(retorno_mujeres))

# (b) Las mismas rectas, estimadas por separado para cada sexo
m_hombres <- lm(lahe ~ yrseduc, data = filter(d, female == 0))
m_mujeres <- lm(lahe ~ yrseduc, data = filter(d, female == 1))
coef(m_hombres)
coef(m_mujeres)
# Constante de las mujeres en el modelo con interaccion: b0 + b_female
b["(Intercept)"] + b["female"]

nombres <- c(
  "(Intercept)"    = "Constante",
  "female"         = "Mujer",
  "yrseduc"        = "Años de educación",
  "female:yrseduc" = "Mujer × años de educación",
  "age"            = "Edad",
  "edad2"          = "Edad² / 100"
)
modelos_int <- list("Hombres" = m_hombres, "Mujeres" = m_mujeres, "Interacción" = m_int)
modelsummary(modelos_int, vcov = "HC1", fmt = 4, coef_rename = nombres, gof_map = gof)
modelsummary(modelos_int, vcov = "HC1", fmt = 4, coef_rename = nombres, gof_map = gof,
             output = "output/reg_interaccion.tex")
write.csv(modelsummary(modelos_int, vcov = "HC1", fmt = 4, coef_rename = nombres,
                       gof_map = gof, output = "data.frame"),
          "output/reg_interaccion.csv", row.names = FALSE)

# (c) Brecha de salarios mujer - hombre segun anos de educacion:
#     b_female + b_interaccion * yrseduc  (en log puntos)
brecha <- tibble(yrseduc = c(12, 16)) %>%
  mutate(
    brecha_log  = b["female"] + b["female:yrseduc"] * yrseduc,
    brecha_pct  = 100 * (exp(brecha_log) - 1) # cambio exacto, en %
  )
brecha


# (d) Edad y edad al cuadrado.
# Edad al cuadrado dividida por 100: el coeficiente queda 100 veces mas grande y
# se lee con cuatro decimales (ver ejercicio 1).
d <- d %>% mutate(edad2 = age^2 / 100)

m_cuad <- lm(lahe ~ female * yrseduc + age + edad2, data = d)

modelos_edad <- list("(1)" = m_int, "(2)" = m_cuad)
modelsummary(modelos_edad, vcov = "HC1", fmt = 4, coef_rename = nombres, gof_map = gof)
modelsummary(modelos_edad, vcov = "HC1", fmt = 4, coef_rename = nombres, gof_map = gof,
             output = "output/reg_edad.tex")
write.csv(modelsummary(modelos_edad, vcov = "HC1", fmt = 4, coef_rename = nombres,
                       gof_map = gof, output = "data.frame"),
          "output/reg_edad.csv", row.names = FALSE)

# Efecto marginal de la edad: d lahe / d age = b_age + 2 * b_edad2 * age / 100
bc <- coef(m_cuad)
efecto_edad <- function(edad) bc["age"] + 2 * bc["edad2"] * edad / 100
c(a_los_30 = unname(efecto_edad(30)), a_los_50 = unname(efecto_edad(50)))

# Edad en la que el perfil alcanza su maximo: b_age + 2 * b_edad2 * age / 100 = 0
edad_max <- -100 * bc["age"] / (2 * bc["edad2"])
unname(edad_max)


# =====================================================================
# 4. Errores estandar con heteroscedasticidad
# =====================================================================

# Modelo: y = 1 + 2 x + u, con x ~ Uniforme(0, 2) y u = x^2 * e, e ~ Normal(0, 1).
# La varianza de u crece con x: el error es heteroscedastico.
set.seed(2026)
N <- 200
R <- 2000
resultados <- matrix(NA, R, 3, dimnames = list(NULL, c("b1", "se_clasico", "se_robusto")))
for (r in 1:R) {
  x <- runif(N, 0, 2)
  y <- 1 + 2 * x + x^2 * rnorm(N)
  m <- lm(y ~ x)
  resultados[r, ] <- c(coef(m)[2],
                       sqrt(vcov(m)[2, 2]),
                       sqrt(vcovHC(m, type = "HC1")[2, 2]))
}
sim <- tibble(
  media_b1        = mean(resultados[, "b1"]),
  de_b1_montecarlo = sd(resultados[, "b1"]),
  media_se_clasico = mean(resultados[, "se_clasico"]),
  media_se_robusto = mean(resultados[, "se_robusto"])
)
sim
write.csv(sim, "output/simulacion_se.csv", row.names = FALSE)

cat("Retorno educacion hombres:", round(retorno_hombres, 4),
    " mujeres:", round(retorno_mujeres, 4), "\n")
cat("Edad del maximo del perfil:", round(edad_max, 1), "\n")
cat("Listo. Tablas y graficos en output/.\n")
