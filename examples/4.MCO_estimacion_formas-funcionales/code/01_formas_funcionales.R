# Formas funcionales y efectos marginales: calcula las cantidades que se le dan
# a los estudiantes en los ejercicios de efectos marginales del deck
# 4.MCO_estimacion (medias muestrales y coeficientes de cada modelo).
#
# Genera (en output/):
#   - info_ejercicios.csv   una fila por cantidad: ejercicio, cantidad, valor
# y las imprime en la consola.
#
# Datos (todos de uso publico, ver README.md):
#   - wage2.dta               Wooldridge, salarios de hombres (935 obs.)
#   - hprice2.dta             Wooldridge, precios de viviendas en Boston (506 obs.)
#   - cps92_08.dta            Stock & Watson, CPS 1992 y 2008, edades 25-34 (15,316 obs.)
#   - cps_education_2004.rds  Stock & Watson, CPS marzo 2005, edades 29-30 (2,950 obs.)

# --- Estructura de carpetas esperada ---
# Este script asume que existe una carpeta de proyecto con esta estructura:
#
#   4.MCO_estimacion_formas-funcionales/
#     code/
#       01_formas_funcionales.R  <- este archivo
#     input/
#       wage2.dta, hprice2.dta, cps92_08.dta, cps_education_2004.rds  <- datos de entrada
#     output/                    <- se crea sola al correr el script
#
# Si no la tenes armada: crea la carpeta "4.MCO_estimacion_formas-funcionales" en tu compu,
# adentro crea "code/" e "input/", pone este script en code/ y los datos en input/.
# output/ no hace falta crearla, el script la crea sola.

# Reemplazar por la ruta a la carpeta "4.MCO_estimacion_formas-funcionales" en TU computadora.
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/examples/4.MCO_estimacion_formas-funcionales"
setwd(dir_proyecto) # fija la carpeta de trabajo: de aca en mas, "input/..." y "output/..." apuntan siempre ahi

rm(list = ls()) # borra todos los objetos que pudieran quedar de una sesion anterior, para partir de cero

# library(x) carga un paquete (una libreria de funciones adicionales) para poder usarlo.
# Hay que haberlo instalado antes una vez con install.packages("x").
library(haven) # para leer archivos .dta (formato nativo de Stata)

dir.create("output", showWarnings = FALSE) # crea la carpeta output/ si todavia no existe

# Lista donde se van acumulando las cantidades; cada elemento es una fila de la tabla final.
filas <- list()
# guardar() agrega una cantidad a la lista y la imprime en la consola.
guardar <- function(ejercicio, cantidad, valor) {
  filas[[length(filas) + 1]] <<- data.frame(ejercicio = ejercicio, cantidad = cantidad, valor = valor)
  cat(sprintf("%-28s %-16s %12.4f\n", ejercicio, cantidad, valor))
}

# --- Salarios, educacion, experiencia e IQ (WAGE2) ---
# lm(y ~ x1 + x2, data = ...) estima una regresion lineal (OLS).
# I(...) hace que R calcule la operacion (exper^2, educ*IQ) antes de estimar.
w <- read_dta("input/wage2.dta")
m_w <- lm(wage ~ educ + exper + I(exper^2) + IQ + I(educ * IQ), data = w)
guardar("wage2 nivel", "E(wage)", mean(w$wage))
guardar("wage2 nivel", "E(educ)", mean(w$educ))
guardar("wage2 nivel", "E(exper)", mean(w$exper))
guardar("wage2 nivel", "E(IQ)", mean(w$IQ))
guardar("wage2 nivel", "sd(IQ)", sd(w$IQ))
for (k in names(coef(m_w))[-1]) guardar("wage2 nivel", paste0("beta_", k), coef(m_w)[k])

m_lw <- lm(log(wage) ~ educ + exper + I(exper^2), data = w)
guardar("wage2 log-nivel", "beta_educ", coef(m_lw)["educ"])

# --- Precios de viviendas y contaminacion (HPRICE2) ---
h <- read_dta("input/hprice2.dta")
m_h <- lm(price ~ log(nox) + rooms + stratio, data = h)
m_hl <- lm(log(price) ~ log(nox) + rooms + stratio, data = h)
guardar("hprice2", "E(price)", mean(h$price))
guardar("hprice2", "E(nox)", mean(h$nox))
guardar("hprice2", "E(rooms)", mean(h$rooms))
guardar("hprice2", "E(stratio)", mean(h$stratio))
for (k in names(coef(m_h))[-1]) guardar("hprice2 nivel-log", paste0("beta_", k), coef(m_h)[k])
for (k in names(coef(m_hl))[-1]) guardar("hprice2 log-log", paste0("beta_", k), coef(m_hl)[k])
# Cambio porcentual exacto por una habitacion mas: (exp(beta) - 1) * 100
guardar("hprice2 cambio exacto", "pct_rooms", (exp(coef(m_hl)["rooms"]) - 1) * 100)

# --- Ingresos por hora: graduados universitarios y de secundaria (CPS 1992-2008) ---
c92 <- read_dta("input/cps92_08.dta")
# El archivo trae el ingreso de cada anio en dolares corrientes. Se expresa el de
# 1992 en dolares de 2008 multiplicando por el cociente de IPC (215.2 / 140.3).
c92$ahe_2008 <- ifelse(c92$year == 1992, c92$ahe * 215.2 / 140.3, c92$ahe)
c92$d2008 <- as.numeric(c92$year == 2008)

c08 <- c92[c92$year == 2008, ]
m_b <- lm(ahe ~ bachelor, data = c08)
guardar("cps 2008", "E(ahe)", mean(c08$ahe))
guardar("cps 2008", "beta_0", coef(m_b)["(Intercept)"])
guardar("cps 2008", "beta_bachelor", coef(m_b)["bachelor"])

m_bi <- lm(ahe_2008 ~ bachelor * d2008, data = c92)
guardar("cps 1992-2008 ($ 2008)", "E(ahe)", mean(c92$ahe_2008))
for (k in names(coef(m_bi))) guardar("cps 1992-2008 ($ 2008)", paste0("beta_", k), coef(m_bi)[k])

# --- Educacion, sexo y salarios (CPS marzo 2005) ---
e <- readRDS("input/cps_education_2004.rds")
e$female <- as.numeric(e$gender == "female")
m_e <- lm(log(earnings) ~ education * female, data = e)
for (k in names(coef(m_e))[-1]) guardar("cps 2004 educ x female", paste0("beta_", k), coef(m_e)[k])

tabla <- do.call(rbind, filas)
write.csv(tabla, "output/info_ejercicios.csv", row.names = FALSE)
cat("Listo. Resultados en output/info_ejercicios.csv.\n")
