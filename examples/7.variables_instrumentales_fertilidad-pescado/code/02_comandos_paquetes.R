# Variables instrumentales con paquetes de R: los comandos de las laminas "En R: ..." del
# deck 7.variables_instrumentales, en el mismo orden que en la clase, con los datos de
# Angrist y Evans (AER 1998) y de la demanda de pescado (Graddy, RAND 1995).
# El script 01_variables_instrumentales.R calcula lo mismo "a mano"; este muestra como
# hacerlo con los paquetes que se usan en la practica.
#
# Paquetes (instalar una sola vez):
#   install.packages(c("sandwich", "lmtest", "car", "ivreg", "ivDiag"))

# Reemplazar por la ruta a la carpeta "7.variables_instrumentales_fertilidad-pescado" en TU computadora.
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/examples/7.variables_instrumentales_fertilidad-pescado"
setwd(dir_proyecto) # fija la carpeta de trabajo: de aca en mas, "input/..." apunta siempre ahi

rm(list = ls()) # borra todos los objetos que pudieran quedar de una sesion anterior

library(sandwich) # vcovHC(): matrices de varianzas y covarianzas robustas
library(lmtest)   # coeftest(): tabla de coeficientes con la matriz que le pasemos
library(car)      # linearHypothesis(): tests de restricciones lineales (Wald / F)
library(ivreg)    # ivreg(): estimacion por 2SLS
library(ivDiag)   # eff_F() y AR_test(): F efectivo e intervalo de Anderson-Rubin

ae   <- readRDS("input/angrist_evans.rds")
fish <- readRDS("input/fish.rds")

# --- Modelo simple (Angrist y Evans): MCO, primera etapa y 2SLS ---
# MCO con errores estandar robustos (HC1)
m <- lm(hourswm ~ morekids, data = ae)
print(coeftest(m, vcov = vcovHC(m, type = "HC1")))
# Primera etapa y forma reducida: igual, con morekids o hourswm ~ samesex
m1 <- lm(morekids ~ samesex, data = ae)
print(coeftest(m1, vcov = vcovHC(m1, type = "HC1")))
# 2SLS: variable endogena | instrumento. vcov. recibe una funcion que calcula la matriz
# robusta; HC0 es la de "ivregress 2sls, robust" en Stata.
iv <- ivreg(hourswm ~ morekids | samesex, data = ae)
print(summary(iv, vcov. = function(x) vcovHC(x, type = "HC0")))

# --- F efectivo e intervalo de Anderson-Rubin (Angrist y Evans) ---
# Estas funciones reciben los nombres de las variables como texto, entre comillas.
print(eff_F(data = ae, Y = "hourswm", D = "morekids", Z = "samesex"))
ar_ae <- AR_test(data = ae, Y = "hourswm", D = "morekids", Z = "samesex", controls = NULL)
print(ar_ae$ci.print)

# --- 2SLS con controles y varios instrumentos (Angrist y Evans) ---
# A la izquierda de "|": endogena y exogenas incluidas. A la derecha: instrumentos y
# exogenas incluidas (las exogenas incluidas son sus propios instrumentos).
iv_c <- ivreg(hourswm ~ morekids + agem1 + agefstm + boy1st + blackm + hispm + othracem |
                boys2 + girls2 + agem1 + agefstm + boy1st + blackm + hispm + othracem, data = ae)
print(summary(iv_c, vcov. = function(x) vcovHC(x, type = "HC0")))

# --- Test de relevancia de los instrumentos (Graddy) ---
m1_f <- lm(lavgprc ~ speed2 + speed3 + wave2 + wave3 + mon + tues + wed + thurs, data = fish)
print(linearHypothesis(m1_f, c("speed2 = 0", "speed3 = 0", "wave2 = 0", "wave3 = 0"),
                       vcov. = vcovHC(m1_f, type = "HC1"), test = "F"))
print(linearHypothesis(m1_f, c("speed2 = 0", "speed3 = 0"),
                       vcov. = vcovHC(m1_f, type = "HC1"), test = "F"))

# --- Anderson-Rubin con controles (Graddy): cuatro instrumentos y la alternativa con dos ---
# AR_test descuenta los controles antes de corregir por grados de libertad: da intervalos
# algo distintos de los de la clase, [-1.679, -0.168] y [-1.859, -0.045].
ar_f4 <- AR_test(data = fish, Y = "ltotqty", D = "lavgprc",
                 Z = c("speed2", "speed3", "wave2", "wave3"),
                 controls = c("mon", "tues", "wed", "thurs"))
print(ar_f4$ci.print)
ar_f2 <- AR_test(data = fish, Y = "ltotqty", D = "lavgprc", Z = c("wave2", "wave3"),
                 controls = c("mon", "tues", "wed", "thurs"))
print(ar_f2$ci.print)

# --- 2SLS, tests de Sargan y de Wu-Hausman (Graddy, cuatro instrumentos) ---
iv_f <- ivreg(ltotqty ~ lavgprc + mon + tues + wed + thurs |
                speed2 + speed3 + wave2 + wave3 + mon + tues + wed + thurs, data = fish)
print(summary(iv_f, vcov. = function(x) vcovHC(x, type = "HC0"), diagnostics = TRUE))
