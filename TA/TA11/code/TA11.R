# Ayudantia 11: contrastes de sobreidentificacion (Sargan) y de endogeneidad (Wu-Hausman)
# "a mano", con los datos de Pascali del ejercicio 2 de la Ayudantia 10.
#
# El script sigue el orden del ejercicio 4 del enunciado (TA11.pdf).
#
# Datos: Pascali (Review of Economics and Statistics, 2016), municipios del centro y norte
# de Italia, 2002 y 2003. Variables: pib (valor agregado per capita, miles de euros), credito
# (credito privado / PIB, en %), jew_peq, jew_med, jew_gde (1 si hubo una comunidad judia
# pequena, mediana o grande en el siglo XV), provincia, anio y caracteristicas geograficas
# (alt_min, alt_max, alt_media, sismico, mar, cerca_mar, rio).

# --- Estructura de carpetas esperada ---
#
#   TA11/
#     code/
#       TA11.R           <- este archivo
#     input/
#       bancos.rds       <- datos (los mismos de la Ayudantia 10)
#
# Si no la tenes armada: crea la carpeta "TA11" en tu compu, adentro crea "code/" e
# "input/", pone este script en code/ y bancos.rds en input/.

# Reemplazar por la ruta a la carpeta "TA11" en TU computadora.
# En Windows la ruta se escribe con barras hacia adelante: "C:/Users/tu_nombre/Documents/TA11".
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/TA/TA11"
setwd(dir_proyecto) # fija la carpeta de trabajo: "input/..." apunta ahi

rm(list = ls())

library(sandwich) # errores estandar robustos
library(lmtest)   # coeftest
library(car)      # linearHypothesis
library(ivreg)    # ivreg

bk <- readRDS("input/bancos.rds")
bk <- bk[!is.na(bk$credito), ] # 4 municipio-anio sin dato de credito
nrow(bk)

# Controles del modelo (exogenos incluidos), como en la Ayudantia 10.
ctrl <- "alt_min + alt_max + alt_media + sismico + mar + cerca_mar + rio +
         factor(provincia) + factor(anio)"

# =====================================================================
# 4. Sargan y Wu-Hausman a mano
# =====================================================================

# (a) 2SLS con los tres instrumentos y los tests de ivreg (los de la Ayudantia 10).
vi <- ivreg(as.formula(paste("pib ~ credito +", ctrl, "| jew_peq + jew_med + jew_gde +", ctrl)),
            data = bk)
coef(vi)["credito"]
diag_hc0 <- summary(vi, vcov. = function(x) vcovHC(x, type = "HC0"), diagnostics = TRUE)$diagnostics
diag_hom <- summary(vi, diagnostics = TRUE)$diagnostics
diag_hc0
diag_hom

# (b) Sargan a mano. Con L = 3 instrumentos y K = 1 endogena, hay L - K = 2 restricciones de
# sobreidentificacion. Se guardan los residuos de 2SLS, calculados con el credito
# observado (no con el predicho): resid() de ivreg ya lo hace. Luego se regresan en TODAS las
# exogenas (controles e instrumentos) y el estadistico es N * R^2 ~ chi2(L - K).
bk$u_hat <- resid(vi)
aux <- lm(as.formula(paste("u_hat ~ jew_peq + jew_med + jew_gde +", ctrl)), data = bk)
R2 <- summary(aux)$r.squared
sargan <- nrow(bk) * R2
c(R2 = R2, N = nrow(bk), sargan = sargan, p = pchisq(sargan, df = 2, lower.tail = FALSE))

# (c) Wu-Hausman a mano (funcion de control). Primera etapa con los tres instrumentos, se
# guardan los residuos v y se agregan a la ecuacion de interes estimada por MCO.
pe <- lm(as.formula(paste("credito ~ jew_peq + jew_med + jew_gde +", ctrl)), data = bk)
bk$v_hat <- resid(pe)
fc <- lm(as.formula(paste("pib ~ credito + v_hat +", ctrl)), data = bk)
# H0: coeficiente de v_hat = 0 (credito es exogena). Con errores robustos HC0, t^2 es el
# Wu-Hausman robusto de ivreg; con errores clasicos, el Wu-Hausman clasico.
rob <- coeftest(fc, vcov = vcovHC(fc, type = "HC0"))[c("credito", "v_hat"), ]
cla <- coeftest(fc)[c("credito", "v_hat"), ]
rob
rob["v_hat", "t value"]^2
cla["v_hat", "t value"]^2
# El coeficiente de credito en esta regresion es el de 2SLS (su error estandar no).
c(funcion_control = unname(coef(fc)["credito"]), dos_etapas = unname(coef(vi)["credito"]))
# Signo de v_hat frente a la diferencia MCO - VI:
mco <- lm(as.formula(paste("pib ~ credito +", ctrl)), data = bk)
c(mco = unname(coef(mco)["credito"]), vi = unname(coef(vi)["credito"]),
  v_hat = unname(coef(fc)["v_hat"]))
