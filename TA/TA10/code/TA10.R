# Ayudantia 10: variables instrumentales en R. Primera etapa, forma reducida y el
# estimador de VI como su cociente; 2SLS "a mano" (y por que sus errores estandar estan
# mal); ivreg con errores estandar robustos; VI con controles; muchas dummies con
# fixest; y un modelo sobreidentificado (varios instrumentos) con el test de relevancia
# conjunta, el test de Sargan y el test de Wu-Hausman.
#
# El script sigue el orden de los ejercicios del enunciado (TA10.pdf).
#
# Datos del ejercicio 1: Bedard y Deschenes (Journal of Human Resources, 2005), Censo de
# EE.UU. de 1980. Mujeres de 21 a 40 anios alguna vez casadas, con al menos un hijo.
# Variables: ingreso (ingreso del hogar por persona, en dolares), divorcio (1 si el primer
# matrimonio termino), nina (1 si el primer hijo es mujer), edad, edad_matr (edad al
# primer matrimonio), edad_hijo (edad al nacer el primer hijo), educ (anios de
# educacion), metro (1 si vive en un area metropolitana), estado (estado de residencia)
# y nacim (lugar de nacimiento).
#
# Datos del ejercicio 2: Pascali (Review of Economics and Statistics, 2016), municipios
# del centro y norte de Italia, 2002 y 2003. Variables: pib (valor agregado per capita,
# miles de euros), credito (credito privado / PIB, en %), jew (1 si hubo una comunidad
# judia en el siglo XV), jew_peq, jew_med, jew_gde (comunidad pequena, mediana o grande),
# provincia, anio y caracteristicas geograficas (alt_min, alt_max, alt_media, sismico,
# mar, cerca_mar, rio).

# --- Estructura de carpetas esperada ---
#
#   TA10/
#     code/
#       TA10.R           <- este archivo
#     input/
#       divorcio.rds     <- datos del ejercicio 1
#       bancos.rds       <- datos del ejercicio 2
#
# Si no la tenes armada: crea la carpeta "TA10" en tu compu, adentro crea "code/" e
# "input/", pone este script en code/ y los archivos .rds en input/.

# Reemplazar por la ruta a la carpeta "TA10" en TU computadora.
# En Windows la ruta se escribe con barras hacia adelante: "C:/Users/tu_nombre/Documents/TA10".
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/TA/TA10"
setwd(dir_proyecto) # fija la carpeta de trabajo: "input/..." apunta ahi

rm(list = ls()) # borra los objetos de una sesion anterior, para partir de cero

library(sandwich) # vcovHC: matrices de varianzas y covarianzas robustas
library(lmtest)   # coeftest: tabla de coeficientes con la matriz de varianzas que uno elija
library(car)      # linearHypothesis: tests de restricciones lineales (Wald / F)
library(ivreg)    # ivreg: estimacion por variables instrumentales y 2SLS
library(fixest)   # feols: MCO y VI con muchas dummies (efectos fijos), muy rapido


# =====================================================================
# 1. Divorcio y situacion economica de la mujer
# =====================================================================

dv <- readRDS("input/divorcio.rds")

# (a) Estadisticas descriptivas: media y desviacion estandar de cada variable.
nrow(dv)
round(sapply(dv[, c("ingreso", "divorcio", "nina", "edad", "educ")],
             function(x) c(media = mean(x), de = sd(x))), 3)

# (b) MCO de ingreso sobre divorcio, con errores estandar robustos (HC1).
mco <- lm(ingreso ~ divorcio, data = dv)
coeftest(mco, vcov = vcovHC(mco, type = "HC1"))

# (c) Primera etapa (endogena sobre el instrumento) y forma reducida (dependiente sobre
# el instrumento).
pe <- lm(divorcio ~ nina, data = dv)
coeftest(pe, vcov = vcovHC(pe, type = "HC1"))
fr <- lm(ingreso ~ nina, data = dv)
coeftest(fr, vcov = vcovHC(fr, type = "HC1"))

# El estimador de VI es el cociente entre los coeficientes del instrumento en la forma
# reducida y en la primera etapa. coef(m)["nina"] extrae ese coeficiente.
b_vi <- coef(fr)["nina"] / coef(pe)["nina"]
b_vi

# Con un instrumento binario, es el estimador de Wald: diferencia de medias de ingreso
# entre nina = 1 y nina = 0, dividida por la diferencia de medias de divorcio.
# tapply(x, g, mean) calcula la media de x en cada grupo de g.
m_y <- tapply(dv$ingreso, dv$nina, mean)
m_d <- tapply(dv$divorcio, dv$nina, mean)
(m_y["1"] - m_y["0"]) / (m_d["1"] - m_d["0"])

# Con 576 mil observaciones cada modelo de lm ocupa unos 140 MB. rm() borra los que ya
# no se usan, para no pasar el limite de memoria (1 GB en Posit Cloud gratuito).
rm(mco, fr)

# (d) 2SLS "a mano": MCO de ingreso sobre el valor predicho de la primera etapa.
dv$divorcio_hat <- fitted(pe)
mano <- lm(ingreso ~ divorcio_hat, data = dv)
coeftest(mano, vcov = vcovHC(mano, type = "HC1"))
# El coeficiente es el mismo, pero el error estandar NO: usa los residuos
# ingreso - b0 - b1 * divorcio_hat, en vez de ingreso - b0 - b1 * divorcio.

# ivreg: a la izquierda de "|" van los regresores, a la derecha los instrumentos.
# vcov. = sandwich calcula la matriz robusta HC0, la de "ivregress 2sls, robust" en
# Stata (es lo mismo que vcovHC(x, type = "HC0"), pero usa menos memoria).
vi <- ivreg(ingreso ~ divorcio | nina, data = dv)
summary(vi, vcov. = sandwich)

rm(pe, mano, vi)

# (e) Controles. I(edad^2) agrega el cuadrado de la edad. Con tantas observaciones, lm e
# ivreg con estos controles usan mucha memoria; feols (paquete fixest) estima lo mismo
# con mucha menos. vcov = "hetero" pide errores estandar robustos.
mco_c <- feols(ingreso ~ divorcio + edad + I(edad^2) + edad_matr + I(edad_matr^2) +
                 edad_hijo + I(edad_hijo^2) + educ + I(educ^2) + metro,
               data = dv, vcov = "hetero")
coeftable(mco_c)["divorcio", ]

# VI con feols. La formula tiene dos partes separadas por "|":
#   dependiente ~ exogenas | endogena ~ instrumento
# Las exogenas se usan automaticamente como sus propios instrumentos. El coeficiente de
# la endogena se llama "fit_divorcio".
vi_c <- feols(ingreso ~ edad + I(edad^2) + edad_matr + I(edad_matr^2) +
                edad_hijo + I(edad_hijo^2) + educ + I(educ^2) + metro |
                divorcio ~ nina,
              data = dv, vcov = "hetero")
coeftable(vi_c)["fit_divorcio", ]

# Primera etapa: summary(..., stage = 1). Test de relevancia, H0: el coeficiente de nina
# en la primera etapa es cero. Con un solo instrumento, F = t^2.
coeftable(summary(vi_c, stage = 1))["nina", ]
fitstat(vi_c, "ivwald1")
rm(mco_c, vi_c)

# (f) Dummies de estado de residencia y de lugar de nacimiento (unas 100 dummies).
# ivreg con factor(estado) + factor(nacim) tarda mas de un minuto y usa varios GB de
# memoria. feols las trata como efectos fijos y tarda unos segundos. La formula tiene
# tres partes separadas por "|":
#   dependiente ~ exogenas | efectos fijos | endogena ~ instrumento
vi_fe <- feols(ingreso ~ edad + I(edad^2) + edad_matr + I(edad_matr^2) +
                 edad_hijo + I(edad_hijo^2) + educ + I(educ^2) + metro |
                 estado + nacim | divorcio ~ nina,
               data = dv, vcov = "hetero")
coeftable(vi_fe)["fit_divorcio", ]
fitstat(vi_fe, "ivwald1") # F robusto de la primera etapa (H0: coeficiente de nina = 0)


# =====================================================================
# 2. Bancos y desarrollo: comunidades judias en el Renacimiento
# =====================================================================

bk <- readRDS("input/bancos.rds")
bk <- bk[!is.na(bk$credito), ] # 4 municipio-anio sin dato de credito
nrow(bk)

# (a) MCO con controles geograficos y dummies de provincia y anio. factor() convierte
# una variable en un conjunto de dummies (una por categoria, menos la omitida).
mco_b <- lm(pib ~ credito + alt_min + alt_max + alt_media + sismico + mar + cerca_mar +
              rio + factor(provincia) + factor(anio), data = bk)
coeftest(mco_b, vcov = vcovHC(mco_b, type = "HC1"))["credito", ]

# (b) Primera etapa con jew y test de relevancia.
pe_b <- lm(credito ~ jew + alt_min + alt_max + alt_media + sismico + mar + cerca_mar +
             rio + factor(provincia) + factor(anio), data = bk)
coeftest(pe_b, vcov = vcovHC(pe_b, type = "HC1"))["jew", ]
linearHypothesis(pe_b, "jew = 0", vcov. = vcovHC(pe_b, type = "HC1"), test = "F")

# (c) VI con jew como instrumento. En ivreg, las variables exogenas incluidas van a
# ambos lados de "|": son sus propios instrumentos. Si se omiten a la derecha, ivreg las
# trata como endogenas.
vi_b <- ivreg(pib ~ credito + alt_min + alt_max + alt_media + sismico + mar + cerca_mar +
                rio + factor(provincia) + factor(anio) |
                jew + alt_min + alt_max + alt_media + sismico + mar + cerca_mar +
                rio + factor(provincia) + factor(anio), data = bk)
coeftest(vi_b, vcov = vcovHC(vi_b, type = "HC0"))["credito", ]

# (d) Modelo sobreidentificado: tres instrumentos (tamano de la comunidad) para una
# variable endogena. Primera etapa y test de relevancia conjunta (H0: los tres
# coeficientes son cero).
pe_b3 <- lm(credito ~ jew_peq + jew_med + jew_gde + alt_min + alt_max + alt_media +
              sismico + mar + cerca_mar + rio + factor(provincia) + factor(anio), data = bk)
coeftest(pe_b3, vcov = vcovHC(pe_b3, type = "HC1"))[c("jew_peq", "jew_med", "jew_gde"), ]
linearHypothesis(pe_b3, c("jew_peq = 0", "jew_med = 0", "jew_gde = 0"),
                 vcov. = vcovHC(pe_b3, type = "HC1"), test = "F")

# 2SLS con los tres instrumentos. diagnostics = TRUE agrega tres tests al final:
# instrumentos debiles (F de la primera etapa), Wu-Hausman (endogeneidad de credito) y
# Sargan (sobreidentificacion: H0 los instrumentos son exogenos).
vi_b3 <- ivreg(pib ~ credito + alt_min + alt_max + alt_media + sismico + mar + cerca_mar +
                 rio + factor(provincia) + factor(anio) |
                 jew_peq + jew_med + jew_gde + alt_min + alt_max + alt_media + sismico +
                 mar + cerca_mar + rio + factor(provincia) + factor(anio), data = bk)
s3 <- summary(vi_b3, vcov. = function(x) vcovHC(x, type = "HC0"), diagnostics = TRUE)
s3$coefficients["credito", ]
s3$diagnostics
