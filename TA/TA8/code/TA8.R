# Ayudantia 8: inferencia con MCO en R. Errores estandar robustos, estadistico t,
# p-valor e intervalos de confianza; contrastes sobre una combinacion lineal de
# coeficientes (a mano con la matriz de varianzas, y con linearHypothesis); intervalo
# de confianza para una transformacion no lineal con el metodo delta (a mano, y con
# deltaMethod); y un primer vistazo al
# test de Wald para dos restricciones.
#
# El script sigue el orden de los ejercicios del enunciado (TA8.pdf). El ejercicio 3
# se resuelve en la pizarra; aca estan el 1 y el 2.
#
# Datos del ejercicio 1: Stock y Watson, 420 distritos escolares de California,
# 1998-1999. Variables: testscr (puntaje promedio), str (alumnos por profesor) y
# el_pct (porcentaje de alumnos que estan aprendiendo ingles).
#
# Datos del ejercicio 2: Meyer, Viscusi y Durbin (AER, 1995), trabajadores con un accidente laboral en
# Kentucky y Michigan, antes y despues de un aumento del tope de la compensacion
# semanal. Se usa solo Kentucky (ky = 1).
# Variables: durat (semanas de compensacion), ldurat (su logaritmo), afchnge (1 si el
# accidente fue despues del cambio), highearn (1 si es de altos ingresos) y
# afhigh = afchnge x highearn.

# --- Estructura de carpetas esperada ---
#
#   TA8/
#     code/
#       TA8.R            <- este archivo
#     input/
#       caschool.dta     <- datos del ejercicio 1
#       injury.dta       <- datos del ejercicio 2
#
# Si no la tenes armada: crea la carpeta "TA8" en tu compu, adentro crea "code/" e
# "input/", pone este script en code/ y el archivo .dta en input/.

# Reemplazar por la ruta a la carpeta "TA8" en TU computadora.
# En Windows la ruta se escribe con barras hacia adelante: "C:/Users/tu_nombre/Documents/TA8".
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/TA/TA8"
setwd(dir_proyecto) # fija la carpeta de trabajo: "input/..." apunta ahi

rm(list = ls()) # borra los objetos de una sesion anterior, para partir de cero

library(haven)    # para leer archivos .dta (formato de Stata)
library(sandwich) # errores estandar robustos (HC1)
library(lmtest)   # coeftest: tabla de coeficientes con la matriz de varianzas que uno elija
library(car)      # linearHypothesis (restricciones lineales) y deltaMethod (metodo delta)


# =====================================================================
# 1. Inferencia en R: tamano de clase y rendimiento academico
# =====================================================================

ca <- read_dta("input/caschool.dta")

# (a) summary() reporta errores estandar que suponen homoscedasticidad.
m_ca <- lm(testscr ~ str + el_pct, data = ca)
summary(m_ca)

# vcovHC(..., type = "HC1") estima Avar(b) = V/N robusta a heteroscedasticidad.
# coeftest() rehace la tabla de coeficientes con esa matriz.
avar_ca <- vcovHC(m_ca, type = "HC1")
avar_ca
coeftest(m_ca, vcov. = avar_ca)

# Errores estandar clasicos y robustos, lado a lado
cbind(clasico = sqrt(diag(vcov(m_ca))), robusto = sqrt(diag(avar_ca)))

# (b) H0: b_str = 0 contra la alternativa bilateral, a mano
b_str  <- coef(m_ca)["str"]
ee_str <- sqrt(avar_ca["str", "str"])
t_str  <- (b_str - 0) / ee_str
p_str  <- 2 * pnorm(-abs(t_str)) # p-valor con la normal, como en clase
c(t = unname(t_str), p = unname(p_str))
# coeftest() usa la t de Student con N - K grados de libertad: el t es el mismo y
# el p-valor casi igual.

# (c) Intervalo de confianza al 95%
z <- qnorm(0.975) # 1.96
c(inf = unname(b_str - z * ee_str), sup = unname(b_str + z * ee_str))
coefci(m_ca, vcov. = avar_ca) # usa la t de Student; con N = 420 casi no cambia

# (d) H0: b_str = 0 contra H1: b_str < 0 (una cola, la izquierda)
qnorm(0.05)   # valor critico: rechazamos si t < -1.645
pnorm(t_str)  # p-valor: area a la izquierda del t observado


# =====================================================================
# 2. Compensacion por accidentes laborales y duracion de la compensacion
# =====================================================================

d <- read_dta("input/injury.dta")
ky <- subset(d, ky == 1) # solo Kentucky
nrow(ky)

# (a) Modelo con las dos dummies y su interaccion
m <- lm(ldurat ~ afchnge + highearn + afhigh, data = ky)
avar <- vcovHC(m, type = "HC1") # estimador de Avar(b) = V/N, robusto
coeftest(m, vcov. = avar)       # coeficientes, errores estandar robustos, t y p-valores

b <- coef(m)

# Medias de ldurat en los cuatro grupos: el modelo es saturado, asi que coinciden
# con las combinaciones de coeficientes.
with(ky, tapply(ldurat, list(despues = afchnge, altos = highearn), mean))

# (b) Altos ingresos, despues menos antes: theta = b_afchnge + b_afhigh.
# A mano, con r = (0, 1, 0, 1)': theta = r'b y Avar(theta) = r' Avar(b) r.
r <- c(0, 1, 0, 1)
theta    <- sum(r * b)
ee_theta <- sqrt(as.numeric(t(r) %*% avar %*% r))
t_theta  <- theta / ee_theta
c(theta = theta, ee = ee_theta, t = t_theta, p = 2 * pnorm(-abs(t_theta)))

# Lo mismo escrito con las varianzas y la covarianza
sqrt(avar["afchnge", "afchnge"] + avar["afhigh", "afhigh"] + 2 * avar["afchnge", "afhigh"])
# Si se olvida la covarianza, el error estandar sale mal:
sqrt(avar["afchnge", "afchnge"] + avar["afhigh", "afhigh"])

# Con un comando: linearHypothesis. Reporta W = t^2 (test = "Chisq") y su p-valor.
linearHypothesis(m, "afchnge + afhigh = 0", vcov. = avar, test = "Chisq")
t_theta^2

# (c) Cambio porcentual exacto, 100 (exp(theta) - 1), con su intervalo al 95%.
# Metodo delta a mano: g(theta) = 100 (exp(theta) - 1), g'(theta) = 100 exp(theta).
g      <- 100 * (exp(theta) - 1)
ee_g   <- 100 * exp(theta) * ee_theta
c(g = g, ee = ee_g, inf = g - 1.96 * ee_g, sup = g + 1.96 * ee_g)
# Con un comando: deltaMethod (paquete car) recibe la funcion de los coeficientes
# escrita con sus nombres y la matriz de varianzas.
deltaMethod(m, "100 * (exp(afchnge + afhigh) - 1)", vcov. = avar)

# (d) Adelanto de la clase del lunes: H0: el cambio de tope no afecto a ningun grupo,
# b_afchnge = 0 (bajos ingresos) y b_afchnge + b_afhigh = 0 (altos ingresos).
# Dos restricciones: R' tiene una fila por restriccion.
Rt <- rbind(c(0, 1, 0, 0),
            c(0, 1, 0, 1))
dif <- Rt %*% b
W <- as.numeric(t(dif) %*% solve(Rt %*% avar %*% t(Rt)) %*% dif)
c(W = W, p = pchisq(W, df = 2, lower.tail = FALSE), valor_critico = qchisq(0.95, df = 2))

linearHypothesis(m, c("afchnge = 0", "afchnge + afhigh = 0"), vcov. = avar, test = "Chisq")

# Los dos estadisticos t por separado, para comparar
t_bajos <- b["afchnge"] / sqrt(avar["afchnge", "afchnge"])
c(t_bajos = unname(t_bajos), t_altos = t_theta)
