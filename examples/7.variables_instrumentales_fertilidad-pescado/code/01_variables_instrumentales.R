# Variables instrumentales y minimos cuadrados en dos etapas (2SLS), para el deck
# 7.variables_instrumentales.
#
# Genera (en output/), cada tabla en .tex y .csv:
#   Fertilidad y oferta laboral (Angrist y Evans, AER 1998), modelo simple:
#   - ae_desc               estadisticas descriptivas
#   - ae_mco                hourswm ~ morekids (MCO)
#   - ae_primera            primera etapa: morekids ~ samesex
#   - ae_reducida           forma reducida: hourswm ~ samesex
#   - ae_2sls_mano          2SLS "a mano": MCO de hourswm sobre la prediccion de la primera etapa
#   - ae_2sls               2SLS: hourswm ~ morekids, instrumento samesex
#   - ae_wald               medias de hourswm y morekids segun samesex (estimador de Wald)
#   - ae_ar                 IC 95% de 2SLS y de Anderson-Rubin para el efecto de morekids
#   Fertilidad y oferta laboral, modelo con controles y dos instrumentos (boys2, girls2):
#   - aeg_mco               MCO con controles
#   - aeg_primera           primera etapa con controles
#   - aeg_iv_pred           IV usando la prediccion de la primera etapa como instrumento
#   - aeg_dos_mco           dos veces MCO (segunda etapa "a mano")
#   - aeg_2sls              2SLS con boys2 y girls2
#   Demanda de pescado en el mercado de Fulton (Graddy, RAND 1995):
#   - fish_mco              MCO: ltotqty ~ lavgprc + dias de la semana
#   - fish_primera4         primera etapa con speed2, speed3, wave2, wave3
#   - fish_relevancia       tests de relevancia de los instrumentos
#   - fish_primera2         primera etapa con wave2 y wave3
#   - fish_2sls             2SLS con los cuatro instrumentos (especificacion base)
#   - fish_2sls_w23         2SLS con wave2 y wave3 (alternativa)
#   - fish_overid_mano      regresion de los residuos de 2SLS (cuatro instrumentos) sobre todas las exogenas
#   - fish_overid           tests de sobreidentificacion (Sargan y Hansen), con cuatro y con dos instrumentos
#   - fish_2sls_wave2       2SLS solo con wave2 (exactamente identificado)
#   - fish_2sls_wave3       2SLS solo con wave3 (exactamente identificado)
#   - fish_cf               MCO controlando por los residuos de la primera etapa con cuatro instrumentos (funcion de control)
#   - fish_ar               IC 95% de 2SLS y de Anderson-Rubin para el efecto de lavgprc, con cuatro y con dos instrumentos
#
# Tablas -> codigo LaTeX (.tex), para \input{} directo en el deck.
#
# Errores estandar robustos a heteroscedasticidad:
#   - MCO: HC1 (la correccion N/(N-K) que usa Stata con "reg, robust"), valores criticos de la t.
#   - 2SLS: la formula de las diapositivas, Avar = (1/N) B^(-1) C B^(-1), sin correccion de
#     grados de libertad (lo mismo que Stata con "ivregress 2sls, robust"), valores criticos de la normal.
#
# Datos (de uso publico, ver README.md):
#   - angrist_evans.rds  Censo de EE.UU. 1980: 262,793 mujeres casadas de 21-35 anios con al menos 2 hijos
#   - fish.rds           97 dias de ventas de merluza en el mercado de Fulton, Nueva York, dic. 1991-may. 1992

# --- Estructura de carpetas esperada ---
# Este script asume que existe una carpeta de proyecto con esta estructura:
#
#   7.variables_instrumentales_fertilidad-pescado/
#     code/
#       01_variables_instrumentales.R   <- este archivo
#     input/
#       angrist_evans.rds, fish.rds     <- datos de entrada
#     output/                           <- se crea sola al correr el script
#
# Si no la tenes armada: crea la carpeta "7.variables_instrumentales_fertilidad-pescado" en tu compu,
# adentro crea "code/" e "input/", pone este script en code/ y los datos en input/.

# Reemplazar por la ruta a la carpeta "7.variables_instrumentales_fertilidad-pescado" en TU computadora.
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/examples/7.variables_instrumentales_fertilidad-pescado"
setwd(dir_proyecto) # fija la carpeta de trabajo: de aca en mas, "input/..." y "output/..." apuntan siempre ahi

rm(list = ls()) # borra todos los objetos que pudieran quedar de una sesion anterior, para partir de cero

# library(x) carga un paquete (una libreria de funciones adicionales) para poder usarlo.
# Hay que haberlo instalado antes una vez con install.packages("x").
library(sandwich) # matriz de varianzas y covarianzas robusta (HC1) para MCO

dir.create("output", showWarnings = FALSE) # crea la carpeta output/ si todavia no existe

# --- Funciones auxiliares para escribir tablas ---

# a_latex() convierte un data.frame en una tabla LaTeX con \toprule/\midrule/\bottomrule
# (el paquete booktabs ya esta cargado en el deck). Si se pasa una nota, se agrega
# como ultima fila de la tabla. Las columnas cuyo nombre empieza con "p-valor" se
# muestran con tres decimales, y los p-valores menores a 0.001 como "<0.001".
a_latex <- function(tabla, archivo, digitos, alineacion, nota = NULL) {
  # Version .csv: numeros sin redondear y sin los escapes de LaTeX "\%" y "\_".
  sin_escapes <- function(x) gsub("\\\\([%_])", "\\1", x)
  csv <- tabla
  names(csv) <- sin_escapes(names(csv))
  csv[] <- lapply(csv, function(x) if (is.character(x)) sin_escapes(x) else x)
  write.csv(csv, paste0("output/", archivo, ".csv"), row.names = FALSE)

  for (col in grep("^p-valor", names(tabla))) {
    tabla[[col]] <- ifelse(tabla[[col]] < 0.001, "$<0.001$", sprintf("%.3f", tabla[[col]]))
  }
  # Los estadisticos t y z se muestran con dos decimales.
  digitos <- ifelse(names(tabla) %in% c("t", "z"), 2, digitos)
  tex <- knitr::kable(tabla, format = "latex", booktabs = TRUE, digits = digitos,
                      align = alineacion, linesep = "", escape = FALSE, row.names = FALSE,
                      format.args = list(scientific = FALSE))
  tex <- as.character(tex)
  if (!is.null(nota)) {
    fila_nota <- paste0("\\midrule\n\\multicolumn{", ncol(tabla), "}{l}{", nota, "}\\\\\n\\bottomrule")
    tex <- sub("\\bottomrule", fila_nota, tex, fixed = TRUE) # fixed = TRUE: busca y reemplaza el texto tal cual
  }
  writeLines(tex, paste0("output/", archivo, ".tex"))
}

# N con separador de miles para las notas de las tablas (por ejemplo, 262{,}793 en LaTeX).
n_tex <- function(n) format(n, big.mark = "{,}")

# tabla_coef() arma la tabla de una regresion a partir de los coeficientes b y su matriz
# de varianzas y covarianzas V: coeficiente, error estandar, estadistico, p-valor e IC 95%.
# Con gl = grados de libertad usa la t de Student (como lm y "reg" de Stata); con gl = NULL
# usa la normal (como "ivregress" de Stata), y la columna del estadistico se llama z.
tabla_coef <- function(b, V, nombres, gl = NULL) {
  ee <- sqrt(diag(V))
  est <- b / ee
  if (is.null(gl)) {
    p <- 2 * pnorm(-abs(est)); c <- qnorm(0.975); nombre_est <- "z"
  } else {
    p <- 2 * pt(-abs(est), gl); c <- qt(0.975, gl); nombre_est <- "t"
  }
  tab <- data.frame(Variable = nombres[names(b)], Coef. = b, `Error est.` = ee,
                    est = est, `p-valor` = p,
                    `IC 95\\% inf.` = b - c * ee, `IC 95\\% sup.` = b + c * ee,
                    check.names = FALSE)
  names(tab)[4] <- nombre_est
  # la constante al final, como en Stata
  rbind(tab[names(b) != "(Intercept)", ], tab[names(b) == "(Intercept)", ])
}

# mco() estima por MCO con lm() y devuelve el modelo, la tabla y la matriz robusta HC1.
mco <- function(formula, datos, nombres) {
  m <- lm(formula, data = datos) # lm(y ~ x1 + x2, data = ...) estima una regresion lineal por MCO
  V <- vcovHC(m, type = "HC1")
  list(modelo = m, V = V, tabla = tabla_coef(coef(m), V, nombres, gl = df.residual(m)))
}

# test_f() contrasta que un grupo de coeficientes es cero con la matriz robusta:
# W = b' V^(-1) b y F = W / q (lo que reporta "test" en Stata despues de "reg, robust").
test_f <- function(res, variables) {
  b <- coef(res$modelo)[variables]
  W <- as.numeric(t(b) %*% solve(res$V[variables, variables, drop = FALSE]) %*% b)
  q <- length(variables)
  c(F = W / q, q = q, gl = df.residual(res$modelo),
    p = pf(W / q, q, df.residual(res$modelo), lower.tail = FALSE))
}

# tsls() calcula el estimador 2SLS con las formulas de las diapositivas:
#   b = [X'Z (Z'Z)^(-1) Z'X]^(-1) X'Z (Z'Z)^(-1) Z'y
#   Avar(b) = (1/N) B^(-1) C B^(-1), con
#   B = (X'Z/N) (Z'Z/N)^(-1) (Z'X/N)
#   C = (X'Z/N) (Z'Z/N)^(-1) (sum u_i^2 z_i z_i' / N) (Z'Z/N)^(-1) (Z'X/N)
# X tiene las variables explicativas (con la constante) y Z todas las variables exogenas:
# las exogenas incluidas en X y los instrumentos excluidos.
tsls <- function(y, X, Z) {
  N   <- nrow(X)
  Szz <- crossprod(Z) / N      # crossprod(A, B) = t(A) %*% B; crossprod(A) = t(A) %*% A
  Sxz <- crossprod(X, Z) / N
  Szy <- crossprod(Z, y) / N
  P   <- Sxz %*% solve(Szz)
  B   <- P %*% t(Sxz)
  b   <- as.numeric(solve(B, P %*% Szy))
  names(b) <- colnames(X)
  u   <- as.numeric(y - X %*% b)                 # residuos con x (no con su prediccion)
  Szzu <- crossprod(Z * u) / N                   # (1/N) sum u_i^2 z_i z_i'
  C   <- P %*% Szzu %*% t(P)
  V   <- solve(B) %*% C %*% solve(B) / N
  dimnames(V) <- list(names(b), names(b))
  list(b = b, V = V, u = u)
}

# ic_ar() calcula el intervalo de confianza de Anderson-Rubin al 95% para el coeficiente
# de la variable endogena x. Para cada valor b de la grilla:
#   1. regresa y - b*x sobre W (las exogenas incluidas y los instrumentos), por MCO,
#   2. hace el test F robusto (HC1) de que los coeficientes de los instrumentos son cero,
#   3. b queda en el intervalo si el test no rechaza al 5%.
# Para no correr una regresion por cada b, se usa que la regresion es lineal: el
# coeficiente de y - b*x es (coef. de y) - b*(coef. de x), y sus residuos son
# (residuos de y) - b*(residuos de x). Asi basta con regresar y y x una sola vez;
# el resultado es exactamente el mismo que correr la regresion para cada b.
ic_ar <- function(y, x, W, instrumentos, grilla) {
  N <- nrow(W); K <- ncol(W); q <- length(instrumentos)
  A  <- solve(crossprod(W))
  gy <- A %*% crossprod(W, y); ey <- as.numeric(y - W %*% gy)   # regresion de y sobre W
  gx <- A %*% crossprod(W, x); ex <- as.numeric(x - W %*% gx)   # regresion de x sobre W
  j  <- match(instrumentos, colnames(W))
  # partes de la matriz robusta: sum e_i^2 w_i w_i' con e_i = ey_i - b ex_i
  Myy <- crossprod(W * ey); Mxy <- crossprod(W * ey, W * ex); Mxx <- crossprod(W * ex)
  F_ar <- sapply(grilla, function(b) {
    g <- (gy - b * gx)[j]
    V <- N / (N - K) * A %*% (Myy - b * (Mxy + t(Mxy)) + b^2 * Mxx) %*% A # HC1
    as.numeric(t(g) %*% solve(V[j, j, drop = FALSE]) %*% g) / q
  })
  acepta <- F_ar <= qf(0.95, q, N - K)
  # el intervalo es [min, max] de los b aceptados, si son contiguos y no tocan los bordes de la grilla
  stopifnot(any(acepta), !acepta[1], !acepta[length(acepta)], all(diff(which(acepta)) == 1))
  c(inf = min(grilla[acepta]), sup = max(grilla[acepta]))
}

# tabla_ar() arma la tabla que compara el IC 95% de 2SLS (b +- 1.96 ee) con el de Anderson-Rubin.
tabla_ar <- function(iv, x, ar) {
  b <- iv$b[x]; e <- sqrt(iv$V[x, x])
  data.frame(`IC 95\\%` = c("2SLS: $\\hat\\beta\\pm 1.96\\,\\widehat{ee}$", "Anderson--Rubin"),
             Inferior = c(b - qnorm(0.975) * e, ar["inf"]),
             Superior = c(b + qnorm(0.975) * e, ar["sup"]),
             check.names = FALSE)
}

# --- 1. Fertilidad y oferta laboral: modelo simple ---
ae <- readRDS("input/angrist_evans.rds") # readRDS() lee un archivo .rds (formato nativo de R)
N_ae <- nrow(ae)

nombres_ae <- c("(Intercept)" = "Constante", "morekids" = "morekids", "samesex" = "samesex",
                "morekidshat" = "morekids\\_hat", "boys2" = "boys2", "girls2" = "girls2",
                "agem1" = "agem1", "agefstm" = "agefstm", "boy1st" = "boy1st",
                "blackm" = "blackm", "hispm" = "hispm", "othracem" = "othracem")

# Estadisticas descriptivas.
vars_desc <- c("workedm", "weeksm1", "hourswm", "morekids", "samesex")
t_desc <- data.frame(
  Variable = vars_desc,
  Obs.     = sapply(ae[vars_desc], length),
  Media    = sapply(ae[vars_desc], mean),
  `Desv. est.` = sapply(ae[vars_desc], sd),
  Min.     = sapply(ae[vars_desc], min),
  Max.     = sapply(ae[vars_desc], max),
  check.names = FALSE
)
a_latex(t_desc, "ae_desc", 3, "lrrrrr")
tex_desc <- readLines("output/ae_desc.tex")
writeLines(gsub(as.character(N_ae), format(N_ae, big.mark = ","), tex_desc), "output/ae_desc.tex") # 262,793 en la diapositiva

ae_mco      <- mco(hourswm ~ morekids, ae, nombres_ae)
ae_primera  <- mco(morekids ~ samesex, ae, nombres_ae)
ae_reducida <- mco(hourswm ~ samesex, ae, nombres_ae)

nota_mco <- function(res) sprintf("$N=%s$, $R^2=%.4f$. Errores estándar robustos (HC1).",
                                  n_tex(nobs(res$modelo)), summary(res$modelo)$r.squared)
a_latex(ae_mco$tabla, "ae_mco", 3, "lrrrrrr", nota = nota_mco(ae_mco))
a_latex(ae_primera$tabla, "ae_primera", 4, "lrrrrrr", nota = nota_mco(ae_primera))
a_latex(ae_reducida$tabla, "ae_reducida", 4, "lrrrrrr", nota = nota_mco(ae_reducida))

pi_11 <- coef(ae_primera$modelo)["samesex"]
pi_21 <- coef(ae_reducida$modelo)["samesex"]

# 2SLS "a mano": MCO de hourswm sobre la prediccion de la primera etapa.
# fitted() devuelve los valores predichos de una regresion.
ae$morekidshat <- fitted(ae_primera$modelo)
ae_mano <- mco(hourswm ~ morekidshat, ae, nombres_ae)
a_latex(ae_mano$tabla, "ae_2sls_mano", 3, "lrrrrrr", nota = nota_mco(ae_mano))

# 2SLS con la formula de las diapositivas.
# model.matrix(~ x, datos) arma la matriz con una columna de unos (constante) y la variable x.
X_ae <- model.matrix(~ morekids, ae)
Z_ae <- model.matrix(~ samesex, ae)
ae_iv <- tsls(ae$hourswm, X_ae, Z_ae)
a_latex(tabla_coef(ae_iv$b, ae_iv$V, nombres_ae), "ae_2sls", 3, "lrrrrrr",
        nota = sprintf("$N=%s$. Instrumento: samesex. Errores estándar robustos.", n_tex(N_ae)))

# Estimador de Wald: medias de hourswm y morekids para samesex = 0 y samesex = 1.
# tapply(x, g, mean) calcula la media de x para cada valor de g.
t_wald <- data.frame(samesex = c("0", "1", "Total"),
                     hourswm  = c(tapply(ae$hourswm, ae$samesex, mean), mean(ae$hourswm)),
                     morekids = c(tapply(ae$morekids, ae$samesex, mean), mean(ae$morekids)))
a_latex(t_wald, "ae_wald", 4, "lrr")

# Intervalo de confianza de Anderson-Rubin (grilla de b entre -15 y 5, cada 0.001).
ar_ae <- ic_ar(ae$hourswm, ae$morekids, Z_ae, "samesex", seq(-15, 5, by = 0.001))
F_ae <- test_f(ae_primera, "samesex")
a_latex(tabla_ar(ae_iv, "morekids", ar_ae), "ae_ar", 3, "lrr",
        nota = sprintf("$F$ robusto de la primera etapa: %.1f.", F_ae["F"]))

# --- 2. Fertilidad y oferta laboral: controles y dos instrumentos ---
controles <- "agem1 + agefstm + boy1st + blackm + hispm + othracem"
f_ <- function(texto) as.formula(texto) # as.formula() convierte un texto en una formula de R

aeg_mco     <- mco(f_(paste("hourswm ~ morekids +", controles)), ae, nombres_ae)
aeg_primera <- mco(f_(paste("morekids ~ boys2 + girls2 +", controles)), ae, nombres_ae)
F_aeg <- test_f(aeg_primera, c("boys2", "girls2"))

a_latex(aeg_mco$tabla, "aeg_mco", 3, "lrrrrrr", nota = nota_mco(aeg_mco))
a_latex(aeg_primera$tabla, "aeg_primera", 4, "lrrrrrr", nota = nota_mco(aeg_primera))

X_aeg <- model.matrix(f_(paste("~ morekids +", controles)), ae)
Z_aeg <- model.matrix(f_(paste("~ boys2 + girls2 +", controles)), ae)
aeg_iv <- tsls(ae$hourswm, X_aeg, Z_aeg)
a_latex(tabla_coef(aeg_iv$b, aeg_iv$V, nombres_ae), "aeg_2sls", 3, "lrrrrrr",
        nota = sprintf("$N=%s$. Instrumentos: boys2 y girls2. Errores estándar robustos.", n_tex(N_ae)))

# IV usando como instrumento la prediccion de la primera etapa (da lo mismo que 2SLS).
ae$morekidshat3 <- fitted(aeg_primera$modelo)
Z_pred <- model.matrix(f_(paste("~ morekidshat3 +", controles)), ae)
aeg_pred <- tsls(ae$hourswm, X_aeg, Z_pred)
a_latex(tabla_coef(aeg_pred$b, aeg_pred$V, nombres_ae), "aeg_iv_pred", 3, "lrrrrrr",
        nota = sprintf("$N=%s$. Instrumento: predicción de la primera etapa. E.e. robustos.", n_tex(N_ae)))

# Dos veces MCO: la segunda etapa "a mano".
aeg_dos <- mco(f_(paste("hourswm ~ morekidshat3 +", controles)), ae,
               c(nombres_ae, "morekidshat3" = "morekids\\_hat"))
a_latex(aeg_dos$tabla, "aeg_dos_mco", 3, "lrrrrrr", nota = nota_mco(aeg_dos))

# --- 3. Demanda de pescado (Graddy) ---
fish <- readRDS("input/fish.rds")
N_f <- nrow(fish)
dias <- "mon + tues + wed + thurs"
nombres_f <- c("(Intercept)" = "Constante", "lavgprc" = "lavgprc", "mon" = "mon",
               "tues" = "tues", "wed" = "wed", "thurs" = "thurs", "speed2" = "speed2",
               "speed3" = "speed3", "wave2" = "wave2", "wave3" = "wave3", "vhat" = "vhat")

fish_mco <- mco(f_(paste("ltotqty ~ lavgprc +", dias)), fish, nombres_f)
a_latex(fish_mco$tabla, "fish_mco", 3, "lrrrrrr", nota = nota_mco(fish_mco))

fish_p4 <- mco(f_(paste("lavgprc ~", dias, "+ speed2 + speed3 + wave2 + wave3")), fish, nombres_f)
a_latex(fish_p4$tabla, "fish_primera4", 3, "lrrrrrr", nota = nota_mco(fish_p4))

F_4     <- test_f(fish_p4, c("speed2", "speed3", "wave2", "wave3"))
F_speed <- test_f(fish_p4, c("speed2", "speed3"))
fish_p2 <- mco(f_(paste("lavgprc ~", dias, "+ wave2 + wave3")), fish, nombres_f)
F_wave  <- test_f(fish_p2, c("wave2", "wave3"))

t_rel <- data.frame(
  `$H_0$` = c("speed2 $=$ speed3 $=$ wave2 $=$ wave3 $=0$", "speed2 $=$ speed3 $=0$",
              "wave2 $=$ wave3 $=0$ (sin speed2 ni speed3)"),
  `$F$` = c(F_4["F"], F_speed["F"], F_wave["F"]),
  `Gl.` = c(sprintf("(%d, %d)", F_4["q"], F_4["gl"]), sprintf("(%d, %d)", F_speed["q"], F_speed["gl"]),
            sprintf("(%d, %d)", F_wave["q"], F_wave["gl"])),
  `p-valor` = c(F_4["p"], F_speed["p"], F_wave["p"]),
  check.names = FALSE)
a_latex(t_rel, "fish_relevancia", 2, "lrcr")
a_latex(fish_p2$tabla, "fish_primera2", 3, "lrrrrrr", nota = nota_mco(fish_p2))

# 2SLS. Especificacion base: los cuatro instrumentos. Alternativa: solo wave2 y wave3,
# porque speed2 y speed3 no son relevantes (test anterior).
X_f  <- model.matrix(f_(paste("~ lavgprc +", dias)), fish)
Z_f4 <- model.matrix(f_(paste("~", dias, "+ speed2 + speed3 + wave2 + wave3")), fish)
Z_f2 <- model.matrix(f_(paste("~", dias, "+ wave2 + wave3")), fish)
instr4 <- c("speed2", "speed3", "wave2", "wave3")
instr2 <- c("wave2", "wave3")
fish_iv4 <- tsls(fish$ltotqty, X_f, Z_f4)
fish_iv2 <- tsls(fish$ltotqty, X_f, Z_f2)
nota_iv_f <- function(instr) sprintf("$N=%d$. Instrumentos: %s. Errores estándar robustos.", N_f, instr)
a_latex(tabla_coef(fish_iv4$b, fish_iv4$V, nombres_f), "fish_2sls", 3, "lrrrrrr",
        nota = nota_iv_f("speed2, speed3, wave2 y wave3"))
a_latex(tabla_coef(fish_iv2$b, fish_iv2$V, nombres_f), "fish_2sls_w23", 3, "lrrrrrr",
        nota = nota_iv_f("wave2 y wave3"))

# Intervalos de confianza de Anderson-Rubin para lavgprc (grilla entre -10 y 5, cada 0.001),
# con los cuatro instrumentos y con wave2 y wave3.
grilla_f <- seq(-10, 5, by = 0.001)
ar_f4 <- ic_ar(fish$ltotqty, fish$lavgprc, Z_f4, instr4, grilla_f)
ar_f2 <- ic_ar(fish$ltotqty, fish$lavgprc, Z_f2, instr2, grilla_f)
fila_ar <- function(iv, ar, F_1, etiqueta) {
  t <- tabla_ar(iv, "lavgprc", ar)
  data.frame(Instrumentos = c(sprintf("%s ($F=%.2f$)", etiqueta, F_1), ""), t, check.names = FALSE)
}
t_ar_f <- rbind(fila_ar(fish_iv4, ar_f4, F_4["F"], "Cuatro"),
                fila_ar(fish_iv2, ar_f2, F_wave["F"], "wave2 y wave3"))
a_latex(t_ar_f, "fish_ar", 3, "llrr",
        nota = "$F$: test $F$ robusto de los instrumentos en la primera etapa.")

# Modelos exactamente identificados: un instrumento por vez (wave2 o wave3).
fish_w2 <- tsls(fish$ltotqty, X_f, model.matrix(f_(paste("~", dias, "+ wave2")), fish))
fish_w3 <- tsls(fish$ltotqty, X_f, model.matrix(f_(paste("~", dias, "+ wave3")), fish))
a_latex(tabla_coef(fish_w2$b, fish_w2$V, nombres_f), "fish_2sls_wave2", 3, "lrrrrrr", nota = nota_iv_f("wave2"))
a_latex(tabla_coef(fish_w3$b, fish_w3$V, nombres_f), "fish_2sls_wave3", 3, "lrrrrrr", nota = nota_iv_f("wave3"))

# Tests de sobreidentificacion.
# (a) Sargan: regresion de los residuos de 2SLS sobre todas las exogenas, LM = N R^2.
# (b) J de Hansen, robusto a heteroscedasticidad:
#     J = N gbar' S^(-1) gbar, con gbar = (1/N) sum z_i u_i y S = (1/N) sum u_i^2 z_i z_i'.
#     S se estima con los residuos de 2SLS, pero gbar usa los residuos de GMM eficiente
#     (ponderacion S^(-1)). Con los residuos de 2SLS en gbar, J no tiene distribucion
#     chi-cuadrado(L - K) bajo heteroscedasticidad.
sobreid <- function(iv, Z, instr) {
  fish$uhat <- iv$u
  m <- mco(f_(paste("uhat ~", paste(instr, collapse = " + "), "+", dias)), fish, nombres_f)
  R2 <- summary(m$modelo)$r.squared
  S  <- crossprod(Z * iv$u) / N_f
  A  <- t(X_f) %*% Z %*% solve(S)
  b_gmm <- solve(A %*% t(Z) %*% X_f, A %*% t(Z) %*% fish$ltotqty)
  gbar  <- crossprod(Z, as.vector(fish$ltotqty - X_f %*% b_gmm)) / N_f
  list(mano = m, R2 = R2, LM = N_f * R2, J = as.numeric(N_f * t(gbar) %*% solve(S) %*% gbar),
       b_gmm = b_gmm[2], gl = ncol(Z) - ncol(X_f)) # gl = L - K
}
over4 <- sobreid(fish_iv4, Z_f4, instr4)
over2 <- sobreid(fish_iv2, Z_f2, instr2)
a_latex(over4$mano$tabla, "fish_overid_mano", 4, "lrrrrrr",
        nota = sprintf("$N=%d$, $R^2=%.5f$. Errores estándar robustos (HC1).", N_f, over4$R2))
t_over <- data.frame(
  Instrumentos = c("Cuatro", "", "wave2 y wave3", ""),
  Test = rep(c("Sargan: $LM_N=N\\hat R^2$ (homoscedasticidad)", "Hansen: $J_N$ (GMM eficiente, robusto)"), 2),
  Estadístico = c(over4$LM, over4$J, over2$LM, over2$J),
  `Gl.` = c(over4$gl, over4$gl, over2$gl, over2$gl),
  `p-valor` = pchisq(c(over4$LM, over4$J, over2$LM, over2$J), c(over4$gl, over4$gl, over2$gl, over2$gl),
                     lower.tail = FALSE),
  check.names = FALSE)
a_latex(t_over, "fish_overid", 3, "llrcr")

# Test de endogeneidad con funcion de control: MCO controlando por los residuos de la
# primera etapa; el test t (o F = t^2) sobre esos residuos. Base: primera etapa con los
# cuatro instrumentos; alternativa: con wave2 y wave3.
fish$vhat <- residuals(fish_p4$modelo) # residuals() devuelve los residuos de una regresion
fish_cf <- mco(f_(paste("ltotqty ~ lavgprc +", dias, "+ vhat")), fish, nombres_f)
F_cf <- test_f(fish_cf, "vhat")
a_latex(fish_cf$tabla, "fish_cf", 3, "lrrrrrr", nota = nota_mco(fish_cf))
fish$vhat <- residuals(fish_p2$modelo)
F_cf2 <- test_f(mco(f_(paste("ltotqty ~ lavgprc +", dias, "+ vhat")), fish, nombres_f), "vhat")

# --- Puntos de control en la consola ---
ee <- function(V, v) sqrt(V[v, v])
cat(sprintf("Angrist-Evans: N = %d\n", N_ae))
cat(sprintf("  MCO morekids: %.3f (%.3f) | primera etapa: %.4f | forma reducida: %.4f | ILS: %.3f\n",
            coef(ae_mco$modelo)["morekids"], ee(ae_mco$V, "morekids"), pi_11, pi_21, pi_21 / pi_11))
cat(sprintf("  2SLS: %.3f (%.3f) | a mano: %.3f (%.3f)\n", ae_iv$b["morekids"], ee(ae_iv$V, "morekids"),
            coef(ae_mano$modelo)["morekidshat"], ee(ae_mano$V, "morekidshat")))
cat(sprintf("  Con controles: MCO %.3f | 2SLS %.3f (%.3f) | dos veces MCO e.e. %.3f | F primera etapa %.2f\n",
            coef(aeg_mco$modelo)["morekids"], aeg_iv$b["morekids"], ee(aeg_iv$V, "morekids"),
            ee(aeg_dos$V, "morekidshat3"), F_aeg["F"]))
cat(sprintf("Graddy: N = %d | MCO %.3f | F primera etapa (4 instr.) %.2f | (speed) %.2f | (wave) %.2f\n", N_f,
            coef(fish_mco$modelo)["lavgprc"], F_4["F"], F_speed["F"], F_wave["F"]))
cat(sprintf("  2SLS 4 instr. %.3f (%.3f) | wave2 y wave3 %.3f (%.3f)\n", fish_iv4$b["lavgprc"],
            ee(fish_iv4$V, "lavgprc"), fish_iv2$b["lavgprc"], ee(fish_iv2$V, "lavgprc")))
cat(sprintf("  Sobreid. 4 instr.: Sargan %.3f, Hansen J %.3f (GMM %.3f) | wave2 y wave3: Sargan %.3f, Hansen J %.3f (GMM %.3f)\n",
            over4$LM, over4$J, over4$b_gmm, over2$LM, over2$J, over2$b_gmm))
cat(sprintf("  Endogeneidad (funcion de control): 4 instr. F = %.2f, p = %.3f | wave2 y wave3 F = %.2f, p = %.3f\n",
            F_cf["F"], F_cf["p"], F_cf2["F"], F_cf2["p"]))
cat(sprintf("Anderson-Rubin: Angrist-Evans [%.3f, %.3f] | Graddy 4 instr. [%.3f, %.3f] | wave2 y wave3 [%.3f, %.3f]\n",
            ar_ae["inf"], ar_ae["sup"], ar_f4["inf"], ar_f4["sup"], ar_f2["inf"], ar_f2["sup"]))
cat("Listo. Tablas en output/.\n")
