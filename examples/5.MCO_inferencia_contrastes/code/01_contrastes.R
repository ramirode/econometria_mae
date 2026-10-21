# Contraste de hipotesis e intervalos de confianza con MCO y errores estandar
# robustos, para el deck 5.MCO_inferencia.
#
# Genera (en output/):
#   - reg_ca.tex/.csv             testscr ~ str + el_pct: coeficientes, errores estandar, t, p-valor e IC 95%
#   - reg_nlsy.tex/.csv           log(wage) ~ educ + IQ + exper + tenure + married + south + urban
#   - vcov_nlsy.tex/.csv          matriz de varianzas y covarianzas estimada (educ, IQ, exper, tenure)
#   - test_exper_tenure.tex/.csv  H0: beta_exper = beta_tenure (Wald y F)
#   - test_educ_iq.tex/.csv       H0: beta_educ = beta_IQ = 0 (Wald y F)
#   - test_global.tex/.csv        H0: todas las pendientes son cero (Wald y F)
#   - reg_hprice.tex/.csv         log(price) ~ log(nox) + rooms + stratio
#   - delta_rooms.tex/.csv        (exp(beta_rooms) - 1) * 100 con su error estandar (metodo delta)
#   - wald_invariancia.tex/.csv   la misma H0 escrita de dos formas da dos estadisticos de Wald distintos
#   - local_power_tstat.pdf       poder asintotico local del test t en funcion de delta
#
# Tablas -> codigo LaTeX (.tex), para \input{} directo en el deck.
# Grafico -> .pdf, para \includegraphics.
#
# Datos (todos de uso publico, ver README.md):
#   - caschool.dta  Stock & Watson, 420 distritos escolares de California, 1998-1999
#   - wage2.dta     Wooldridge, NLSY: 935 hombres, salarios en 1980 (Blackburn y Neumark, 1992)
#   - hprice2.dta   Wooldridge, 506 comunidades del area de Boston en 1970 (Harrison y Rubinfeld, 1978)
#
# Todos los contrastes se calculan "a mano" con las formulas de las diapositivas:
# W = (R'b - theta0)' [R' Avar(b) R]^(-1) (R'b - theta0), donde Avar(b) = V/N es
# la matriz robusta HC1 (la que Stata llama "robust").

# --- Estructura de carpetas esperada ---
# Este script asume que existe una carpeta de proyecto con esta estructura:
#
#   5.MCO_inferencia_contrastes/
#     code/
#       01_contrastes.R   <- este archivo
#     input/
#       caschool.dta, wage2.dta, hprice2.dta  <- datos de entrada
#     output/             <- se crea sola al correr el script (tablas y grafico van aca)
#
# Si no la tenes armada: crea la carpeta "5.MCO_inferencia_contrastes" en tu compu,
# adentro crea "code/" e "input/", pone este script en code/ y los datos en input/.
# output/ no hace falta crearla, el script la crea sola.

# Reemplazar por la ruta a la carpeta "5.MCO_inferencia_contrastes" en TU computadora.
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/examples/5.MCO_inferencia_contrastes"
setwd(dir_proyecto) # fija la carpeta de trabajo: de aca en mas, "input/..." y "output/..." apuntan siempre ahi

rm(list = ls()) # borra todos los objetos que pudieran quedar de una sesion anterior, para partir de cero

# library(x) carga un paquete (una libreria de funciones adicionales) para poder usarlo.
# Hay que haberlo instalado antes una vez con install.packages("x").
library(haven)    # para leer archivos .dta (formato nativo de Stata)
library(sandwich) # matriz de varianzas y covarianzas robusta (HC1)

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

# tabla_coef() arma la tabla de una regresion: coeficiente, error estandar robusto,
# estadistico t, p-valor e intervalo de confianza al 95%.
# Como lm() y Stata, usa los valores criticos de la t de Student con N - K grados de libertad.
tabla_coef <- function(modelo, nombres) {
  b  <- coef(modelo)
  ee <- sqrt(diag(vcovHC(modelo, type = "HC1"))) # errores estandar robustos
  gl <- df.residual(modelo)                      # grados de libertad: N - K
  t  <- b / ee
  c  <- qt(0.975, gl)                            # valor critico al 5% (dos colas)
  data.frame(
    Variable    = nombres[names(b)],
    Coef.       = b,
    `Error est.` = ee,
    t           = t,
    `p-valor`   = 2 * pt(-abs(t), gl),
    `IC 95\\% inf.` = b - c * ee,
    `IC 95\\% sup.` = b + c * ee,
    check.names = FALSE
  )
}

# wald() calcula el estadistico de Wald para H0: R'b = theta0 con la matriz robusta,
# y lo reporta junto con su p-valor (chi-cuadrado con q grados de libertad) y con
# la version que reportan R y Stata, F = W / q, con su p-valor de la F de Fisher-Snedecor.
# Rt es la matriz R' de las diapositivas: una fila por restriccion.
wald <- function(modelo, Rt, theta0, hipotesis) {
  b     <- coef(modelo)
  avar  <- vcovHC(modelo, type = "HC1")  # estimador de Avar(b) = V/N
  dif   <- Rt %*% b - theta0
  W     <- as.numeric(t(dif) %*% solve(Rt %*% avar %*% t(Rt)) %*% dif)
  q     <- nrow(Rt)
  data.frame(
    `$H_0$`            = hipotesis,
    `$q$`              = q,
    `$W_N$`            = W,
    `p-valor ($\\chi^2_q$)` = pchisq(W, q, lower.tail = FALSE),
    `$F_N=W_N/q$`      = W / q,
    `p-valor ($F$)`    = pf(W / q, q, df.residual(modelo), lower.tail = FALSE),
    check.names = FALSE
  )
}

# --- 1. Tamano de clase y rendimiento academico (California) ---
# lm(y ~ x1 + x2, data = ...) estima una regresion lineal (MCO) de y sobre x1 y x2.
ca <- read_dta("input/caschool.dta")
m_ca <- lm(testscr ~ str + el_pct, data = ca)

nombres_ca <- c("(Intercept)" = "Constante", "str" = "str", "el_pct" = "el\\_pct")
t_ca <- tabla_coef(m_ca, nombres_ca)
t_ca <- t_ca[c(2, 3, 1), ] # la constante al final, como en Stata
a_latex(t_ca, "reg_ca", 3, "lrrrrrr",
        nota = sprintf("$N=%d$, $R^2=%.3f$. Errores estándar robustos (HC1).",
                       nobs(m_ca), summary(m_ca)$r.squared))

# Intervalo de confianza al 95% con el valor critico de la normal, como en las diapositivas.
b_str  <- coef(m_ca)["str"]
ee_str <- sqrt(vcovHC(m_ca, type = "HC1")["str", "str"])
cat("CA: coef. de str", round(b_str, 3), "| e.e.", round(ee_str, 3),
    "| IC 95% (normal): [", round(b_str - 1.96 * ee_str, 3), ",", round(b_str + 1.96 * ee_str, 3), "]\n")

# --- 2. Retornos de la educacion (NLSY) ---
w <- read_dta("input/wage2.dta")
m_w <- lm(log(wage) ~ educ + IQ + exper + tenure + married + south + urban, data = w)

nombres_w <- c("(Intercept)" = "Constante", "educ" = "educ", "IQ" = "IQ", "exper" = "exper",
               "tenure" = "tenure", "married" = "married", "south" = "south", "urban" = "urban")
t_w <- tabla_coef(m_w, nombres_w)
t_w <- t_w[c(2:8, 1), ]
a_latex(t_w, "reg_nlsy", 4, "lrrrrrr",
        nota = sprintf("$N=%d$, $R^2=%.3f$. Errores estándar robustos (HC1).",
                       nobs(m_w), summary(m_w)$r.squared))

# Matriz de varianzas y covarianzas estimada de los cuatro primeros coeficientes.
# Se multiplica por 10^6 para que los numeros sean legibles.
avar_w <- vcovHC(m_w, type = "HC1")
sub_v  <- avar_w[c("educ", "IQ", "exper", "tenure"), c("educ", "IQ", "exper", "tenure")] * 1e6
sub_v[upper.tri(sub_v)] <- NA # matriz simetrica: se muestra solo la mitad inferior
t_v <- data.frame(Variable = rownames(sub_v), sub_v, check.names = FALSE)
a_latex(t_v, "vcov_nlsy", 3, "lrrrr")
tex_v <- readLines("output/vcov_nlsy.tex")
writeLines(gsub("\\bNA\\b", "", tex_v), "output/vcov_nlsy.tex") # celdas vacias en lugar de NA

# Cada fila de Rt es una restriccion; las columnas siguen el orden de coef(m_w):
# (Intercept), educ, IQ, exper, tenure, married, south, urban.
Rt_exper_tenure <- matrix(c(0, 0, 0, 1, -1, 0, 0, 0), nrow = 1)
Rt_educ_iq      <- rbind(c(0, 1, 0, 0, 0, 0, 0, 0),
                         c(0, 0, 1, 0, 0, 0, 0, 0))
Rt_global       <- cbind(0, diag(7)) # R' = [0  I_K]: todas las pendientes, sin la constante

test_et <- wald(m_w, Rt_exper_tenure, 0, "$\\beta_{exper}=\\beta_{tenure}$")
test_ei <- wald(m_w, Rt_educ_iq, c(0, 0), "$\\beta_{educ}=\\beta_{IQ}=0$")
test_gl <- wald(m_w, Rt_global, rep(0, 7), "Todas las pendientes $=0$")
a_latex(test_et, "test_exper_tenure", 4, "lrrrrr")
a_latex(test_ei, "test_educ_iq", 2, "lrrrrr")
a_latex(test_gl, "test_global", 2, "lrrrrr")

# Estadistico t de H0: beta_exper = beta_tenure (con una restriccion, W = t^2).
t_et <- (coef(m_w)["exper"] - coef(m_w)["tenure"]) /
  sqrt(avar_w["exper", "exper"] + avar_w["tenure", "tenure"] - 2 * avar_w["exper", "tenure"])
cat("NLSY: t de exper - tenure:", round(t_et, 3), "| p-valor (normal):", round(2 * pnorm(-abs(t_et)), 3), "\n")
cat("NLSY: W de educ = IQ = 0:", round(test_ei[["$W_N$"]], 2), "\n")
cat("NLSY: inversa de la submatriz de educ e IQ:\n")
print(round(solve(avar_w[c("educ", "IQ"), c("educ", "IQ")])))

# --- 3. Contaminacion y precio de las viviendas (Boston) ---
h <- read_dta("input/hprice2.dta")
m_h <- lm(log(price) ~ log(nox) + rooms + stratio, data = h)

nombres_h <- c("(Intercept)" = "Constante", "log(nox)" = "log(nox)", "rooms" = "rooms",
               "stratio" = "stratio")
t_h <- tabla_coef(m_h, nombres_h)
t_h <- t_h[c(2:4, 1), ]
a_latex(t_h, "reg_hprice", 4, "lrrrrrr",
        nota = sprintf("$N=%d$, $R^2=%.3f$. Errores estándar robustos (HC1).",
                       nobs(m_h), summary(m_h)$r.squared))

# Metodo delta para r(b) = (exp(b_rooms) - 1) * 100, el cambio porcentual exacto del precio
# por una habitacion mas. Su derivada es R = exp(b_rooms) * 100.
avar_h  <- vcovHC(m_h, type = "HC1")
b_rooms <- coef(m_h)["rooms"]
r_hat   <- (exp(b_rooms) - 1) * 100
R_hat   <- exp(b_rooms) * 100
avar_r  <- R_hat * avar_h["rooms", "rooms"] * R_hat # = R' Avar(b) R, un escalar
ee_r    <- sqrt(avar_r)
t_delta <- data.frame(
  ` `           = "$(\\exp(\\beta_{rooms})-1)\\times 100$",
  Estimador     = r_hat,
  `Error est.`  = ee_r,
  z             = r_hat / ee_r,
  `p-valor`     = 2 * pnorm(-abs(r_hat / ee_r)),
  `IC 95\\% inf.` = r_hat - 1.96 * ee_r,
  `IC 95\\% sup.` = r_hat + 1.96 * ee_r,
  check.names = FALSE
)
a_latex(t_delta, "delta_rooms", 3, "lrrrrrr")

# La misma hipotesis (rooms no afecta el precio) escrita de dos formas:
# lineal, H0: beta_rooms = 0, y no lineal, H0: (exp(beta_rooms) - 1) * 100 = 0.
W_lineal    <- as.numeric(b_rooms^2 / avar_h["rooms", "rooms"])
W_no_lineal <- as.numeric(r_hat^2 / avar_r)
t_inv <- data.frame(
  `$H_0$`   = c("$\\beta_{rooms}=0$", "$(\\exp(\\beta_{rooms})-1)\\times 100=0$"),
  `$W_N$`   = c(W_lineal, W_no_lineal),
  `p-valor ($\\chi^2_1$)` = pchisq(c(W_lineal, W_no_lineal), 1, lower.tail = FALSE),
  check.names = FALSE
)
a_latex(t_inv, "wald_invariancia", 2, "lrr")
cat("Boston: r(b) =", round(r_hat, 3), "| Avar(r) =", round(avar_r, 3),
    "| W no lineal =", round(W_no_lineal, 2), "| W lineal =", round(W_lineal, 2), "\n")

# --- 4. Poder asintotico local del test t ---
# Bajo la alternativa local, t_N converge a Z + delta, y el poder del test de dos colas es
# Phi(delta - c) + Phi(-delta - c), con c = z_{1 - alpha/2}.
delta <- seq(-3, 3, by = 0.01)
alfas <- c(0.10, 0.05, 0.01)
colores <- c("#1F77B4", "#FF7F0E", "#2CA02C")
pdf("output/local_power_tstat.pdf", width = 6, height = 3.8)
par(mar = c(4.5, 4.5, 1, 1))
plot(NULL, xlim = c(-3, 3), ylim = c(0, 1), xlab = expression(delta),
     ylab = "Poder asintótico local", las = 1)
abline(h = alfas, col = "grey85", lty = 3)
for (k in seq_along(alfas)) {
  c_alfa <- qnorm(1 - alfas[k] / 2)
  lines(delta, pnorm(delta - c_alfa) + pnorm(-delta - c_alfa), col = colores[k], lwd = 2)
}
# bquote() arma la expresion matematica "alpha = 0.10" para cada valor de alfa.
leyenda <- as.expression(lapply(format(alfas), function(a) bquote(alpha == .(a))))
legend("top", legend = leyenda, col = colores, lwd = 2,
       horiz = TRUE, bty = "n")
invisible(dev.off()) # cierra el archivo pdf

cat("Listo. Tablas y grafico en output/.\n")
