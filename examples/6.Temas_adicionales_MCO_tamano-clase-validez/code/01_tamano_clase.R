# Tamano de clase, forma funcional y validez interna/externa, para el deck
# 6.Temas_adicionales_MCO. Mismo analisis para California (distritos escolares),
# Massachusetts (distritos escolares) y el experimento STAR de Tennessee.
#
# Genera (en output/):
#   - reg_ca_formas.tex/.csv        testscr: 4 modelos (controles, ingreso en logaritmos, interaccion, polinomio cubico en str)
#   - me_ca.tex/.csv                efecto marginal promedio de str en cada modelo de California
#   - reg_ca_interaccion.tex/.csv   modelo con la interaccion centrada (efecto de str en la media de el_pct)
#   - desc_ca_ma.tex/.csv           medias y desviaciones estandar, California vs Massachusetts
#   - reg_ma_formas.tex/.csv        totsc4: 5 modelos para Massachusetts
#   - me_ma.tex/.csv                efecto marginal promedio de tchratio en cada modelo de Massachusetts
#   - test_conjunto_formas.tex/.csv test F robusto de H0: str^2 = str^3 = 0 (California y Massachusetts)
#   - efecto_2_estudiantes.tex/.csv efecto de reducir 2 estudiantes por profesor, California vs Massachusetts
#   - reg_star.tex/.csv             STAR: puntaje en kinder, 1ro, 2do y 3er grado
#   - comparacion_star.tex/.csv     efecto estimado en desviaciones estandar: STAR, California y Massachusetts
#   - fig_ingreso_dispersion.pdf    puntaje vs ingreso: ajuste lineal, nivel-log y cuadratico
#   - fig_ingreso_particionada.pdf  regresion particionada del puntaje sobre el ingreso
#   - fig_ingreso_residuos.pdf      valores predichos vs residuos del modelo lineal en el ingreso
#
# Tablas -> codigo LaTeX (.tex) para \input{} directo en el deck, y .csv para abrirlas en Excel.
# Figuras -> .pdf, para \includegraphics.
#
# Datos (todos de uso publico, ver README.md):
#   - caschool.dta  Stock & Watson, 420 distritos escolares de California, 1998-1999
#   - mcas.dta      Stock & Watson, 220 distritos escolares de Massachusetts, 1997-1998
#   - star_sw.dta   Stock & Watson, experimento STAR (Tennessee), estudiantes de kinder a 3er grado
#
# Todos los errores estandar son robustos (HC1, los que Stata llama "robust").

# --- Estructura de carpetas esperada ---
#   6.Temas_adicionales_MCO_tamano-clase-validez/
#     code/01_tamano_clase.R   <- este archivo
#     input/caschool.dta, mcas.dta, star_sw.dta
#     output/                  <- se crea sola al correr el script

# Reemplazar por la ruta a la carpeta "6.Temas_adicionales_MCO_tamano-clase-validez" en TU computadora.
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/examples/6.Temas_adicionales_MCO_tamano-clase-validez"
setwd(dir_proyecto) # de aca en mas, "input/..." y "output/..." apuntan siempre a esa carpeta

rm(list = ls()) # borra los objetos de una sesion anterior, para partir de cero

library(haven)    # para leer archivos .dta (formato nativo de Stata)
library(sandwich) # matriz de varianzas y covarianzas robusta (HC1)
library(car)      # linearHypothesis(): test F de restricciones lineales
library(ggplot2)  # graficos
library(knitr)    # kable(): tablas en LaTeX

dir.create("output", showWarnings = FALSE)

# --- Funciones auxiliares ---

# Coeficiente, error estandar robusto y p-valor (t de Student con N - K grados de libertad).
coefs_robustos <- function(m) {
  b  <- coef(m)
  se <- sqrt(diag(vcovHC(m, type = "HC1")))
  p  <- 2 * pt(-abs(b / se), df.residual(m))
  cbind(b = b, se = se, p = p)
}

# Combinacion lineal g'b de los coeficientes, con error estandar robusto y p-valor.
lincom <- function(m, g) {
  b <- coef(m); V <- vcovHC(m, type = "HC1")
  g <- g[names(b)]; g[is.na(g)] <- 0
  est <- sum(g * b); se <- sqrt(as.numeric(t(g) %*% V %*% g))
  c(b = est, se = se, p = 2 * pt(-abs(est / se), df.residual(m)))
}

estrellas <- function(p) ifelse(p < 0.01, "^{***}", ifelse(p < 0.05, "^{**}", ifelse(p < 0.10, "^{*}", "")))
celda <- function(r) paste0("$", sprintf("%.3f", r["b"]), estrellas(r["p"]), "$")
nota_estrellas <- "Errores est\\'andar robustos entre par\\'entesis. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."

# Escribe la tabla en output/<archivo>.tex (LaTeX) y .csv (texto plano, sin LaTeX).
guardar <- function(tabla, archivo, alineacion, nota = NULL) {
  sin_latex <- function(x) {
    x <- gsub("\\^\\{(\\*+)\\}", "\\1", x)
    x <- gsub("[$]|\\\\|\\{|\\}", "", x)
    gsub("'", "", x)
  }
  csv <- tabla
  names(csv) <- sin_latex(names(csv))
  csv[] <- lapply(csv, function(x) if (is.character(x)) sin_latex(x) else x)
  write.csv(csv, paste0("output/", archivo, ".csv"), row.names = FALSE)

  tex <- as.character(kable(tabla, format = "latex", booktabs = TRUE, align = alineacion,
                            linesep = "", escape = FALSE, row.names = FALSE))
  if (!is.null(nota)) { # la nota va debajo de la tabla, en un parrafo que no ensancha la tabla
    tex <- paste0(tex, "\n\\par\\vspace{3pt}\n{\\footnotesize\\parbox{0.9\\linewidth}{\\centering ", nota, "}}")
  }
  writeLines(tex, paste0("output/", archivo, ".tex"))
}

# Tabla de varios modelos: una fila por variable con el coeficiente (y estrellas) y debajo
# el error estandar entre parentesis (debajo, o en la misma fila si una_linea = TRUE); al final R2 y N. "columnas" son los encabezados (por defecto (1), (2), ...).
tabla_modelos <- function(modelos, vars, etiquetas, archivo, columnas = NULL, una_linea = FALSE) {
  k <- length(modelos)
  filas <- list()
  for (v in vars) {
    c1 <- c2 <- rep("", k)
    for (j in seq_len(k)) {
      if (v %in% names(coef(modelos[[j]]))) {
        r <- coefs_robustos(modelos[[j]])[v, ]
        c1[j] <- celda(r); c2[j] <- sprintf("(%.3f)", r["se"])
      }
    }
    if (una_linea) { # coeficiente y error estandar en la misma fila, para tablas con muchas filas
      filas[[length(filas) + 1]] <- c(etiquetas[[v]], ifelse(c1 == "", "", paste(c1, c2)))
    } else {
      filas[[length(filas) + 1]] <- c(etiquetas[[v]], c1)
      filas[[length(filas) + 1]] <- c("", c2)
    }
  }
  tabla <- as.data.frame(do.call(rbind, filas), stringsAsFactors = FALSE)
  if (is.null(columnas)) columnas <- paste0("(", seq_len(k), ")")
  names(tabla) <- c("", columnas)
  pie <- rbind(
    c("$R^2$", sprintf("%.3f", sapply(modelos, function(m) summary(m)$r.squared))),
    c("$N$",   sapply(modelos, function(m) format(nobs(m), big.mark = "")))
  )
  pie <- as.data.frame(pie, stringsAsFactors = FALSE); names(pie) <- names(tabla)
  tabla <- rbind(tabla, pie)
  guardar(tabla, archivo, c("l", rep("c", k)), nota_estrellas)
}

# Tabla de un solo modelo: coeficiente (con estrellas) y error estandar en columnas separadas.
tabla_un_modelo <- function(m, vars, etiquetas, archivo) {
  r <- coefs_robustos(m)[vars, , drop = FALSE]
  tabla <- data.frame(
    Variable = unlist(etiquetas[vars]),
    `Coef.` = apply(r, 1, celda),
    `Error est.` = sprintf("%.3f", r[, "se"]),
    check.names = FALSE, stringsAsFactors = FALSE)
  pie <- data.frame(Variable = c("$R^2$", "$N$"),
                    `Coef.` = c(sprintf("%.3f", summary(m)$r.squared), format(nobs(m))),
                    `Error est.` = "", check.names = FALSE, stringsAsFactors = FALSE)
  guardar(rbind(tabla, pie), archivo, c("l", "c", "c"),
          "Errores est\\'andar robustos (HC1). $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.")
}

# Tabla del efecto marginal promedio: una fila, un modelo por columna.
tabla_me <- function(ests, archivo, etiqueta) {
  k <- length(ests)
  f1 <- c(etiqueta, sapply(ests, celda))
  f2 <- c("", sapply(ests, function(r) sprintf("(%.3f)", r["se"])))
  tabla <- as.data.frame(rbind(f1, f2), stringsAsFactors = FALSE)
  names(tabla) <- c("", paste0("(", seq_len(k), ")"))
  guardar(tabla, archivo, c("l", rep("c", k)), nota_estrellas)
}

# --- 1. California: forma funcional ---
ca <- read_dta("input/caschool.dta")
ca$lavginc <- log(ca$avginc)
ca$str2 <- ca$str^2
ca$str3 <- ca$str^3
ca$elstr <- ca$el_pct * ca$str

ca1 <- lm(testscr ~ str + el_pct + meal_pct, data = ca)
ca2 <- lm(testscr ~ str + el_pct + meal_pct + lavginc, data = ca)
ca3 <- lm(testscr ~ str + el_pct + meal_pct + lavginc + elstr, data = ca)
ca4 <- lm(testscr ~ str + el_pct + meal_pct + lavginc + str2 + str3, data = ca)

et_ca <- list(str = "str", el_pct = "el\\_pct", meal_pct = "meal\\_pct", lavginc = "log(avginc)",
              elstr = "el\\_pct $\\times$ str", str2 = "str$^2$", str3 = "str$^3$", "(Intercept)" = "Constante")
tabla_modelos(list(ca1, ca2, ca3, ca4),
              c("str", "el_pct", "meal_pct", "lavginc", "elstr", "str2", "str3", "(Intercept)"),
              et_ca, "reg_ca_formas", una_linea = TRUE)

# Efecto marginal promedio de str: promedio sobre la muestra de dE(testscr|x)/dstr.
# Es una combinacion lineal de los coeficientes, con los promedios de str, str^2 y el_pct.
me_ca <- list(
  lincom(ca1, c(str = 1)),
  lincom(ca2, c(str = 1)),
  lincom(ca3, c(str = 1, elstr = mean(ca$el_pct))),
  lincom(ca4, c(str = 1, str2 = 2 * mean(ca$str), str3 = 3 * mean(ca$str^2)))
)
tabla_me(me_ca, "me_ca", "Efecto marginal promedio")

# Modelo con la interaccion centrada: el coeficiente de str es el efecto en la media de el_pct.
ca$el_str_dem <- (ca$el_pct - mean(ca$el_pct)) * ca$str
ca_c <- lm(testscr ~ str + el_pct + meal_pct + lavginc + el_str_dem, data = ca)
tabla_un_modelo(ca_c, c("str", "el_pct", "meal_pct", "lavginc", "el_str_dem", "(Intercept)"),
                c(et_ca, el_str_dem = "(el\\_pct $-$ media) $\\times$ str"), "reg_ca_interaccion")

# --- 2. Massachusetts ---
ma <- read_dta("input/mcas.dta")
ma$p2 <- ma$percap^2; ma$p3 <- ma$percap^3
ma$ts2 <- ma$tchratio^2; ma$ts3 <- ma$tchratio^3
ma$elt <- ma$pctel * ma$tchratio

ma1 <- lm(totsc4 ~ tchratio, data = ma)
ma2 <- lm(totsc4 ~ tchratio + pctel + lnch_pct, data = ma)
ma3 <- lm(totsc4 ~ tchratio + pctel + lnch_pct + percap + p2 + p3, data = ma)
ma4 <- lm(totsc4 ~ tchratio + pctel + lnch_pct + percap + p2 + p3 + elt, data = ma)
ma5 <- lm(totsc4 ~ tchratio + pctel + lnch_pct + percap + p2 + p3 + ts2 + ts3, data = ma)

et_ma <- list(tchratio = "tchratio", pctel = "pctel", lnch_pct = "lnch\\_pct", percap = "percap",
              p2 = "percap$^2$", p3 = "percap$^3$", elt = "pctel $\\times$ tchratio",
              ts2 = "tchratio$^2$", ts3 = "tchratio$^3$", "(Intercept)" = "Constante")
tabla_modelos(list(ma1, ma2, ma3, ma4, ma5),
              c("tchratio", "pctel", "lnch_pct", "percap", "p2", "p3", "elt", "ts2", "ts3", "(Intercept)"),
              et_ma, "reg_ma_formas", una_linea = TRUE)

me_ma <- list(
  lincom(ma1, c(tchratio = 1)),
  lincom(ma2, c(tchratio = 1)),
  lincom(ma3, c(tchratio = 1)),
  lincom(ma4, c(tchratio = 1, elt = mean(ma$pctel))),
  lincom(ma5, c(tchratio = 1, ts2 = 2 * mean(ma$tchratio), ts3 = 3 * mean(ma$tchratio^2)))
)
tabla_me(me_ma, "me_ma", "Efecto marginal promedio")

# Test F robusto (HC1) de H0: el polinomio cubico en str no agrega nada (str^2 = str^3 = 0).
f_ca <- linearHypothesis(ca4, c("str2 = 0", "str3 = 0"), vcov. = vcovHC(ca4, type = "HC1"), test = "F")
f_ma <- linearHypothesis(ma5, c("ts2 = 0", "ts3 = 0"), vcov. = vcovHC(ma5, type = "HC1"), test = "F")
test_conj <- data.frame(
  Muestra = c("California (modelo 4)", "Massachusetts (modelo 5)"),
  `$F$`   = sprintf("%.2f", c(f_ca$F[2], f_ma$F[2])),
  `p-valor` = sprintf("%.3f", c(f_ca$`Pr(>F)`[2], f_ma$`Pr(>F)`[2])),
  check.names = FALSE, stringsAsFactors = FALSE)
guardar(test_conj, "test_conjunto_formas", c("l", "c", "c"),
        "$H_0:\\beta_{str^2}=\\beta_{str^3}=0$. Test $F$ con matriz robusta (HC1), $q=2$.")

# Descriptivas: California vs Massachusetts. Los puntajes son de tests distintos y el ingreso
# es el promedio del distrito (California) o el ingreso per capita (Massachusetts).
ms <- function(x) c(sprintf("%.2f", mean(x)), sprintf("%.2f", sd(x)))
desc <- data.frame(
  Variable = c("Puntaje del test", "Estudiantes por profesor", "\\% de estudiantes que aprenden ingl\\'es",
               "\\% de estudiantes con almuerzo subsidiado", "Ingreso (miles de US\\$)"),
  rbind(c(ms(ca$testscr), ms(ma$totsc4)), c(ms(ca$str), ms(ma$tchratio)),
        c(ms(ca$el_pct), ms(ma$pctel)), c(ms(ca$meal_pct), ms(ma$lnch_pct)),
        c(ms(ca$avginc), ms(ma$percap))),
  stringsAsFactors = FALSE)
names(desc) <- c("Variable", "CA: media", "CA: d.e.", "MA: media", "MA: d.e.")
guardar(desc, "desc_ca_ma", c("l", "c", "c", "c", "c"),
        "CA: 420 distritos, \\texttt{testscr}, \\texttt{str}, \\texttt{el\\_pct}, \\texttt{meal\\_pct}, \\texttt{avginc}. MA: 220 distritos, \\texttt{totsc4}, \\texttt{tchratio}, \\texttt{pctel}, \\texttt{lnch\\_pct}, \\texttt{percap}.")
cat("Ingreso medio MA / CA:", round(mean(ma$percap) / mean(ca$avginc), 3), "\n")

# Efecto de reducir 2 estudiantes por profesor, en puntos y en desviaciones estandar del puntaje.
b_ca_lin <- coef(ca2)["str"]; b_ca_cub <- me_ca[[4]]["b"]; b_ma <- coef(ma3)["tchratio"]
sd_ca <- sd(ca$testscr); sd_ma <- sd(ma$totsc4)
efecto <- data.frame(
  Modelo = c("CA, lineal (2)", "CA, c\\'ubico (4)", "MA, lineal (3)"),
  `Coef.` = paste0("$", sprintf("%.2f", c(b_ca_lin, b_ca_cub, b_ma)), "$"),
  `d.e.` = sprintf("%.1f", c(sd_ca, sd_ca, sd_ma)),
  `Efecto (puntos)` = sprintf("%.2f", -2 * c(b_ca_lin, b_ca_cub, b_ma)),
  `Efecto (d.e.)` = sprintf("%.3f", -2 * c(b_ca_lin, b_ca_cub, b_ma) / c(sd_ca, sd_ca, sd_ma)),
  check.names = FALSE, stringsAsFactors = FALSE)
guardar(efecto, "efecto_2_estudiantes", c("l", "c", "c", "c", "c"),
        "Efecto estimado de tener 2 estudiantes menos por profesor. Coef.: coeficiente de str (en el modelo c\\'ubico, efecto marginal promedio). d.e. = desviaci\\'on est\\'andar del puntaje entre distritos.")

# --- 3. STAR (Tennessee) ---
star <- read_dta("input/star_sw.dta")
grados <- c(k = "Kinder", `1` = "1er grado", `2` = "2do grado", `3` = "3er grado")
m_star <- lapply(names(grados), function(g) {
  d <- data.frame(y = star[[paste0("tscore", g)]], small = star[[paste0("sc", g)]], aide = star[[paste0("ra", g)]])
  lm(y ~ small + aide, data = d)
})
tabla_modelos(m_star, c("small", "aide", "(Intercept)"),
              list(small = "Small class", aide = "Regular size with aide", "(Intercept)" = "Constante (clase regular)"),
              "reg_star", columnas = unname(grados))

# Comparacion de los tres estudios: efecto estimado en desviaciones estandar del puntaje.
# Las d.e. del puntaje entre estudiantes de California (38.0) y Massachusetts (39.0) son las
# que reportan Stock y Watson; no se pueden calcular con los datos por distrito de arriba.
b_star <- coef(m_star[[1]])["small"]; sd_star <- sd(star$tscorek, na.rm = TRUE)
comp <- data.frame(
  Estudio = c("STAR (kinder)", "California", "Massachusetts"),
  `Coef.` = paste0("$", sprintf("%.2f", c(b_star, b_ca_lin, b_ma)), "$"),
  `Cambio est./prof.` = c("Small vs Regular", "$-7.5$", "$-7.5$"),
  `d.e. del puntaje` = sprintf("%.1f", c(sd_star, 38.0, 39.0)),
  `Efecto estimado (d.e.)` = sprintf("%.2f", c(b_star / sd_star, b_ca_lin * -7.5 / 38.0, b_ma * -7.5 / 39.0)),
  check.names = FALSE, stringsAsFactors = FALSE)
guardar(comp, "comparacion_star", c("l", "c", "c", "c", "c"),
        "STAR: coeficiente de la dummy \\emph{small class}. California y Massachusetts: coeficiente de \\emph{str} (modelos lineales con controles). d.e. = desviaci\\'on est\\'andar del puntaje entre estudiantes.")

# --- 4. Figuras: efecto del ingreso en California ---
tema <- theme_minimal(base_size = 11)
g1 <- ggplot(ca, aes(avginc, testscr)) +
  geom_point(colour = "grey40", alpha = 0.7, size = 1.2) +
  geom_smooth(aes(colour = "Lineal"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_smooth(aes(colour = "Nivel-log"), method = "lm", formula = y ~ log(x), se = FALSE) +
  geom_smooth(aes(colour = "Cuadr\u00e1tico"), method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  scale_colour_manual(values = c("Lineal" = "#1b9e77", "Nivel-log" = "#d95f02", "Cuadr\u00e1tico" = "#7570b3"),
                      breaks = c("Lineal", "Nivel-log", "Cuadr\u00e1tico")) +
  labs(x = "avginc (miles de US$)", y = "testscr", colour = NULL) + tema + theme(legend.position = "bottom")
ggsave("output/fig_ingreso_dispersion.pdf", g1, width = 6.5, height = 4)

# Regresion particionada (Frisch-Waugh-Lovell): puntaje e ingreso, netos de str, el_pct y meal_pct.
rx <- resid(lm(avginc ~ str + el_pct + meal_pct, data = ca))
ry <- resid(lm(testscr ~ str + el_pct + meal_pct, data = ca))
g2 <- ggplot(data.frame(rx, ry), aes(rx, ry)) +
  geom_point(colour = "grey40", alpha = 0.7, size = 1.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "#d95f02") +
  labs(x = "avginc | str, el_pct, meal_pct", y = "testscr | str, el_pct, meal_pct") + tema
ggsave("output/fig_ingreso_particionada.pdf", g2, width = 6.5, height = 4)

# Valores predichos vs residuos del modelo lineal en el ingreso.
m_lin <- lm(testscr ~ str + el_pct + meal_pct + avginc, data = ca)
g3 <- ggplot(data.frame(pred = fitted(m_lin), res = resid(m_lin)), aes(pred, res)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(colour = "grey40", alpha = 0.7, size = 1.2) +
  labs(x = "Valores predichos", y = "Residuos") + tema
ggsave("output/fig_ingreso_residuos.pdf", g3, width = 6.5, height = 4)

# Resumen en consola para chequear contra las diapositivas.
cat("CA efectos marginales promedio:", round(sapply(me_ca, `[`, "b"), 3), "\n")
cat("MA efectos marginales promedio:", round(sapply(me_ma, `[`, "b"), 3), "\n")
cat("F conjunto CA:", round(f_ca$F[2], 2), "p =", round(f_ca$`Pr(>F)`[2], 3),
    "| MA:", round(f_ma$F[2], 2), "p =", round(f_ma$`Pr(>F)`[2], 3), "\n")
cat("STAR N por grado:", sapply(m_star, nobs), "| sd kinder:", round(sd_star, 2), "\n")
cat("Listo. Tablas y figuras en output/.\n")
