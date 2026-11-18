# Relevancia con dos variables endogenas y dos instrumentos (simulacion).
# Calcula el F usual de cada primera etapa, el F condicional de Sanderson-Windmeijer y el
# estadistico de Cragg-Donald, todos con errores homoscedasticos.
# Entradas: ninguna (los datos se simulan). Salidas: output/relevancia_varias_endogenas.{tex,csv}

rm(list = ls())
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/examples/7.variables_instrumentales_relevancia-varias-endogenas"
setwd(dir_proyecto)

# install.packages(c("fixest", "lfe"))   # una sola vez
library(fixest)   # F usual y Cragg-Donald
library(lfe)      # F condicional (condfstat)

dir.create("output", showWarnings = FALSE)

# Modelo: y = 1 + x1 + x2 + w + u, con u = v + e. Las dos x contienen v, asi que son endogenas.
# El control w es exogeno; z1 y z2 son los dos instrumentos excluidos.
simular <- function(escenario, N = 500) {
  z1 <- rnorm(N); z2 <- rnorm(N); w <- rnorm(N); v <- rnorm(N)
  if (escenario == "A") {            # cada instrumento mueve una endogena distinta
    x1 <- w + 1.0 * z1 + v + rnorm(N)
    x2 <- w + 1.0 * z2 + v + rnorm(N)
  } else {                           # los dos instrumentos mueven casi lo mismo a las dos
    x1 <- w + 0.5 * z1 + 0.5 * z2 + v + rnorm(N)
    x2 <- w + 0.5 * z1 + 0.6 * z2 + v + rnorm(N)
  }
  y <- 1 + x1 + x2 + w + v + rnorm(N)
  data.frame(y, x1, x2, w, z1, z2)
}

diagnosticos <- function(d, escenario) {
  m_fx  <- feols(y ~ w | 0 | x1 + x2 ~ z1 + z2, data = d, vcov = "iid")
  m_lfe <- felm(y ~ w | 0 | (x1 | x2 ~ z1 + z2), data = d)
  f_usual <- sapply(fitstat(m_fx, "ivf1"), function(r) r$stat)
  f_cond  <- as.numeric(condfstat(m_lfe, type = "iid"))
  cd <- unname(fitstat(m_fx, "cd")$cd)
  b1 <- unname(coef(m_fx)["fit_x1"]); se1 <- unname(se(m_fx)["fit_x1"])
  data.frame(Escenario = escenario,
             F_usual_x1 = f_usual[1], F_usual_x2 = f_usual[2],
             F_cond_x1 = f_cond[1],   F_cond_x2 = f_cond[2],
             CD = cd, beta1 = b1, ee_beta1 = se1,
             row.names = NULL)
}

# F condicional a mano para la endogena xj, dada la otra endogena xo (K2 = 2, L1 = 2 instrumentos).
f_cond_mano <- function(d, xj, xo, L1 = 2, K2 = 2) {
  xhat_j <- fitted(lm(reformulate(c("w", "z1", "z2"), xj), d))   # 1. primeras etapas
  xhat_o <- fitted(lm(reformulate(c("w", "z1", "z2"), xo), d))
  g  <- coef(lm(xhat_j ~ xhat_o + w, data = d))["xhat_o"]        # 2. xhat_j sobre xhat_o y w
  d$dj <- d[[xj]] - g * d[[xo]]                                  # 3. xj menos lo que explica xo
  f_ingenuo <- anova(lm(dj ~ w, d), lm(dj ~ w + z1 + z2, d))$F[2]  # 4. F de z1 y z2 en dj ~ z
  f_ingenuo * L1 / (L1 - K2 + 1)                                 # 5. ajuste de grados de libertad
}

set.seed(2026)
res <- rbind(diagnosticos(simular("A"), "A"),
             diagnosticos(simular("B"), "B"))
print(res, digits = 3)

# Control: el F condicional a mano coincide (casi) con condfstat
d_ctrl <- simular("B")
c(mano = f_cond_mano(d_ctrl, "x1", "x2"),
  condfstat = unname(condfstat(felm(y ~ w | 0 | (x1 | x2 ~ z1 + z2), d_ctrl), type = "iid")[1]))

write.csv(res, "output/relevancia_varias_endogenas.csv", row.names = FALSE)

# Tabla en LaTeX para la lamina
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)
filas <- with(res, paste0(
  c("A: cada instrumento mueve una endógena", "B: primeras etapas casi proporcionales"), " & ",
  fmt(F_usual_x1, 1), " & ", fmt(F_usual_x2, 1), " & ",
  fmt(F_cond_x1, 1),  " & ", fmt(F_cond_x2, 1),  " & ",
  fmt(CD, 1), " & ", fmt(beta1, 2), " (", fmt(ee_beta1, 2), ") \\\\"))
tex <- c("\\begin{tabular}{lrrrrrr}", "\\hline",
         " & \\multicolumn{2}{c}{$F$ usual} & \\multicolumn{2}{c}{$F$ condicional} & & \\\\",
         "Escenario & $x_1$ & $x_2$ & $x_1$ & $x_2$ & CD & $\\hat\\beta_1$ (e.e.) \\\\",
         "\\hline", filas, "\\hline", "\\end{tabular}")
writeLines(tex, "output/relevancia_varias_endogenas.tex")
