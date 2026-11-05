# Simulacion de Monte Carlo: distribucion muestral de los estimadores MCO e IV
# en el modelo y = 1 + x + u (beta1 = 1), con un instrumento z, para el deck
# 7.variables_instrumentales (seccion Instrumentos debiles).
#
# Modelo generador de datos: (u, z, x) normal estandar conjunta con
# correlaciones rho_ux, rho_uz y rho_zx. Cada escenario cambia esas
# correlaciones:
#   rho_ux: exogeneidad de x    (rho_ux = 0  -> x exogena)
#   rho_uz: exogeneidad de z    (rho_uz = 0  -> z exogeno, instrumento valido)
#   rho_zx: relevancia de z     (rho_zx = 0  -> z irrelevante)
#
#   Escenario 1: x exogena,                  z exogeno, rho_zx = 0.20
#   Escenario 2: x endogena (rho_ux = 0.80), z exogeno, rho_zx = 0.20
#   Escenario 3: x endogena (rho_ux = 0.80), z endogeno (rho_uz = 0.20), rho_zx = 0.20
#   Escenario 4: x endogena (rho_ux = 0.80), z exogeno pero irrelevante, rho_zx = 0
#   Escenario 5: como el 2, mas 20 instrumentos irrelevantes adicionales (2SLS con 21 instrumentos)
#
# Genera (en output/):
#   - density_ols_iv<k>.png   densidad de beta1 estimado por MCO y por IV en el escenario k,
#                             con una linea vertical en el valor verdadero beta1 = 1
#   - estimaciones.csv        las 5 x 500 estimaciones de MCO e IV (los datos de los graficos)
#   - resumen.csv             mediana y percentiles 5 y 95 de cada estimador en cada escenario
#   - cobertura.tex/.csv      (y cobertura_2sls.tex, sin la columna de Anderson-Rubin) con x endogena y distintos grados de relevancia de z: F robusto mediano
#                             de la primera etapa, mediana de beta1 estimado por IV, y la proporcion
#                             de simulaciones en que el IC 95% de 2SLS y el de Anderson-Rubin
#                             contienen el verdadero beta1 (2,000 simulaciones por fila)

# --- Estructura de carpetas esperada ---
#
#   7.variables_instrumentales_simulacion-instrumentos-debiles/
#     code/
#       01_montecarlo_instrumentos_debiles.R   <- este archivo
#     output/                                   <- se crea sola al correr el script
#
# No hay carpeta input/: los datos se simulan adentro del script.

# Reemplazar por la ruta a la carpeta "7.variables_instrumentales_simulacion-instrumentos-debiles" en TU computadora.
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/examples/7.variables_instrumentales_simulacion-instrumentos-debiles"
setwd(dir_proyecto) # fija la carpeta de trabajo: de aca en mas, "output/..." apunta siempre ahi

rm(list = ls()) # borra todos los objetos que pudieran quedar de una sesion anterior

library(ggplot2) # graficos

dir.create("output", showWarnings = FALSE) # crea la carpeta output/ si no existe todavia

# --- parametros de la simulacion ---
sample_size <- 500 # N, tamanio de cada muestra
n_simul     <- 500 # cuantas muestras independientes simulamos en cada escenario
n_instr_irr <- 20  # instrumentos irrelevantes adicionales del escenario 5
beta0 <- 1
beta1 <- 1         # valor verdadero del parametro de interes
set.seed(20260705) # fija la semilla: corriendo el script de nuevo se obtienen los mismos numeros

# data.frame con una fila por escenario: las tres correlaciones y si agrega instrumentos irrelevantes
escenarios <- data.frame(
  escenario = 1:5,
  rho_ux    = c(0.00, 0.80, 0.80, 0.80, 0.80),
  rho_uz    = c(0.00, 0.00, 0.20, 0.00, 0.00),
  rho_zx    = c(0.20, 0.20, 0.20, 0.00, 0.20),
  irrelev   = c(FALSE, FALSE, FALSE, FALSE, TRUE)
)

# Estimador IV/2SLS. Z es la matriz de instrumentos (sin constante).
# Primera etapa: MCO de x sobre (1, Z) -> prediccion x_hat.
# Segunda etapa: beta1_2SLS = Cov(x_hat, y) / Cov(x_hat, x).
# Con un solo instrumento esto es exactamente Cov(z, y) / Cov(z, x).
estimar_iv <- function(y, x, Z) {
  x_hat <- fitted(lm(x ~ Z)) # lm(x ~ Z) regresa x sobre una constante y todas las columnas de Z
  cov(x_hat, y) / cov(x_hat, x)
}

resultados <- list() # aca vamos guardando un data.frame por escenario

# for (k in 1:5) repite todo lo de adentro una vez por escenario
for (k in escenarios$escenario) {
  esc <- escenarios[k, ]

  # matriz de varianzas y covarianzas de (u, z, x), en ese orden (como en la diapositiva)
  Sigma <- matrix(c(1,          esc$rho_uz, esc$rho_ux,
                    esc$rho_uz, 1,          esc$rho_zx,
                    esc$rho_ux, esc$rho_zx, 1),
                  nrow = 3, byrow = TRUE)
  # chol(Sigma) da una matriz R tal que t(R) %*% R = Sigma. Si e son normales
  # estandar independientes, e %*% R tiene varianza Sigma: asi generamos (u, z, x)
  # correlacionadas sin necesitar ningun paquete adicional.
  R <- chol(Sigma)

  b_ols <- numeric(n_simul) # vectores vacios donde guardamos una estimacion por simulacion
  b_iv  <- numeric(n_simul)

  for (s in 1:n_simul) {
    e   <- matrix(rnorm(sample_size * 3), ncol = 3) # N x 3 normales estandar independientes
    uzx <- e %*% R                                  # N x 3 con correlaciones Sigma
    u <- uzx[, 1]
    z <- uzx[, 2]
    x <- uzx[, 3]
    y <- beta0 + beta1 * x + u

    b_ols[s] <- cov(x, y) / var(x) # MCO en la regresion simple

    Z <- matrix(z, ncol = 1)
    if (esc$irrelev) {
      # agrega 20 columnas de normales independientes de todo: instrumentos irrelevantes
      Z <- cbind(Z, matrix(rnorm(sample_size * n_instr_irr), ncol = n_instr_irr))
    }
    b_iv[s] <- estimar_iv(y, x, Z)
  }

  resultados[[k]] <- data.frame(escenario = k,
                                estimador = rep(c("MCO", "IV"), each = n_simul),
                                beta1_hat = c(b_ols, b_iv))
}

estimaciones <- do.call(rbind, resultados) # junta los 5 data.frames en uno solo, uno debajo del otro
write.csv(estimaciones, "output/estimaciones.csv", row.names = FALSE)

# --- resumen: mediana y percentiles 5 y 95 (no la media: IV con un instrumento no tiene media finita) ---
resumen <- aggregate(beta1_hat ~ escenario + estimador, data = estimaciones,
                     FUN = function(b) quantile(b, c(0.05, 0.50, 0.95)))
resumen <- do.call(data.frame, resumen) # aplana las columnas de percentiles
names(resumen) <- c("escenario", "estimador", "p05", "mediana", "p95")
resumen <- resumen[order(resumen$escenario, resumen$estimador != "MCO"), ]
resumen[, 3:5] <- round(resumen[, 3:5], 3)
write.csv(resumen, "output/resumen.csv", row.names = FALSE)

# --- un grafico de densidades por escenario ---
# density() estima la densidad de cada estimador usando TODAS sus estimaciones,
# y la evalua en una grilla fina entre -0.5 y 3.5 (el grafico muestra 0 a 3).
# Esto importa en el escenario 4: IV tiene colas muy largas y, si la grilla
# cubriera todo su rango, quedaria con muy pocos puntos en la zona que se ve.
densidad <- function(b) {
  dens <- density(b, from = -0.5, to = 3.5, n = 801)
  data.frame(beta1_hat = dens$x, densidad = dens$y)
}

for (k in escenarios$escenario) {
  d_ols <- densidad(subset(estimaciones, escenario == k & estimador == "MCO")$beta1_hat)
  d_iv  <- densidad(subset(estimaciones, escenario == k & estimador == "IV")$beta1_hat)
  d <- rbind(cbind(d_ols, estimador = "MCO"), cbind(d_iv, estimador = "IV"))
  d$estimador <- factor(d$estimador, levels = c("MCO", "IV"))
  p <- ggplot(d, aes(x = beta1_hat, y = densidad, color = estimador)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = beta1, linetype = "dashed") + # valor verdadero beta1 = 1
    annotate("text", x = beta1, y = Inf, label = "beta[1] == 1", parse = TRUE,
             hjust = -0.15, vjust = 1.5, size = 4.5) +
    scale_color_manual(values = c(MCO = "#4C72B0", IV = "#C44E52")) +
    coord_cartesian(xlim = c(0, 3)) +
    labs(x = expression(hat(beta)[1]), y = "densidad", color = NULL) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top")
  ggsave(paste0("output/density_ols_iv", k, ".png"), p, width = 7, height = 3.4, dpi = 200)
}

# --- cobertura de los intervalos de confianza: 2SLS vs. Anderson-Rubin ---
# Con x endogena (rho_ux = 0.80) y z exogeno, variamos la relevancia de z. En cada
# simulacion calculamos:
#   - el F robusto de la primera etapa (test de que los coeficientes de los instrumentos son 0),
#   - el IC 95% de 2SLS con errores estandar robustos, beta1_hat +- 1.96 ee, y si contiene beta1 = 1,
#   - el test de Anderson-Rubin de H0: beta1 = 1: regresar y - 1*x sobre (1, Z) y contrastar
#     con un test F que los coeficientes de Z son cero. Si no rechaza, el IC de Anderson-Rubin contiene beta1 = 1.
# Errores estandar robustos a heteroscedasticidad. Tests F de MCO (primera etapa y Anderson-Rubin):
# HC1 y valores criticos de la F, como "reg, robust" + "test" en Stata. 2SLS: HC0 y normal, como
# "ivregress 2sls, robust".

# f_robusto() regresa w sobre W = (1, Z) y devuelve el estadistico F robusto (HC1)
# de que los coeficientes de Z (todas las columnas menos la constante) son cero: F = Wald / q.
f_robusto <- function(w, W) {
  n <- nrow(W); K <- ncol(W)
  WW_inv <- solve(crossprod(W))
  g <- WW_inv %*% crossprod(W, w)                    # coeficientes MCO
  e <- as.numeric(w - W %*% g)                       # residuos
  V <- n / (n - K) * WW_inv %*% crossprod(W * e) %*% WW_inv # robusta HC1: (W'W)^-1 (sum e_i^2 w_i w_i') (W'W)^-1 * N/(N-K)
  j <- -1                                            # todas las filas/columnas menos la constante
  as.numeric(t(g[j]) %*% solve(V[j, j, drop = FALSE]) %*% g[j]) / (K - 1)
}

# tsls_robusto() devuelve el coeficiente de x por 2SLS y su error estandar robusto.
tsls_robusto <- function(y, x, W) {
  X  <- cbind(1, x)
  Xh <- W %*% solve(crossprod(W), crossprod(W, X))   # prediccion de la primera etapa para cada columna de X
  A  <- solve(crossprod(Xh, X))
  b  <- A %*% crossprod(Xh, y)
  u  <- as.numeric(y - X %*% b)                      # residuos con x (no con su prediccion)
  V  <- A %*% crossprod(Xh * u) %*% t(A)
  c(b = b[2], ee = sqrt(V[2, 2]))
}

n_simul_cob <- 2000
casos <- data.frame(rho_zx  = c(0.20, 0.10, 0.05, 0.00, 0.20),
                    irrelev = c(0, 0, 0, 0, n_instr_irr)) # instrumentos irrelevantes adicionales
rho_ux <- 0.80

cobertura <- data.frame()
for (k in 1:nrow(casos)) {
  Sigma <- matrix(c(1, 0, rho_ux,
                    0, 1, casos$rho_zx[k],
                    rho_ux, casos$rho_zx[k], 1), nrow = 3, byrow = TRUE)
  R <- chol(Sigma)
  F_1 <- b_iv <- cubre_2sls <- cubre_ar <- numeric(n_simul_cob)
  for (s in 1:n_simul_cob) {
    uzx <- matrix(rnorm(sample_size * 3), ncol = 3) %*% R
    u <- uzx[, 1]; z <- uzx[, 2]; x <- uzx[, 3]
    y <- beta0 + beta1 * x + u
    W <- cbind(1, z)
    if (casos$irrelev[k] > 0) W <- cbind(W, matrix(rnorm(sample_size * casos$irrelev[k]), ncol = casos$irrelev[k]))
    L1 <- ncol(W) - 1                                  # numero de instrumentos

    F_1[s] <- f_robusto(x, W)                          # F robusto de la primera etapa
    iv <- tsls_robusto(y, x, W)
    b_iv[s] <- iv["b"]
    cubre_2sls[s] <- abs(iv["b"] - beta1) / iv["ee"] <= qnorm(0.975)
    cubre_ar[s] <- f_robusto(y - beta1 * x, W) <= qf(0.95, L1, sample_size - L1 - 1)
  }
  cobertura <- rbind(cobertura, data.frame(
    rho_zx = casos$rho_zx[k], instrumentos = 1 + casos$irrelev[k],
    F_mediano = median(F_1), mediana_b_iv = median(b_iv),
    cobertura_2sls = mean(cubre_2sls), cobertura_ar = mean(cubre_ar)))
}
write.csv(cobertura, "output/cobertura.csv", row.names = FALSE)

# Tablas LaTeX para las diapositivas (booktabs), escritas a mano para no depender de otro paquete:
# cobertura_2sls.tex sin la columna de Anderson-Rubin, y cobertura.tex con ella.
# La nota (rho_ux, N, numero de simulaciones) va en el texto de la diapositiva.
tabla_cobertura <- function(con_ar, archivo) {
  nc <- if (con_ar) 6 else 5
  filas <- sprintf("%.2f & %d & %.1f & %.2f & %.1f\\%%%s \\\\",
                   cobertura$rho_zx, cobertura$instrumentos, cobertura$F_mediano, cobertura$mediana_b_iv,
                   100 * cobertura$cobertura_2sls,
                   if (con_ar) sprintf(" & %.1f\\%%", 100 * cobertura$cobertura_ar) else "")
  tex <- c(sprintf("\\begin{tabular}{%s}", strrep("r", nc)), "\\toprule",
           paste0("$\\rho_{z,x}$ & Instrumentos & $F$ mediano & Mediana $\\hat\\beta_{1,IV}$ & IC 2SLS",
                  if (con_ar) " & IC AR" else "", " \\\\"),
           "\\midrule", filas, "\\bottomrule", "\\end{tabular}")
  writeLines(tex, paste0("output/", archivo, ".tex"))
}
tabla_cobertura(FALSE, "cobertura_2sls")
tabla_cobertura(TRUE, "cobertura")

print(resumen, row.names = FALSE)
print(within(cobertura, { cobertura_2sls <- round(cobertura_2sls, 3); cobertura_ar <- round(cobertura_ar, 3)
                          F_mediano <- round(F_mediano, 1); mediana_b_iv <- round(mediana_b_iv, 3) }), row.names = FALSE)
cat("Listo. Figuras y tablas en output/\n")
