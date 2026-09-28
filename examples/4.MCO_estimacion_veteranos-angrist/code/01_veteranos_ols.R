# Efecto del servicio militar voluntario sobre los ingresos (Angrist 1998):
# regresion simple, regresion multiple con controles, sesgo de variable omitida
# y su descomposicion, para el deck 4.MCO_estimacion.
#
# Genera (en output/):
#   - desc_stats.tex/.csv          descriptivas por raza y condicion de veterano
#   - reg_simple.tex/.csv          earnvar ~ dvet (todos, blancos, no blancos)
#   - reg_progression.tex/.csv     efecto de dvet al agregar controles, por raza
#   - progression_plot.png         el mismo resultado en un grafico
#   - ovb_decomp.tex/.csv          descomposicion del sesgo de variable omitida
#   - info_ejercicios.csv          medias y coeficientes de los ejercicios de dummies
#
# Tablas -> codigo LaTeX (.tex), para \input{} directo en el deck.
# Graficos -> imagenes (.png), para \includegraphics.
#
# Datos: solicitantes hombres a las fuerzas armadas de EE.UU. entre 1979 y 1982
# (Angrist 1998, Econometrica). Cada fila es una celda de solicitantes con las
# mismas caracteristicas; earnvar es el ingreso promedio de la celda en
# 1988-1991 (registros de seguridad social) y todas las celdas pesan lo mismo.
# Variables: earnvar (ingreso promedio 1988-91), dvet (1 = veterano),
# dnwhite (1 = no blanco), afqtgrp (grupo de puntaje en el test de aptitud AFQT),
# edgrp (grupo de escolaridad al momento de solicitar), dobyy (anio de
# nacimiento), transyy (anio de solicitud), id (identificador de celda de
# covariables).

# --- Estructura de carpetas esperada ---
# Este script asume que existe una carpeta de proyecto con esta estructura:
#
#   4.MCO_estimacion_veteranos-angrist/
#     code/
#       01_veteranos_ols.R   <- este archivo
#     input/
#       veteran.dta          <- datos de entrada
#     output/                <- se crea sola al correr el script (tablas y graficos van aca)
#
# Si no la tenes armada: crea la carpeta "4.MCO_estimacion_veteranos-angrist" en tu compu,
# adentro crea "code/" e "input/", pone este script en code/ y el archivo .dta en input/.
# output/ no hace falta crearla, el script la crea sola.

# Reemplazar por la ruta a la carpeta "4.MCO_estimacion_veteranos-angrist" en TU computadora.
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/examples/4.MCO_estimacion_veteranos-angrist"
setwd(dir_proyecto) # fija la carpeta de trabajo: de aca en mas, "input/..." y "output/..." apuntan siempre ahi

rm(list = ls()) # borra todos los objetos que pudieran quedar de una sesion anterior, para partir de cero

# library(x) carga un paquete (una libreria de funciones adicionales) para poder usarlo.
# Hay que haberlo instalado antes una vez con install.packages("x").
library(tidyverse)    # incluye dplyr y ggplot2 (graficos)
library(haven)        # para leer archivos .dta (formato nativo de Stata)
library(sandwich)     # errores estandar robustos (HC1)
library(modelsummary) # arma tablas de regresiones

options("modelsummary_factory_latex" = "kableExtra")   # booktabs simple, sin tabularray/siunitx
options("modelsummary_format_numeric_latex" = "plain") # sin \num{}, no requiere siunitx

dir.create("output", showWarnings = FALSE) # crea la carpeta output/ si todavia no existe

# read_dta() lee un archivo de datos en formato .dta (el formato nativo de Stata).
d <- read_dta("input/veteran.dta")
d$blanco <- 1 - d$dnwhite

cat("N celdas:", nrow(d), "\n")

# Funciones auxiliares -------------------------------------------------------

# Escribe un data.frame como tabla LaTeX (booktabs) y como CSV.
# knitr::kable() convierte un data.frame en una tabla LaTeX; escape = FALSE deja
# pasar el codigo LaTeX de los nombres (\\%, $R^2$, etc.).
guardar <- function(df, archivo, align, digits = 1) {
  write.csv(df, paste0("output/", archivo, ".csv"), row.names = FALSE)
  tex <- knitr::kable(df, format = "latex", booktabs = TRUE, digits = digits,
                      align = align, escape = FALSE, linesep = "", format.args = list(big.mark = ","))
  writeLines(as.character(tex), paste0("output/", archivo, ".tex"))
}

# Coeficiente de dvet y su error estandar robusto (HC1) en una regresion.
# vcovHC(modelo, type = "HC1") es la matriz de varianzas robusta a heterocedasticidad.
efecto_dvet <- function(formula, datos) {
  m <- lm(formula, data = datos)
  c(b = unname(coef(m)["dvet"]), se = sqrt(vcovHC(m, type = "HC1")["dvet", "dvet"]))
}

# --- Estadisticas descriptivas por raza y condicion de veterano ---
# Para cada grupo: ingreso promedio y distribucion del grupo AFQT (el test de aptitud
# que usan las fuerzas armadas para seleccionar postulantes).
grupos <- expand_grid(raza = c(0, 1), vet = c(0, 1))
desc <- map_dfr(seq_len(nrow(grupos)), function(i) {
  g <- d %>% filter(dnwhite == grupos$raza[i], dvet == grupos$vet[i])
  afqt <- prop.table(table(factor(g$afqtgrp, levels = 2:6))) * 100
  tibble(
    nombre = paste0(ifelse(grupos$raza[i] == 0, "Blancos", "No blancos"), ", ",
                    ifelse(grupos$vet[i] == 0, "no vet.", "vet.")),
    celdas = nrow(g), ingreso = mean(g$earnvar),
    a2 = afqt[1], a3 = afqt[2], a4 = afqt[3], a5 = afqt[4], a6 = afqt[5]
  )
})
# t() traspone la tabla: una fila por variable y una columna por grupo.
# formatC() da formato al numero: sin decimales para conteos e ingresos, un decimal para porcentajes.
m <- t(as.matrix(desc[, -1]))
desc_t <- data.frame(
  Variable = c("Celdas", "Ingreso promedio 1988-91 (US\\$)", "\\% en grupo AFQT 2", "\\% en grupo AFQT 3",
               "\\% en grupo AFQT 4", "\\% en grupo AFQT 5", "\\% en grupo AFQT 6"),
  apply(m, 2, function(z) c(formatC(z[1:2], format = "f", digits = 0, big.mark = ","),
                            formatC(z[3:7], format = "f", digits = 1))),
  check.names = FALSE, row.names = NULL
)
names(desc_t) <- c("Variable", desc$nombre)
guardar(desc_t, "desc_stats", align = "lcccc", digits = 1)

# --- Regresion simple: earnvar ~ dvet ---
# lm(y ~ x, data = ...) estima una regresion lineal (OLS) de y sobre x.
# Como dvet es una dummy, el coeficiente es la diferencia de medias entre veteranos y no veteranos.
m_todos <- lm(earnvar ~ dvet, data = d)
m_blanco <- lm(earnvar ~ dvet, data = filter(d, dnwhite == 0))
m_noblanco <- lm(earnvar ~ dvet, data = filter(d, dnwhite == 1))

reg_args <- list(
  vcov = "HC1", stars = FALSE, escape = FALSE,
  fmt = function(x) formatC(x, format = "f", digits = 1, big.mark = ","), # separador de miles
  coef_rename = c("(Intercept)" = "Constante", "dvet" = "Veterano"),
  gof_map = data.frame(raw = c("nobs", "r.squared"), clean = c("N", "$R^2$"), fmt = c(0, 3))
)
mods <- list("Todos" = m_todos, "Blancos" = m_blanco, "No blancos" = m_noblanco)
tex <- do.call(modelsummary, c(list(mods, output = "latex"), reg_args))
tex <- sub("^\\\\begin\\{table\\}\n\\\\centering\n", "", as.character(tex))
tex <- sub("\n\\\\end\\{table\\}\\s*$", "", tex)
# gsub() reemplaza cada N sin formato (ej. "7904") por la version con separador de miles ("7,904");
# \\b es limite de palabra, para no tocar otros numeros de la tabla.
for (n in sapply(mods, nobs)) tex <- gsub(paste0("\\b", n, "\\b"), format(n, big.mark = ","), tex)
writeLines(tex, "output/reg_simple.tex")
write.csv(do.call(modelsummary, c(list(mods, output = "data.frame"), reg_args)),
          "output/reg_simple.csv", row.names = FALSE)

# --- Progresion de controles, por raza ---
# Se agregan controles de a uno. factor(x) trata x como categorias: una dummy por valor.
especificaciones <- list(
  "(1)" = earnvar ~ dvet,
  "(2)" = earnvar ~ dvet + factor(afqtgrp),
  "(3)" = earnvar ~ dvet + factor(edgrp),
  "(4)" = earnvar ~ dvet + factor(afqtgrp) + factor(edgrp),
  "(5)" = earnvar ~ dvet + factor(afqtgrp) + factor(edgrp) + factor(dobyy) + factor(transyy),
  "(6)" = earnvar ~ dvet + factor(id)  # una dummy por celda de covariables
)
resultados <- expand_grid(raza = c(0, 1), esp = names(especificaciones)) %>%
  rowwise() %>%
  mutate(
    r = list(efecto_dvet(especificaciones[[esp]], filter(d, dnwhite == raza))),
    b = r["b"], se = r["se"]
  ) %>%
  ungroup() %>%
  select(-r)

# Tabla: dos filas por raza (coeficiente y error estandar) y filas con los controles incluidos
fmt_b <- function(x) formatC(x, format = "f", digits = 1, big.mark = ",")
fila <- function(nombre, raza) {
  z <- filter(resultados, raza == !!raza)
  rbind(c(nombre, fmt_b(z$b)), c("", paste0("(", fmt_b(z$se), ")")))
}
marca <- function(nombre, activo) c(nombre, ifelse(activo, "$\\checkmark$", ""))
prog <- rbind(
  fila("Veterano, blancos", 0),
  fila("Veterano, no blancos", 1),
  marca("Grupo AFQT", c(0, 1, 0, 1, 1, 1) == 1),
  marca("Escolaridad", c(0, 0, 1, 1, 1, 1) == 1),
  marca("A\\~no de nacimiento y de solicitud", c(0, 0, 0, 0, 1, 1) == 1),
  marca("Celda de covariables", c(0, 0, 0, 0, 0, 1) == 1)
)
prog <- as.data.frame(prog)
names(prog) <- c("", names(especificaciones))
write.csv(resultados, "output/reg_progression.csv", row.names = FALSE)
tex <- knitr::kable(prog, format = "latex", booktabs = TRUE, align = "lcccccc", escape = FALSE,
                    linesep = "", row.names = FALSE)
writeLines(as.character(tex), "output/reg_progression.tex")

# Grafico: efecto estimado y su intervalo de confianza de 95% en cada especificacion
p <- resultados %>%
  mutate(raza = ifelse(raza == 0, "Blancos", "No blancos")) %>%
  ggplot(aes(x = esp, y = b, color = raza)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se),
                  position = position_dodge(width = 0.4)) +
  labs(x = "Especificación", y = "Efecto estimado de ser veterano (US$)", color = NULL) +
  theme_minimal(base_size = 13) + theme(legend.position = "bottom")
ggsave("output/progression_plot.png", p, width = 7, height = 4.2, dpi = 200)

# --- Sesgo de variable omitida: descomposicion ---
# Regresion simple:   earnvar = g0 + g1 dvet + v        (g1 = coef. "corto")
# Regresion multiple: earnvar = b0 + b1 dvet + sum_j bj control_j + u   (b1 = coef. "largo")
# En la muestra vale exactamente:  g1 = b1 + sum_j bj * dj,
# donde dj es el coeficiente de dvet en la regresion de control_j sobre dvet.
# Cada termino bj * dj es la parte del sesgo que aporta la variable omitida j.
descomponer <- function(datos) {
  largo <- lm(earnvar ~ dvet + factor(afqtgrp) + factor(edgrp), data = datos)
  corto <- lm(earnvar ~ dvet, data = datos)
  controles <- model.matrix(~ factor(afqtgrp) + factor(edgrp), data = datos)[, -1]
  bj <- coef(largo)[colnames(controles)]
  # apply(M, 2, f) aplica f a cada columna de M; aca, una regresion auxiliar por control
  dj <- apply(controles, 2, function(z) coef(lm(z ~ datos$dvet))[2])
  aporte <- bj * dj
  c(corto = unname(coef(corto)["dvet"]), largo = unname(coef(largo)["dvet"]),
    afqt = sum(aporte[grepl("afqtgrp", names(aporte))]),
    escolaridad = sum(aporte[grepl("edgrp", names(aporte))]))
}
ovb <- rbind(Blancos = descomponer(filter(d, dnwhite == 0)),
             `No blancos` = descomponer(filter(d, dnwhite == 1)))
# Comprobacion de la identidad: corto = largo + aporte AFQT + aporte escolaridad
stopifnot(all(abs(ovb[, "corto"] - ovb[, "largo"] - ovb[, "afqt"] - ovb[, "escolaridad"]) < 1e-6))
ovb_df <- data.frame(
  Grupo = rownames(ovb),
  `Regresi\\'on simple` = ovb[, "corto"],
  `Con controles` = ovb[, "largo"],
  `Sesgo` = ovb[, "corto"] - ovb[, "largo"],
  `Aporte AFQT` = ovb[, "afqt"],
  `Aporte escolaridad` = ovb[, "escolaridad"],
  check.names = FALSE, row.names = NULL
)
guardar(ovb_df, "ovb_decomp", align = "lccccc", digits = 1)

# --- Datos de los ejercicios de dummies e interacciones ---
m_int <- lm(earnvar ~ dvet * dnwhite, data = d)
info <- data.frame(
  cantidad = c("E(earnings)", "beta_dvet (solo dvet)", "beta_dvet", "beta_dnwhite", "beta_dvet_dnwhite"),
  valor = c(mean(d$earnvar), coef(m_todos)["dvet"], coef(m_int)["dvet"],
            coef(m_int)["dnwhite"], coef(m_int)["dvet:dnwhite"])
)
write.csv(info, "output/info_ejercicios.csv", row.names = FALSE)

cat("Efecto de dvet, regresion simple (todos):", round(coef(m_todos)["dvet"], 1), "\n")
cat("Blancos: simple", round(ovb["Blancos", "corto"], 1), "-> con controles", round(ovb["Blancos", "largo"], 1), "\n")
cat("No blancos: simple", round(ovb["No blancos", "corto"], 1), "-> con controles", round(ovb["No blancos", "largo"], 1), "\n")
cat("Listo. Tablas y grafico en output/.\n")
