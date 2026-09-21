# Discriminacion en el mercado laboral: experimento vs. datos observacionales.
#
# La idea del ejemplo es comparar dos regresiones que se ven identicas en el
# papel --- una variable dependiente binaria, una dummy de raza, una progresion
# de controles --- pero que se interpretan de forma completamente distinta,
# porque en una la variable de interes fue asignada al azar y en la otra no.
#
#   Parte A (experimento): Bertrand y Mullainathan (2004) mandaron 4.870
#     curriculos ficticios a avisos de trabajo reales en Boston y Chicago. A
#     cada curriculo le asignaron AL AZAR un nombre tipicamente blanco (Emily,
#     Greg) o tipicamente afroamericano (Lakisha, Jamal). La variable dependiente
#     es si la empresa llamo para una entrevista.
#     -> como el nombre se asigno al azar, agregar controles casi no mueve el
#        coeficiente. Eso es lo que uno espera cuando E(u|x) = 0 se cumple por diseño.
#
#   Parte B (observacional): CPS 2024 (la encuesta de hogares de Estados
#     Unidos). Misma forma de regresion: estar empleado sobre una dummy de raza,
#     con controles.
#     -> aca la raza NO se asigno al azar. El coeficiente se mueve muchisimo al
#        agregar controles, y ni siquiera el modelo mas completo identifica un
#        efecto causal.
#
# Tablas -> codigo LaTeX (output/*.tex), para \input{} directo en el deck,
#           y una copia en .csv para abrir en Excel/Sheets.
# Graficos -> imagenes (output/*.png), para \includegraphics.

# --- Estructura de carpetas esperada ---
# Este script asume que existe una carpeta de proyecto con esta estructura:
#
#   extra_discriminacion-mercado-laboral/
#     code/
#       01_analysis.R              <- este archivo
#     input/
#       resumes_bm2004.rds         <- datos del experimento
#       cps_2024_subset.rds        <- datos del CPS
#     output/                      <- se crea sola al correr el script
#
# Si no la tenes armada: crea la carpeta "extra_discriminacion-mercado-laboral"
# en tu compu, adentro crea "code/" e "input/", pone este script en code/ y los
# dos archivos .rds en input/. output/ la crea el script solo.

# Reemplazar por la ruta a la carpeta "extra_discriminacion-mercado-laboral" en TU computadora.
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/examples/extra_discriminacion-mercado-laboral"
setwd(dir_proyecto) # fija la carpeta de trabajo: de aca en mas, "input/..." y "output/..." apuntan siempre ahi

rm(list = ls()) # borra todos los objetos que pudieran quedar de una sesion anterior, para partir de cero

# library(x) carga un paquete (una libreria de funciones adicionales) para poder usarlo.
# Hay que haberlo instalado antes una vez con install.packages("x").
library(tidyverse)    # incluye dplyr (filter, mutate, %>%), ggplot2 (graficos), tibble, etc.
library(sandwich)     # errores estandar robustos (HC1)
library(modelsummary) # arma tablas de estadisticas descriptivas y de regresiones
library(kableExtra)   # formato de las tablas en LaTeX (booktabs)

options("modelsummary_factory_latex" = "kableExtra")   # booktabs simple, sin tabularray/siunitx
options("modelsummary_format_numeric_latex" = "plain") # sin \num{}, no requiere siunitx

# crea la carpeta output/ si todavia no existe (por ejemplo, en una copia recien clonada)
dir.create("output", showWarnings = FALSE)

# fmt4() da el formato de los numeros en las tablas: 4 decimales.
# Hacen falta 4 (y no 2 o 3) porque toda la gracia del ejemplo esta en comparar
# cuanto se mueve el coeficiente entre especificaciones, y con menos decimales
# el movimiento del experimento se redondearia hasta desaparecer.
fmt4 <- function(x) format(round(x, 4), big.mark = ",", nsmall = 4, trim = TRUE)

# format_n() arregla el N al pie de las tablas de modelsummary: el argumento fmt
# no llega a esa fila, asi que se reemplaza el numero sin formato ("4870") por la
# version con separador de miles ("4,870") en el texto LaTeX ya generado.
# El \\b es limite de palabra, para no tocar por accidente otros numeros de la tabla.
format_n <- function(tabla, n) {
  gsub(paste0("\\b", n, "\\b"), format(n, big.mark = ","), as.character(tabla))
}


# ============================================================================
# PARTE A --- El experimento: Bertrand y Mullainathan (2004)
# ============================================================================

# readRDS() lee un archivo de datos en formato .rds (el formato nativo de R).
resumes <- readRDS("input/resumes_bm2004.rds")

# grupo es una etiqueta de texto para las tablas y el grafico.
# ifelse(condicion, valor_si, valor_no) elige uno de dos valores fila por fila.
# factor(..., levels = ...) fija el orden de los grupos: primero el grupo de
# referencia (nombre blanco), que es contra el que se compara todo.
resumes <- resumes %>%
  mutate(grupo = factor(ifelse(black == 1, "Nombre afroamericano", "Nombre blanco"),
                        levels = c("Nombre blanco", "Nombre afroamericano")))

n_resumes <- nrow(resumes)
cat("Experimento: N =", n_resumes, "curriculos enviados\n")

# --- Tasa de callback por grupo -> output/callback_rates.png ---
# group_by() + summarise() colapsa la base a una fila por grupo, calculando
# en cada una las estadisticas que le pedimos.
tasas <- resumes %>%
  group_by(grupo) %>%
  summarise(
    n        = n(),                  # cuantos curriculos se mandaron
    callback = mean(callback),       # proporcion que recibio llamado
    # error estandar de una proporcion: sqrt(p(1-p)/n)
    se       = sqrt(callback * (1 - callback) / n),
    .groups = "drop"
  ) %>%
  mutate(ci_low = callback - 1.96 * se, ci_high = callback + 1.96 * se)

print(tasas)
write.csv(tasas, "output/callback_rates.csv", row.names = FALSE)

# ggplot() arma graficos por capas: primero se definen los datos y los ejes (aes),
# despues se van agregando capas con + (barras, lineas, etiquetas, estilo).
plot_tasas <- ggplot(tasas, aes(x = grupo, y = callback)) +
  geom_col(fill = "#1F77B4", width = 0.5) +                              # una barra por grupo
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.12) +      # intervalo de confianza 95%
  # scales::percent formatea el eje y como porcentaje (0.0965 -> "9.7%")
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = NULL, y = "Llamados para entrevista") +
  theme_minimal(base_size = 13)

# ggsave() guarda el grafico como imagen en disco
ggsave("output/callback_rates.png", plot_tasas, width = 6.5, height = 4, dpi = 200)

# --- Tabla de balance -> output/balance_audit.tex ---
# Comparamos las caracteristicas de los curriculos entre los dos grupos. Como el
# nombre se asigno al azar, los dos grupos tienen que ser iguales en todo lo
# demas: esa es la prueba visual de que la aleatorizacion funciono.
vars_balance <- c(
  "Años de experiencia"         = "experience",
  "Cantidad de empleos previos" = "jobs",
  "Currículo de alta calidad"   = "alta_cal",
  "Tiene distinciones"          = "honors",
  "Trabajo voluntario"          = "volunteer",
  "Servicio militar"            = "military",
  "Huecos en el historial"      = "holes",
  "Sabe computación"            = "computer",
  "Algo de universidad"         = "college",
  "Mujer"                       = "female",
  "Chicago (vs. Boston)"        = "chicago"
)

# map_dfr(x, funcion) aplica la funcion a cada elemento de x y junta los
# resultados en una sola tabla (data frame), fila por fila.
# seq_along(v) devuelve 1, 2, 3, ... hasta el largo de v.
balance <- map_dfr(seq_along(vars_balance), function(i) {
  etiqueta <- names(vars_balance)[i]
  v        <- resumes[[vars_balance[i]]]
  # varias de estas variables son factores "no"/"yes"; las pasamos a 0/1
  if (is.factor(v)) v <- as.numeric(v == "yes")
  blanco <- v[resumes$black == 0]
  afam   <- v[resumes$black == 1]
  dif    <- mean(afam) - mean(blanco)
  # error estandar de la diferencia de medias entre dos grupos independientes
  se_dif <- sqrt(var(afam) / length(afam) + var(blanco) / length(blanco))
  # tibble() arma una fila de tabla con estas columnas
  tibble(Variable = etiqueta,
         `Nombre blanco` = mean(blanco), `Nombre afroamericano` = mean(afam),
         Diferencia = dif, `Error estándar` = se_dif)
})

print(balance)
write.csv(balance, "output/balance_audit.csv", row.names = FALSE)

# kbl() convierte un data frame en una tabla LaTeX con formato booktabs.
# digits = 3 redondea; linesep = "" saca el espacio extra cada 5 filas.
balance_tex <- kbl(balance, format = "latex", booktabs = TRUE,
                   digits = 3, linesep = "", row.names = FALSE)
writeLines(as.character(balance_tex), "output/balance_audit.tex")

# --- Regresiones: callback sobre nombre afroamericano, agregando controles ---
# lm(y ~ x, data = ...) estima una regresion lineal (OLS) de y sobre x.
# Como la variable dependiente es binaria (0/1), esto es un modelo de
# probabilidad lineal: el coeficiente se lee como cambio en la PROBABILIDAD
# de recibir un llamado.
a1 <- lm(callback ~ black, data = resumes)
a2 <- lm(callback ~ black + female + chicago, data = resumes)
a3 <- lm(callback ~ black + female + chicago + alta_cal + experience + jobs +
           honors + volunteer + military + holes + school + email + computer +
           special + college, data = resumes)
# industry son efectos fijos de rubro del aviso: compara curriculos que
# compitieron por avisos del mismo tipo de industria.
a4 <- lm(callback ~ black + female + chicago + alta_cal + experience + jobs +
           honors + volunteer + military + holes + school + email + computer +
           special + college + industry, data = resumes)

# list(nombre = objeto, ...) agrupa los modelos en una sola lista, con un
# nombre para cada uno, para poder pasarlos juntos a modelsummary().
modelos_audit <- list(
  "(1) Sin controles"   = a1,
  "(2) + Ciudad, sexo"  = a2,
  "(3) + Currículo"     = a3,
  "(4) + Industria"     = a4
)

# modelsummary() arma la tabla de resultados.
# vcov = "HC1" pide errores estandar robustos a heterocedasticidad.
# coef_map deja solo la fila que cuenta la historia (no los 15 controles).
audit_tex <- modelsummary(
  modelos_audit, vcov = "HC1", output = "latex", stars = FALSE, fmt = fmt4,
  coef_map = c("black" = "Nombre afroamericano"),
  gof_map = c("nobs", "r.squared")
)
writeLines(format_n(audit_tex, n_resumes), "output/reg_audit.tex")

# Misma tabla, pero como data.frame -> CSV, para abrirla sin compilar LaTeX
audit_csv <- modelsummary(
  modelos_audit, vcov = "HC1", output = "data.frame", stars = FALSE, fmt = fmt4,
  coef_map = c("black" = "Nombre afroamericano"),
  gof_map = c("nobs", "r.squared")
)
write.csv(audit_csv, "output/reg_audit.csv", row.names = FALSE)


# ============================================================================
# PARTE B --- Los datos observacionales: CPS 2024
# ============================================================================

cps <- readRDS("input/cps_2024_subset.rds")
n_cps <- nrow(cps)
cat("CPS: N =", n_cps, "personas de 22 a 40 anios\n")

# --- Descriptivas por raza -> output/desc_cps.tex ---
# La formula de datasummary se lee "filas ~ columnas". Factor(black) parte la
# tabla en dos columnas (una por grupo racial) y Mean pide la media de cada
# variable de fila dentro de cada columna.
# transmute() es como mutate(), pero se queda SOLO con las columnas que crea.
# Lo usamos para que la tabla tenga unicamente las variables con su etiqueta en
# castellano: si quedaran tambien las originales (age, female, ...), datasummary
# podria tomar el nombre en ingles como titulo de la fila.
cps_desc <- cps %>%
  transmute(
    Grupo = ifelse(black == 1, "Negro", "Blanco"),
    `Empleado` = employed,
    `Mujer` = female,
    `Edad` = age,
    `Casado/a` = married,
    `Vive en área metropolitana` = metro,
    `Universitaria completa o más` = as.numeric(educ %in% c("Universitaria completa", "Postgrado"))
  )

desc_formula <- Empleado + Mujer + Edad + `Casado/a` + `Vive en área metropolitana` +
  `Universitaria completa o más` ~ Factor(Grupo) * Mean

desc_tex <- datasummary(desc_formula, data = cps_desc, output = "latex", fmt = 3)
writeLines(format_n(desc_tex, n_cps), "output/desc_cps.tex")

desc_csv <- datasummary(desc_formula, data = cps_desc, output = "data.frame", fmt = 3)
write.csv(desc_csv, "output/desc_cps.csv", row.names = FALSE)

# --- Regresiones: estar empleado sobre raza, agregando controles ---
# Misma forma que la Parte A: modelo de probabilidad lineal, errores robustos.
# La diferencia es que aca 'black' no fue asignado al azar.
c1 <- lm(employed ~ black, data = cps)
c2 <- lm(employed ~ black + female + age + I(age^2), data = cps)
c3 <- lm(employed ~ black + female + age + I(age^2) + educ, data = cps)
# state son efectos fijos de estado: compara personas que viven en el mismo estado.
c4 <- lm(employed ~ black + female + age + I(age^2) + educ + married + metro +
           state, data = cps)

modelos_cps <- list(
  "(1) Sin controles"       = c1,
  "(2) + Sexo, edad"        = c2,
  "(3) + Educación"         = c3,
  "(4) + Estado, familia"   = c4
)

cps_tex <- modelsummary(
  modelos_cps, vcov = "HC1", output = "latex", stars = FALSE, fmt = fmt4,
  coef_map = c("black" = "Negro"),
  gof_map = c("nobs", "r.squared")
)
writeLines(format_n(cps_tex, n_cps), "output/reg_cps.tex")

cps_csv <- modelsummary(
  modelos_cps, vcov = "HC1", output = "data.frame", stars = FALSE, fmt = fmt4,
  coef_map = c("black" = "Negro"),
  gof_map = c("nobs", "r.squared")
)
write.csv(cps_csv, "output/reg_cps.csv", row.names = FALSE)


# ============================================================================
# COMPARACION --- el grafico que resume el ejemplo
# ============================================================================

# extraer_coef() toma una lista de modelos y devuelve, para cada uno, el
# coeficiente de la variable de interes con su intervalo de confianza al 95%
# (usando errores robustos HC1). Se define una vez y se usa para las dos partes.
extraer_coef <- function(modelos, variable, panel) {
  map_dfr(names(modelos), function(nm) {
    mod <- modelos[[nm]]
    est <- coef(mod)[variable] # coef() extrae los coeficientes estimados
    # vcovHC(mod, type = "HC1") es la matriz de varianzas-covarianzas robusta;
    # [variable, variable] toma la varianza del coeficiente, y su raiz es el error estandar.
    se  <- sqrt(vcovHC(mod, type = "HC1")[variable, variable])
    tibble(panel = panel, especificacion = nm, coef = est, se_robusto = se,
           ci_low = est - 1.96 * se, ci_high = est + 1.96 * se)
  })
}

comparacion <- bind_rows(
  extraer_coef(modelos_audit, "black", "A. Experimento: llamado para entrevista"),
  extraer_coef(modelos_cps,   "black", "B. CPS 2024: estar empleado")
) %>%
  # factor(x, levels = unique(x)) fija el orden en que aparecen las
  # especificaciones en el eje horizontal: (1), (2), (3), (4). Sin esto ggplot
  # las ordenaria alfabeticamente.
  mutate(especificacion = factor(especificacion, levels = unique(especificacion)))

# Cuanto se mueve el coeficiente entre el modelo (1) y el (4), en porcentaje.
# Es el numero que resume todo el ejemplo.
cambio <- comparacion %>%
  group_by(panel) %>%
  summarise(coef_1 = first(coef), coef_4 = last(coef),
            cambio_pct = 100 * (last(coef) - first(coef)) / abs(first(coef)),
            .groups = "drop")

print(comparacion)
print(cambio)
write.csv(comparacion, "output/coef_comparison.csv", row.names = FALSE)
write.csv(cambio, "output/coef_change.csv", row.names = FALSE)

# Linea de referencia: el coeficiente del modelo (1) en cada panel. Sirve para
# ver de un golpe si agregar controles mueve o no la estimacion.
referencia <- comparacion %>%
  group_by(panel) %>%
  summarise(ref = first(coef), .groups = "drop")

plot_comparacion <- ggplot(comparacion, aes(x = especificacion, y = coef)) +
  # linea horizontal punteada en el valor sin controles
  geom_hline(data = referencia, aes(yintercept = ref),
             linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high), color = "#1F77B4", linewidth = 0.8) +
  # facet_wrap divide el grafico en paneles, uno por dataset.
  # scales = "free" deja que cada panel tenga su propia escala vertical Y su
  # propio eje horizontal: si no, los dos paneles mostrarian las etiquetas de
  # las ocho especificaciones juntas, no las cuatro que le tocan a cada uno.
  facet_wrap(~ panel, scales = "free") +
  labs(x = NULL, y = "Coef. de la dummy racial (IC 95%)") +
  theme_minimal(base_size = 12) +
  # angula las etiquetas del eje x para que no se pisen
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("output/coef_comparison.png", plot_comparacion, width = 10, height = 4, dpi = 200)

cat("Listo. Tablas y gráficos en output/.\n")
