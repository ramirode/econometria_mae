# Ayudantia 5: primeros pasos en R. Importar datos, estadisticas descriptivas,
# graficos y regresion lineal, con datos de salarios de la Current Population
# Survey (CPS) de EEUU.
#
# El script sigue el orden de los ejercicios del enunciado (TA5.pdf).
#
# Genera (en output/):
#   - desc_stats.csv            media y desvio estandar de las variables principales
#   - desc_por_sexo.csv         las mismas estadisticas, separadas por sexo
#   - frec_educacion.csv        tabla de frecuencias de los anos de educacion
#   - hist_ahe.png              histograma del salario por hora
#   - hist_lahe.png             histograma del logaritmo del salario por hora
#   - box_lahe_sexo.png         log salario por sexo (diagrama de caja)
#   - cef_lahe_educ.png         media del log salario por ano de educacion
#   - cef_lahe_educ_recta.png   el mismo grafico con la recta de regresion
#   - reg_salarios.tex/.csv     tabla con las tres regresiones
#
# Datos: Current Population Survey, marzo de 2005 (Stock & Watson, cap. 8).
# 61,395 trabajadores de tiempo completo de entre 21 y 64 anos.
# Variables: ahe (salario promedio por hora en 2004, en dolares), yrseduc (anos de
# educacion), female (1 si es mujer), age (edad), y northeast, midwest, south,
# west (dummies de region).

# --- Estructura de carpetas esperada ---
# Este script asume que existe una carpeta de proyecto con esta estructura:
#
#   TA5/
#     code/
#       TA5.R            <- este archivo
#     input/
#       ch8_cps.dta      <- datos de entrada
#     output/            <- se crea sola al correr el script (tablas y graficos van aca)
#
# Si no la tenes armada: crea la carpeta "TA5" en tu compu, adentro crea "code/" e
# "input/", pone este script en code/ y el archivo .dta en input/.
# output/ no hace falta crearla, el script la crea sola.

# Reemplazar por la ruta a la carpeta "TA5" en TU computadora.
# En Windows la ruta se escribe con barras hacia adelante: "C:/Users/tu_nombre/Documents/TA5".
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/TA/TA5"
setwd(dir_proyecto) # fija la carpeta de trabajo: de aca en mas, "input/..." y "output/..." apuntan siempre ahi

rm(list = ls()) # borra todos los objetos que pudieran quedar de una sesion anterior, para partir de cero

# library(x) carga un paquete (una libreria de funciones adicionales) para poder usarlo.
# Hay que haberlo instalado antes una vez con install.packages("x").
library(tidyverse)    # incluye dplyr (manejo de datos) y ggplot2 (graficos)
library(haven)        # para leer archivos .dta (formato nativo de Stata)
library(sandwich)     # errores estandar robustos (HC1)
library(modelsummary) # arma tablas de regresiones

options("modelsummary_factory_latex" = "kableExtra")   # tabla LaTeX simple, con booktabs
options("modelsummary_format_numeric_latex" = "plain") # sin \num{}, no requiere siunitx

dir.create("output", showWarnings = FALSE) # crea la carpeta output/ si todavia no existe


# =====================================================================
# 1. Importar e inspeccionar los datos
# =====================================================================

# "<-" es el operador de asignacion: guarda lo que esta a la derecha en un objeto
# con el nombre de la izquierda. read_dta() lee un archivo .dta y devuelve una
# tabla de datos (un "data frame"): una fila por observacion, una columna por variable.
d <- read_dta("input/ch8_cps.dta")

nrow(d)    # numero de filas (observaciones)
ncol(d)    # numero de columnas (variables)
names(d)   # nombres de las variables
head(d)    # las primeras 6 filas
glimpse(d) # una linea por variable: tipo (dbl = numero) y primeros valores

# summary() muestra minimo, cuartiles, media y maximo de cada variable.
# Es la forma mas rapida de detectar valores raros o faltantes (aparecerian como NA).
summary(d)

# d$ahe extrae una columna del data frame como un vector.
mean(d$ahe)
sd(d$ahe)

cat("N observaciones:", nrow(d), "\n")


# =====================================================================
# 2. Crear variables
# =====================================================================

# El operador %>% ("pipe") pasa el objeto de la izquierda como primer argumento de
# la funcion de la derecha: d %>% mutate(...) es lo mismo que mutate(d, ...).
# mutate() crea columnas nuevas (o reemplaza las que ya existen).
# log() es el logaritmo natural.
# if_else(condicion, a, b) devuelve a cuando la condicion es verdadera y b cuando no;
# "==" compara (pregunta si es igual), a diferencia de "=" que asigna.
d <- d %>%
  mutate(
    lahe = log(ahe),
    sexo = if_else(female == 1, "Mujer", "Hombre")
  )

# Siempre conviene chequear que la variable nueva quedo bien armada.
# count() cuenta cuantas filas hay para cada combinacion de valores.
d %>% count(female, sexo)


# =====================================================================
# 3. Estadisticas descriptivas
# =====================================================================

# (a) Media y desvio estandar de las variables principales.
# summarise() colapsa todas las filas en una sola, con los estadisticos que le pidamos.
# across(variables, funciones) aplica las mismas funciones a varias columnas a la vez.
desc <- d %>%
  summarise(across(c(ahe, lahe, yrseduc, age, female),
                   list(media = mean, de = sd)))
desc

# La tabla anterior queda "ancha" (una sola fila, diez columnas). pivot_longer() la
# pasa a formato largo: una fila por variable, con columnas media y de.
desc <- desc %>%
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_sep = "_")
desc
write.csv(desc, "output/desc_stats.csv", row.names = FALSE)

# (b) Las mismas estadisticas por sexo.
# group_by() agrupa las filas: todo lo que sigue se calcula por separado para cada grupo.
# n() cuenta las filas del grupo.
desc_sexo <- d %>%
  group_by(sexo) %>%
  summarise(
    n            = n(),
    media_ahe    = mean(ahe),
    de_ahe       = sd(ahe),
    media_lahe   = mean(lahe),
    media_educ   = mean(yrseduc),
    media_edad   = mean(age)
  )
desc_sexo
write.csv(desc_sexo, "output/desc_por_sexo.csv", row.names = FALSE)

# Brecha de salarios "bruta" (sin controlar por nada): diferencia de medias entre
# hombres y mujeres. filter() se queda con las filas que cumplen la condicion.
ahe_h <- mean(filter(d, female == 0)$ahe)
ahe_m <- mean(filter(d, female == 1)$ahe)
cat("Salario medio hombres:", round(ahe_h, 2), " mujeres:", round(ahe_m, 2),
    " brecha:", round(100 * (ahe_m / ahe_h - 1), 1), "%\n")

# (c) Tabla de frecuencias de los anos de educacion.
frec <- d %>%
  count(yrseduc) %>%
  mutate(porcentaje = 100 * n / sum(n))
frec
write.csv(frec, "output/frec_educacion.csv", row.names = FALSE)


# =====================================================================
# 4. Graficos
# =====================================================================

# Un grafico de ggplot se arma por capas, sumadas con "+":
#   ggplot(datos, aes(...))  que datos y que variable va en cada eje
#   geom_...()               que dibujar (barras, puntos, lineas)
#   labs()                   titulos de los ejes
#   theme_minimal()          estilo general

# (a) Histograma del salario por hora y de su logaritmo.
p_hist_ahe <- ggplot(d, aes(x = ahe)) +
  geom_histogram(bins = 50, fill = "#1F77B4", color = "white") +
  labs(x = "Salario por hora (ahe, dólares)", y = "Número de personas") +
  theme_minimal(base_size = 13)
p_hist_ahe # escribir el nombre del objeto lo muestra en la pestana "Plots" de RStudio
ggsave("output/hist_ahe.png", p_hist_ahe, width = 7, height = 4.5, dpi = 200) # lo guarda como imagen

p_hist_lahe <- ggplot(d, aes(x = lahe)) +
  geom_histogram(bins = 50, fill = "#1F77B4", color = "white") +
  labs(x = "Logaritmo del salario por hora (lahe)", y = "Número de personas") +
  theme_minimal(base_size = 13)
p_hist_lahe
ggsave("output/hist_lahe.png", p_hist_lahe, width = 7, height = 4.5, dpi = 200)

# (b) Log salario por sexo: un diagrama de caja por grupo.
# La linea del medio es la mediana y la caja va del percentil 25 al 75.
p_box <- ggplot(d, aes(x = sexo, y = lahe)) +
  geom_boxplot(fill = "#AEC7E8") +
  labs(x = NULL, y = "Logaritmo del salario por hora (lahe)") +
  theme_minimal(base_size = 13)
p_box
ggsave("output/box_lahe_sexo.png", p_box, width = 7, height = 4.5, dpi = 200)

# (c) Esperanza condicional muestral: media de lahe para cada valor de yrseduc.
# Con 61,395 puntos una nube de dispersion no deja ver nada; como la educacion toma
# pocos valores, se puede graficar directamente la media de cada grupo.
cef <- d %>%
  group_by(yrseduc) %>%
  summarise(media_lahe = mean(lahe), n = n())
cef

# size = n hace el punto mas grande cuanta mas gente hay en ese nivel de educacion.
p_cef <- ggplot(cef, aes(x = yrseduc, y = media_lahe)) +
  geom_point(aes(size = n), color = "#1F77B4") +
  labs(x = "Años de educación (yrseduc)", y = "Media del log salario por hora",
       size = "Personas") +
  theme_minimal(base_size = 13)
p_cef

# (d) Guardar los graficos: cada ggsave() escribe una imagen en output/.
ggsave("output/cef_lahe_educ.png", p_cef, width = 7, height = 4.5, dpi = 200)


# =====================================================================
# 5. Regresion simple
# =====================================================================

# (a) lm(y ~ x, data = ...) estima por MCO la regresion de y sobre x.
# La constante se incluye sola, no hay que agregarla.
m1 <- lm(lahe ~ yrseduc, data = d)

# summary() muestra coeficientes, errores estandar, R2, etc. Ojo: estos errores
# estandar suponen homocedasticidad.
summary(m1)

# coef() devuelve el vector de coeficientes estimados.
coef(m1)

# (b) Errores estandar robustos a heterocedasticidad.
# vcovHC() estima la matriz de varianzas y covarianzas de los coeficientes;
# type = "HC1" es la version que Stata llama "robust". Los errores estandar son la
# raiz cuadrada de la diagonal de esa matriz.
V1 <- vcovHC(m1, type = "HC1")
se_robusto <- sqrt(diag(V1))
se_clasico <- sqrt(diag(vcov(m1))) # vcov() devuelve la matriz bajo homocedasticidad

# cbind() pega vectores como columnas, para verlos lado a lado.
cbind(coef = coef(m1), se_clasico, se_robusto)

# (c) En la regresion simple, la pendiente es Cov(x, y) / Var(x) y la constante es
# media(y) - pendiente * media(x). Verificamos que da lo mismo que lm().
b1 <- cov(d$yrseduc, d$lahe) / var(d$yrseduc)
b0 <- mean(d$lahe) - b1 * mean(d$yrseduc)
c(b0, b1)
coef(m1)

# (d) Valores predichos y residuos.
# fitted() devuelve la prediccion para cada observacion y resid() el residuo.
d <- d %>%
  mutate(
    lahe_pred = fitted(m1),
    residuo   = resid(m1)
  )

# Las condiciones de primer orden de MCO implican que los residuos tienen media
# cero y covarianza cero con el regresor (son cero salvo error de redondeo).
mean(d$residuo)
cov(d$residuo, d$yrseduc)

# (e) Recta de regresion sobre el grafico de medias por ano de educacion.
# Un grafico guardado en un objeto se puede seguir ampliando con "+".
# Los puntos salen de "cef" (una fila por ano de educacion), pero la recta se estima
# con todas las observaciones de "d": por eso geom_smooth() recibe data = d.
# method = "lm" pide la recta de regresion lineal; se = FALSE la dibuja sin banda
# de confianza.
p_cef_recta <- p_cef +
  geom_smooth(data = d, aes(x = yrseduc, y = lahe), method = "lm", formula = y ~ x,
              se = FALSE, color = "#D62728", linewidth = 0.8)
p_cef_recta
ggsave("output/cef_lahe_educ_recta.png", p_cef_recta, width = 7, height = 4.5, dpi = 200)

# Prediccion del modelo para 12 y 16 anos de educacion.
# predict() evalua el modelo estimado en datos nuevos.
predict(m1, newdata = data.frame(yrseduc = c(12, 16)))


# =====================================================================
# 6. Regresion multiple y tabla de resultados
# =====================================================================

# (a) Se agregan regresores con "+".
m2 <- lm(lahe ~ yrseduc + female, data = d)
m3 <- lm(lahe ~ yrseduc + female + age, data = d)

# (b) Por que cambia (poco) el coeficiente de educacion al agregar female?
# Formula de variable omitida, que se cumple exactamente en la muestra:
#   coef. corto = coef. largo + (coef. de female en el largo) x (pendiente de female sobre yrseduc)
aux <- lm(female ~ yrseduc, data = d)
coef(m1)["yrseduc"]
coef(m2)["yrseduc"] + coef(m2)["female"] * coef(aux)["yrseduc"]

# (c) Tabla con los tres modelos.
# list() arma una lista de modelos; el nombre de cada elemento es el titulo de la columna.
modelos <- list("(1)" = m1, "(2)" = m2, "(3)" = m3)

# Nombres de fila para la tabla.
nombres <- c(
  "(Intercept)" = "Constante",
  "yrseduc"     = "Años de educación",
  "female"      = "Mujer",
  "age"         = "Edad"
)

# gof_map elige los estadisticos de ajuste a mostrar al pie de la tabla.
gof <- data.frame(
  raw   = c("nobs", "r.squared"),
  clean = c("N", "R2"),
  fmt   = c(0, 3)
)

# modelsummary() arma la tabla. vcov = "HC1" pide errores estandar robustos (van
# entre parentesis, debajo de cada coeficiente); fmt = 4 son los decimales.
# Sin el argumento output, la tabla se muestra en la consola.
modelsummary(modelos, vcov = "HC1", fmt = 4, coef_rename = nombres, gof_map = gof)

# output = "archivo.tex" la guarda como tabla de LaTeX, lista para pegar en un informe.
modelsummary(modelos, vcov = "HC1", fmt = 4, coef_rename = nombres, gof_map = gof,
             output = "output/reg_salarios.tex")

# output = "data.frame" la devuelve como tabla de datos, para guardarla en .csv y
# abrirla en Excel o Google Sheets.
tabla <- modelsummary(modelos, vcov = "HC1", fmt = 4, coef_rename = nombres, gof_map = gof,
                      output = "data.frame")
write.csv(tabla, "output/reg_salarios.csv", row.names = FALSE)


# =====================================================================
# 7. Practica adicional: MCO con matrices y regresiones por sexo
# =====================================================================

# cbind() arma la matriz X: una columna de unos (la constante) y los regresores.
X <- cbind(1, d$yrseduc, d$female, d$age)
y <- d$lahe

# t() traspone, %*% es el producto de matrices y solve() invierte una matriz.
# beta = (X'X)^{-1} X'y
beta <- solve(t(X) %*% X) %*% (t(X) %*% y)
beta
coef(m3) # mismos numeros que lm()

# Regresion simple por separado para hombres y para mujeres.
# filter() se queda con las filas que cumplen la condicion.
m_hombres <- lm(lahe ~ yrseduc, data = filter(d, female == 0))
m_mujeres <- lm(lahe ~ yrseduc, data = filter(d, female == 1))
modelsummary(list("Hombres" = m_hombres, "Mujeres" = m_mujeres),
             vcov = "HC1", fmt = 4, coef_rename = nombres, gof_map = gof)


cat("Coef. de yrseduc, regresion simple:", round(coef(m1)["yrseduc"], 4), "\n")
cat("Coef. de yrseduc, controlando por female:", round(coef(m2)["yrseduc"], 4), "\n")
cat("Coef. de female, controlando por yrseduc:", round(coef(m2)["female"], 4), "\n")
cat("Listo. Tablas y graficos en output/.\n")
