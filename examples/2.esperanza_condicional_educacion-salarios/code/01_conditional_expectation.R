# Esperanza condicional del log salario dado los anios de escolaridad (CASEN 2024).
# Reproduce, con datos actuales, las 3 figuras de la slide de "Educacion y
# salarios en Chile" del deck 2.esperanza_condicional: (1) esperanza
# condicional sola, (2) + regresion lineal, (3) + regresion cuadratica.
#
# Poblacion: hombres entre 30 y 65 anios que trabajan en Chile -- mismo recorte
# de edad que el ejemplo de retornos a la educacion de 1.intro (consistencia
# entre los ejemplos del curso). No se restringe a 40-49 como la figura de
# comparacion de EEUU (AP, Censo 1980): esa franja angosta es un artefacto de
# una muestra vieja y mas chica, no algo que valga la pena imitar con Casen
# 2024. Se deja solo hombres (no ambos sexos) para no mezclar en esta slide
# el problema de seleccion muestral por participacion laboral femenina.
# Variables: esc (anios de escolaridad), ytrabajocor (ingreso del trabajo
# corregido), edad, sexo.

# --- Estructura de carpetas esperada ---
# Este script asume que existe una carpeta de proyecto con esta estructura:
#
#   2.esperanza_condicional_educacion-salarios/
#     code/
#       01_conditional_expectation.R   <- este archivo
#     input/
#       casen_2024_subset.rds           <- datos de entrada
#     output/                           <- se crea sola al correr el script (graficos van aca)
#
# Si no la tenes armada: crea la carpeta "2.esperanza_condicional_educacion-salarios"
# en tu compu, adentro crea "code/" e "input/", pone este script en code/ y el
# archivo .rds en input/. output/ no hace falta crearla, el script la crea sola.

# Reemplazar por la ruta a la carpeta "2.esperanza_condicional_educacion-salarios" en TU computadora.
dir_proyecto <- "/Users/ramirodeelejalde/Dropbox/Teaching/Econometria I_MAE/econometria_mae/examples/2.esperanza_condicional_educacion-salarios"
setwd(dir_proyecto) # fija la carpeta de trabajo: de aca en mas, "input/..." y "output/..." apuntan siempre ahi

rm(list = ls()) # borra todos los objetos que pudieran quedar de una sesion anterior, para partir de cero

# library(x) carga un paquete (una libreria de funciones adicionales) para poder usarlo.
# Hay que haberlo instalado antes una vez con install.packages("x").
library(tidyverse)    # incluye dplyr (filter, mutate, group_by, summarise, %>%) y ggplot2 (graficos)
library(haven)        # para sacarle las etiquetas Stata (haven_labelled) a las variables antes de compararlas
library(kableExtra)   # arma la tabla de la Ilustracion de la LEI en LaTeX (booktabs)

options("modelsummary_factory_latex" = "kableExtra")   # booktabs simple, sin tabularray/siunitx
options("modelsummary_format_numeric_latex" = "plain") # sin \num{}, no requiere siunitx

dir.create("output", showWarnings = FALSE) # crea la carpeta output/ si todavia no existe

# readRDS() lee un archivo de datos en formato .rds (el formato nativo de R).
d <- readRDS("input/casen_2024_subset.rds")
d$sexo <- zap_labels(d$sexo) # saca la etiqueta Stata (haven_labelled) y deja un numero comun
d$e6a <- zap_labels(d$e6a)
d$e6c_completo <- zap_labels(d$e6c_completo)
d$e8 <- zap_labels(d$e8)

# --- Muestra: hombres 30-65 anios que trabajan, con escolaridad hasta 21 anios ---
# (a partir de 22 anios quedan muy pocas observaciones por valor de esc, y el
# promedio se vuelve muy ruidoso; ademas asi el eje x queda comparable con la
# figura de EEUU de la slide, que llega hasta "20+").
sample <- d %>%
  filter(
    edad >= 30, edad <= 65,        # mismo recorte de edad que el ejemplo de 1.intro
    sexo == 1,                     # solo hombres (ver nota arriba sobre seleccion muestral)
    !is.na(esc), !is.na(ytrabajocor), ytrabajocor > 0,
    esc <= 21
  ) %>%
  mutate(lwage = log(ytrabajocor)) # logaritmo natural del ingreso

cat("N muestra (hombres 30-65, esc<=21):", nrow(sample), "\n")

# --- Tabla CEF: salario promedio segun graduado universitario o no ---
# Se arma con la misma poblacion (hombres 30-65 que trabajan), pero sin el
# tope esc<=21 (ese tope era solo para el eje x del grafico, no aplica aca).
#
# "Graduado universitario" se define con 3 variables de la CASEN (ver libro de
# codigos), no con un corte arbitrario de anios de escolaridad (esc):
#   - e6a %in% 13:15: el nivel educacional mas alto alcanzado es "Profesional
#     (carreras de 4 o mas anios)" (13), "Magister" (14) o "Doctorado" (15).
#     e6a guarda el nivel MAS ALTO, asi que quien tiene postgrado aparece con
#     14 o 15 y no con 13: hay que incluir esos dos codigos para no dejar a los
#     postgraduados en el grupo de no universitarios. Queda excluido tecnico de
#     nivel superior (carreras de 1 a 3 anios, e6a==12).
#   - e6c_completo == 1: completo ese nivel (no lo dejo trunco).
#   - e8 %in% c(3,4,5,7): la institucion de educacion superior fue una
#     universidad (privada CRUCH o no-CRUCH, estatal, o extranjera) -- esto
#     excluye Centro de Formacion Tecnica (e8==1) e Instituto Profesional
#     (e8==2), que tambien pueden otorgar titulos "Profesional" de 4+ anios.
sample_cef <- d %>%
  filter(
    edad >= 30, edad <= 65,
    sexo == 1,
    !is.na(esc), !is.na(ytrabajocor), ytrabajocor > 0
  ) %>%
  mutate(universitario = if_else(e6a %in% 13:15 & e6c_completo == 1 & e8 %in% c(3, 4, 5, 7), 1, 0))

cef_tab <- sample_cef %>%
  group_by(universitario) %>%
  summarise(wage_k = mean(ytrabajocor) / 1000, n = n(), .groups = "drop") %>% # wage_k: salario promedio en miles de $ mensuales
  mutate(pr = n / sum(n))

cat("Tabla CEF (universitario 0/1 - salario en miles de $, Pr):\n")
print(cef_tab)

# --- output/tabla_cef_universitario.tex: version LaTeX de la tabla de arriba, para \input{} en el deck ---
cef_display <- tibble(
  fila = c("$universitario = 0$", "$universitario = 1$"),
  wage = formatC(cef_tab$wage_k, format = "f", digits = 0, big.mark = ","),
  pr = formatC(cef_tab$pr, format = "f", digits = 2)
)
names(cef_display) <- c("", "$\\E(\\text{salario}|universitario)$", "$\\Pr(universitario)$")

cef_tex <- kbl(cef_display, format = "latex", booktabs = TRUE, escape = FALSE, align = "rcc")
writeLines(as.character(cef_tex), "output/tabla_cef_universitario.tex")

# Misma tabla sin formatear (numeros planos), para abrir en Excel/Sheets sin compilar LaTeX
write.csv(cef_tab, "output/tabla_cef_universitario.csv", row.names = FALSE)

# --- Esperanza condicional: promedio de lwage para cada valor exacto de esc ---
# group_by(esc) agrupa las filas por cada valor de escolaridad; summarise()
# calcula un resumen (aca, la media de lwage) para cada grupo por separado.
cond_exp <- sample %>%
  group_by(esc) %>%
  summarise(lwage_mean = mean(lwage), n = n(), .groups = "drop")

# --- Regresiones para overlay: lineal y cuadratica en esc ---
# lm(y ~ x, data=...) estima una regresion lineal (OLS) de y sobre x.
reg_lineal <- lm(lwage ~ esc, data = sample)
# esc + I(esc^2): agrega el cuadrado de esc como regresor (I() evita que ^2 se
# interprete como notacion de formula en vez de una operacion aritmetica).
reg_cuadratica <- lm(lwage ~ esc + I(esc^2), data = sample)

# predict() usa los coeficientes estimados para calcular el valor ajustado de
# la regresion en cada valor de esc de la tabla cond_exp (0 a 21).
cond_exp <- cond_exp %>%
  mutate(
    fit_lineal = predict(reg_lineal, newdata = data.frame(esc = esc)),
    fit_cuadratica = predict(reg_cuadratica, newdata = data.frame(esc = esc))
  )

# La descripcion de la muestra ya va en el \caption{} de LaTeX debajo de cada
# figura en la slide, asi que no se repite tambien adentro del grafico.

# --- Figura 1: esperanza condicional sola ---
p1 <- ggplot(cond_exp, aes(x = esc, y = lwage_mean)) +
  geom_line(color = "#1F77B4", linewidth = 0.8) +
  geom_point(color = "#1F77B4", size = 1.6) +
  labs(
    title = "Esperanza condicional",
    subtitle = "log(wage) según nivel educativo",
    x = "Años de educación (esc)", y = "log(wage)"
  ) +
  theme_minimal(base_size = 13)
ggsave("output/cond_expec_wage_educ1.png", p1, width = 7, height = 4.5, dpi = 200)

# --- Figura 2: esperanza condicional + regresion lineal ---
# pivot_longer junta las columnas lwage_mean y fit_lineal en una sola columna
# "value", con otra columna "serie" que dice de cual venia cada fila; asi
# ggplot puede dibujar ambas series con un solo geom_line + color=serie
# (y arma la leyenda automaticamente).
d2 <- cond_exp %>%
  select(esc, lwage_mean, fit_lineal) %>%
  pivot_longer(cols = c(lwage_mean, fit_lineal), names_to = "serie", values_to = "value") %>%
  mutate(serie = recode(serie, lwage_mean = "Esperanza condicional", fit_lineal = "Regresión lineal"))

p2 <- ggplot(d2, aes(x = esc, y = value, color = serie)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = filter(d2, serie == "Conditional expectation"), size = 1.6) +
  scale_color_manual(values = c("Esperanza condicional" = "#1F77B4", "Regresión lineal" = "#D62728")) +
  labs(
    title = "Esperanza condicional y modelo de regresión lineal",
    subtitle = "log(wage) según nivel educativo",
    x = "Años de educación (esc)", y = "log(wage)", color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
ggsave("output/cond_expec_wage_educ2.png", p2, width = 7, height = 4.5, dpi = 200)

# --- Figura 3: esperanza condicional + regresion cuadratica ---
d3 <- cond_exp %>%
  select(esc, lwage_mean, fit_cuadratica) %>%
  pivot_longer(cols = c(lwage_mean, fit_cuadratica), names_to = "serie", values_to = "value") %>%
  mutate(serie = recode(serie, lwage_mean = "Esperanza condicional", fit_cuadratica = "Regresión cuadrática"))

p3 <- ggplot(d3, aes(x = esc, y = value, color = serie)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = filter(d3, serie == "Conditional expectation"), size = 1.6) +
  scale_color_manual(values = c("Esperanza condicional" = "#1F77B4", "Regresión cuadrática" = "#D62728")) +
  labs(
    title = "Esperanza condicional y modelo de regresión cuadrática",
    subtitle = "log(wage) según nivel educativo",
    x = "Años de educación (esc)", y = "log(wage)", color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
ggsave("output/cond_expec_wage_educ3.png", p3, width = 7, height = 4.5, dpi = 200)

cat("Listo. Graficos en output/.\n")
