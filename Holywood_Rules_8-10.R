# Caso Hollywood Rules
# Preguntas 8 a 10

library(tidyverse)
library(dplyr)
library(readxl)

# Cargar Datos
datos <- read_excel("Hollywood.xls", sheet = "Exhibit 1") 

#Ajustar la base: Limpiar espacios, renombrar la columna de critica y crear Comedia
# Se conservan los nombres originales de las demas variables.
datos <- datos %>%
  rename_with(trimws) %>% 
  rename(Critics_Opinion = starts_with("Critics")) %>%
  mutate(Comedia = if_else(Genre == "Comedy", 1, 0))

names(datos)

# ============================================================
# PREGUNTA 8
# ============================================================

# 8a. Modelo de regresion con todos los factores conocidos antes
# de la produccion, antes del estreno y despues del fin de semana
# de estreno (incluye Opening Gross y Critics_Opinion)

modelo8 <- lm(`Total U.S. Gross` ~ Budget + Comedia + MPAA_D + Sequel + 
              `Known Story` + Summer + Holiday + Christmas + 
              `Opening Theatres` + `Opening Gross` + Critics_Opinion, data = datos)

summary(modelo8)

# 8b. Eliminacion progresiva de variables no significativas
# al nivel del 10% (se elimina la de mayor p-valor en cada paso)

sort(coef(summary(modelo8))[, "Pr(>|t|)"], decreasing = TRUE)

# Primer ajuste: eliminar Holiday

modelo8_b1 <- lm(`Total U.S. Gross` ~ Budget + Comedia + MPAA_D + Sequel + `Known Story` + Summer + Christmas + `Opening Theatres` + `Opening Gross` + Critics_Opinion, data = datos)

summary(modelo8_b1)

sort(coef(summary(modelo8_b1))[, "Pr(>|t|)"], decreasing = TRUE)

# Segundo ajuste: eliminar Opening Theatres

modelo8_b2 <- lm(`Total U.S. Gross` ~ Budget + Comedia + MPAA_D + Sequel + `Known Story` + Summer + Christmas + `Opening Gross` + Critics_Opinion, data = datos)

summary(modelo8_b2)

sort(coef(summary(modelo8_b2))[, "Pr(>|t|)"], decreasing = TRUE)

# Tercer ajuste: eliminar Christmas

modelo8_b3 <- lm(`Total U.S. Gross` ~ Budget + Comedia + MPAA_D + Sequel + `Known Story` + Summer + `Opening Gross` + Critics_Opinion, data = datos)

summary(modelo8_b3)

sort(coef(summary(modelo8_b3))[, "Pr(>|t|)"], decreasing = TRUE)

# Cuarto ajuste: eliminar Sequel

modelo8_b4 <- lm(`Total U.S. Gross` ~ Budget + Comedia + MPAA_D + `Known Story` + Summer + `Opening Gross` + Critics_Opinion, data = datos)

summary(modelo8_b4)

sort(coef(summary(modelo8_b4))[, "Pr(>|t|)"], decreasing = TRUE)

# Quinto ajuste: eliminar Known Story

modelo8_b5 <- lm(`Total U.S. Gross` ~ Budget + Comedia + MPAA_D + Summer + `Opening Gross` + Critics_Opinion, data = datos)

summary(modelo8_b5)

sort(coef(summary(modelo8_b5))[, "Pr(>|t|)"], decreasing = TRUE)

# Sexto ajuste: eliminar Summer

modelo8_b6 <- lm(`Total U.S. Gross` ~ Budget + Comedia + MPAA_D + `Opening Gross` + Critics_Opinion, data = datos)

summary(modelo8_b6)

sort(coef(summary(modelo8_b6))[, "Pr(>|t|)"], decreasing = TRUE)

# Septimo ajuste: eliminar Comedia

modelo8_b7 <- lm(`Total U.S. Gross` ~ Budget + MPAA_D + `Opening Gross` + Critics_Opinion, data = datos)

summary(modelo8_b7)

sort(coef(summary(modelo8_b7))[, "Pr(>|t|)"], decreasing = TRUE)

# Modelo final del punto 8
modelo8_final <- modelo8_b7

summary(modelo8_final)

# 8b: El modelo final conserva Budget, MPAA_D, Opening Gross y
# Critics_Opinion, ya que todas son significativas al nivel del 10%.
# Se eliminaron Holiday, Opening Theatres, Christmas, Sequel,
# Known Story, Summer y Comedia por no ser significativas.


# 8c. Prediccion para una pelicula con las caracteristicas de Flags of Our Fathers

flags <- tibble(`Opening Gross` = 10245190, `Opening Theatres` = 1876, Budget = 90000000, `Known Story` = 1, Sequel = 0,
                Comedia = 0, Summer = 1, Holiday = 0, Christmas = 0, MPAA_D = 1, Critics_Opinion = 79)

# Estimacion puntual e intervalo de prediccion del 95%
pred_flags <- predict(modelo8_final, newdata = flags, interval = "prediction", level = 0.95)

pred_flags
pred_flags / 1000000

# Total U.S. Gross real de Flags of Our Fathers
33602376

# Verificar si el valor real cae dentro del intervalo
33602376 >= pred_flags[, "lwr"] & 33602376 <= pred_flags[, "upr"]

# Diferencia valor percibido - Valor estimado
33602376 - pred_flags [, "fit"]
(33602376 - pred_flags [, "fit"]) / 1000000

# 8c: Estimacion puntual aproximada: 57.65 millones de dolares.
# Intervalo de prediccion del 95%: aproximadamente entre
# 21.51 y 93.80 millones de dolares.
# El Total U.S. Gross real (33.60 millones) cae dentro del intervalo,
# aunque queda muy por debajo de la estimacion puntual, aproximadamente 
# 24 millones por debajo.

# ------------------------------------------------------------
# 8d. Cuanto invertir para subir 10 puntos la opinion de la critica
# ------------------------------------------------------------

beta_critics <- coef(modelo8_final)["Critics_Opinion"]

beta_critics

# Ingreso adicional esperado por 10 puntos extra (79 a 89)
beta_critics * 10
(beta_critics * 10) / 1000000

# Intervalo de confianza del 95% para el efecto de 10 puntos
ic_critics <- confint(modelo8_final, parm = "Critics_Opinion", level = 0.95)

ic_critics * 10
(ic_critics * 10) / 1000000

# 8d: Manteniendo constantes las demas variables, 10 puntos adicionales
# en Critics_Opinion se asocian con un aumento esperado de aproximadamente
# 5.91 millones de dolares en el Total U.S. Gross.
# Ese es el maximo que Griffith deberia estar dispuesto a pagar para
# lograr ese aumento; pagar mas que eso no se recupera.
# Critics_Opinion es significativa y su asociacion con la recaudacion
# es positiva. Esto no demuestra causalidad ni que los criticos sean
# responsables del resultado de Flags of Our Fathers.

# ============================================================
# PREGUNTA 9
# ============================================================

# 9a. Modificar el modelo del punto 8 agregando la interaccion
# Critics_Opinion x Comedia.
# Comedia y Critics_Opinion se mantienen en el modelo aunque
# no sean significativas, porque forman parte de la interaccion.

modelo9 <- lm(`Total U.S. Gross` ~ Budget + MPAA_D + `Opening Gross` + Critics_Opinion + Comedia + Critics_Opinion:Comedia, data = datos)

summary(modelo9)

# Coeficientes de interes
coef9 <- summary(modelo9)$coefficients

coef9["Critics_Opinion", ]
coef9["Critics_Opinion:Comedia", ]

# Pendiente de Critics_Opinion en no comedias
beta_critics_no_comedia <- coef(modelo9)["Critics_Opinion"]

# Pendiente de Critics_Opinion en comedias
beta_critics_comedia <- coef(modelo9)["Critics_Opinion"] + coef(modelo9)["Critics_Opinion:Comedia"]

beta_critics_no_comedia
beta_critics_comedia

# Prueba de una cola:
# H0: beta_interaccion >= 0
# H1: beta_interaccion < 0  (la critica pesa menos en comedias)

t_inter <- coef9["Critics_Opinion:Comedia", "t value"]

gl_9 <- df.residual(modelo9)

pvalor_inter_una_cola <- pt(t_inter, df = gl_9)

t_inter
pvalor_inter_una_cola

# 9a: El coeficiente de la interaccion es negativo (aprox. -228,000),
# pero no es significativo: p-valor de dos colas aprox. 0.44
# y de una cola aprox. 0.22, ambos mayores que 0.10.
# Por lo tanto, no se puede probar la teoria de Griffith: con estos
# datos no hay evidencia estadistica de que la opinion de la critica
# influya menos en las comedias que en las demas peliculas.

# ============================================================
# PREGUNTA 10
# ============================================================

# 10a: Si se agregara star power (p. ej. el pago a los actores) a la
# regresion del punto 9, la teoria de Griffith (lo que impulsa el
# Total U.S. Gross son los actores y no el presupuesto) se apoyaria si:
#
# 1. El coeficiente de star power fuera positivo y estadisticamente
#    significativo.
#
# 2. El coeficiente de Budget disminuyera hacia cero y perdiera
#    significancia, ya que las estrellas son una parte importante del
#    presupuesto y Budget y star power estan correlacionados
#    (multicolinealidad). Budget estaria capturando el efecto de las
#    estrellas.
#
# Si, por el contrario, Budget siguiera siendo significativo y star power
# no lo fuera, la teoria no se sostendria.
#
# Sin embargo hay una limitacion, que no hay datos de star power, por lo que
# esta conclusion es solo un razonamiento y no se puede comprobar.
