# Caso Hollywood Rules
# Preguntas 5 a 7

library(readxl)

datos <- read_excel("Hollywood.xls", sheet = "Exhibit 1")

head(datos)
names(datos)

# ============================================================
# PREGUNTA 5
# ============================================================

# 5a. Crear variable para identificar las comedias
datos$Comedia <- ifelse(datos$Genre == "Comedy", 1, 0)

# Revisar cantidad de comedias y no comedias
table(datos$Comedia)

# 5a. Modelo de regresion con factores previos a la produccion

modelo5 <- lm(
  `Total U.S. Gross` ~ Budget + Comedia + MPAA_D + Sequel + `Known Story`,
  data = datos
)

summary(modelo5)

# 5b. Eliminacion progresiva de variables no significativas

modelo5_b1 <- lm(
  `Total U.S. Gross` ~ Budget + Comedia + Sequel + `Known Story`,
  data = datos
)

summary(modelo5_b1)

# Segundo ajuste: eliminar Known Story

modelo5_b2 <- lm(
  `Total U.S. Gross` ~ Budget + Comedia + Sequel,
  data = datos
)

summary(modelo5_b2)

# Modelo final del punto 5
modelo5_final <- modelo5_b2

summary(modelo5_final)

# 5c. Efecto de ser una secuela
coef_sequel <- coef(modelo5_final)["Sequel"]

coef_sequel
coef_sequel / 1000000


# Modelo final del punto 5
modelo5_final <- modelo5_b2

summary(modelo5_final)

# 5b. El modelo final conserva Budget, Comedia y Sequel,
# ya que todas son significativas al nivel del 10%.
# MPAA_D y Known Story fueron eliminadas por no ser significativas.

# 5c. Efecto de ser una secuela
coef_sequel <- coef(modelo5_final)["Sequel"]

coef_sequel
coef_sequel / 1000000

# 5c. Manteniendo constantes el presupuesto y el genero,
# las secuelas tienen en promedio un Total U.S. Gross
# aproximadamente 31.67 millones de dolares mayor que
# las peliculas que no son secuelas.

par(mfrow = c(2, 2))
plot(modelo5_final)

par(mfrow = c(1, 1))

modelo5_final <- lm(
  `Total U.S. Gross` ~ Budget + Comedia + Sequel,
  data = datos
)

shapiro.test(residuals(modelo5_final))

cooks <- cooks.distance(modelo5_final)

which(cooks > 4 / nrow(datos))

max(cooks)

modelo5_log <- lm(
  log(`Total U.S. Gross`) ~ Budget + Comedia + Sequel,
  data = datos
)

summary(modelo5_log)

par(mfrow = c(2, 2))
plot(modelo5_log)
par(mfrow = c(1, 1))

# ============================================================
# AJUSTE DEL PUNTO 5 - MODELO LOGARITMICO
# ============================================================

# 5a. Modelo logaritmico inicial con todos los factores
modelo5_log_completo <- lm(
  log(`Total U.S. Gross`) ~ Budget + Comedia + MPAA_D +
    Sequel + `Known Story`,
  data = datos
)

summary(modelo5_log_completo)

# Primer ajuste del modelo logaritmico: eliminar MPAA_D

modelo5_log_b1 <- lm(
  log(`Total U.S. Gross`) ~ Budget + Comedia + Sequel + `Known Story`,
  data = datos
)

summary(modelo5_log_b1)

# Segundo ajuste del modelo logaritmico: eliminar Known Story

modelo5_log_final <- lm(
  log(`Total U.S. Gross`) ~ Budget + Comedia + Sequel,
  data = datos
)

summary(modelo5_log_final)

par(mfrow = c(2, 2))
plot(modelo5_log_final)
par(mfrow = c(1, 1))

summary(modelo5_log_final)

# 5b. Modelo final
# Se conservan Budget, Comedia y Sequel porque son significativas
# al nivel del 10%. MPAA_D y Known Story fueron eliminadas.

# 5c. Efecto de ser una secuela
beta_sequel <- coef(modelo5_log_final)["Sequel"]

efecto_sequel <- (exp(beta_sequel) - 1) * 100

beta_sequel
efecto_sequel

# 5c. Manteniendo constantes el presupuesto y el genero,
# una pelicula que es secuela presenta un Total U.S. Gross esperado
# aproximadamente 76.81% mayor que una pelicula que no es secuela.

# Se utiliza el logaritmo de Total U.S. Gross porque los diagnosticos
# del modelo original mostraban problemas de heterocedasticidad y
# desviaciones en los residuos. La transformacion logaritmica mejora
# el comportamiento de los supuestos del modelo lineal.

# ============================================================
# PREGUNTA 6
# ============================================================

# 6a. Modelo inicial para predecir Opening Gross

modelo6_completo <- lm(
  `Opening Gross` ~ Budget + Comedia + MPAA_D + Sequel +
    `Known Story` + Summer + Holiday + Christmas +
    `Opening Theatres`,
  data = datos
)

summary(modelo6_completo)

# Diagnosticos del modelo 6 original

par(mfrow = c(2, 2))
plot(modelo6_completo)
par(mfrow = c(1, 1))
# Modelo logaritmico completo para comparar

modelo6_log_completo <- lm(
  log(`Opening Gross`) ~ Budget + Comedia + MPAA_D + Sequel +
    `Known Story` + Summer + Holiday + Christmas +
    `Opening Theatres`,
  data = datos
)

summary(modelo6_log_completo)

par(mfrow = c(2, 2))
plot(modelo6_log_completo)
par(mfrow = c(1, 1))

# 6b. Primer ajuste: eliminar Holiday

modelo6_log_b1 <- lm(
  log(`Opening Gross`) ~ Budget + Comedia + MPAA_D + Sequel +
    `Known Story` + Summer + Christmas + `Opening Theatres`,
  data = datos
)

summary(modelo6_log_b1)

sort(
  coef(summary(modelo6_log_b1))[, "Pr(>|t|)"],
  decreasing = TRUE
)

# Segundo ajuste: eliminar Comedia

modelo6_log_b2 <- lm(
  log(`Opening Gross`) ~ Budget + MPAA_D + Sequel +
    `Known Story` + Summer + Christmas + `Opening Theatres`,
  data = datos
)

summary(modelo6_log_b2)

sort(
  coef(summary(modelo6_log_b2))[, "Pr(>|t|)"],
  decreasing = TRUE
)

# Tercer ajuste: eliminar MPAA_D

modelo6_log_b3 <- lm(
  log(`Opening Gross`) ~ Budget + Sequel +
    `Known Story` + Summer + Christmas + `Opening Theatres`,
  data = datos
)

summary(modelo6_log_b3)

sort(
  coef(summary(modelo6_log_b3))[, "Pr(>|t|)"],
  decreasing = TRUE
)

# Cuarto ajuste: eliminar Known Story

modelo6_log_b4 <- lm(
  log(`Opening Gross`) ~ Budget + Sequel +
    Summer + Christmas + `Opening Theatres`,
  data = datos
)

summary(modelo6_log_b4)

sort(
  coef(summary(modelo6_log_b4))[, "Pr(>|t|)"],
  decreasing = TRUE
)

# Quinto ajuste: eliminar Christmas

modelo6_log_final <- lm(
  log(`Opening Gross`) ~ Budget + Sequel +
    Summer + `Opening Theatres`,
  data = datos
)

summary(modelo6_log_final)

sort(
  coef(summary(modelo6_log_final))[, "Pr(>|t|)"],
  decreasing = TRUE
)

modelo6_log_final <- lm(
  log(`Opening Gross`) ~ Budget + Sequel +
    Summer + `Opening Theatres`,
  data = datos
)

# ============================================================
# 6c. Interpretacion de los coeficientes
# ============================================================

coef(modelo6_log_final)

# Efecto de aumentar el presupuesto en 1 millon de dolares
beta_budget <- coef(modelo6_log_final)["Budget"]

efecto_budget_1m <- (exp(beta_budget * 1000000) - 1) * 100

efecto_budget_1m

# Efecto de ser una secuela
beta_sequel_6 <- coef(modelo6_log_final)["Sequel"]

efecto_sequel_6 <- (exp(beta_sequel_6) - 1) * 100

efecto_sequel_6

# Efecto de estrenarse en verano
beta_summer <- coef(modelo6_log_final)["Summer"]

efecto_summer <- (exp(beta_summer) - 1) * 100

efecto_summer

# Efecto del numero de teatros

nombre_teatros <- grep(
  "Opening Theatres",
  names(coef(modelo6_log_final)),
  value = TRUE
)

beta_teatros <- coef(modelo6_log_final)[nombre_teatros]

beta_teatros

# ============================================================
# 6d. Efecto de aumentar 100 teatros
# ============================================================

# Estimacion puntual del cambio porcentual
cambio_100_teatros <- (exp(100 * beta_teatros) - 1) * 100

cambio_100_teatros

# Intervalo de confianza del 95% para el coeficiente de teatros
ic_beta_teatros <- confint(
  modelo6_log_final,
  parm = nombre_teatros,
  level = 0.95
)

ic_beta_teatros

# Convertir el intervalo al cambio porcentual por 100 teatros
ic_cambio_100 <- (exp(100 * ic_beta_teatros) - 1) * 100

ic_cambio_100

# 6d. Manteniendo constantes Budget, Sequel y Summer,
# un aumento de 100 teatros se asocia con un incremento esperado
# de aproximadamente 5.03% en el Opening Gross.
#
# El intervalo de confianza del 95% indica que el incremento esperado
# se encuentra aproximadamente entre 3.55% y 6.52%.

# ============================================================
# CONCLUSIONES PUNTO 6
# ============================================================

# 6b. El modelo final conserva Budget, Sequel, Summer y
# Opening Theatres, ya que todas son significativas al 10%.
# Se eliminaron Holiday, Comedia, MPAA_D, Known Story y Christmas.

# 6c. Interpretacion de los coeficientes:

# Budget:
# Manteniendo constantes las demas variables, un aumento de
# 1 millon de dolares en el presupuesto se asocia con un aumento
# aproximado de 0.51% en el Opening Gross esperado.

# Sequel:
# Manteniendo constantes las demas variables, una pelicula que
# es secuela presenta un Opening Gross esperado aproximadamente
# 43.62% mayor que una pelicula que no es secuela.

# Summer:
# Manteniendo constantes las demas variables, una pelicula estrenada
# en verano presenta un Opening Gross esperado aproximadamente
# 20.84% menor que una pelicula que no se estrena en verano.

# Opening Theatres:
# Manteniendo constantes las demas variables, cada teatro adicional
# se asocia aproximadamente con un aumento de 0.049% en el
# Opening Gross esperado.

# 6d. Un aumento de 100 teatros se asocia con un incremento esperado
# de aproximadamente 5.03% en el Opening Gross.
# El intervalo de confianza del 95% para este incremento se encuentra
# aproximadamente entre 3.55% y 6.52%.

# ============================================================
# PREGUNTA 7
# ============================================================

# 7a. Regresion lineal simple entre Opening Gross
# y Total U.S. Gross

modelo7_simple <- lm(
  `Total U.S. Gross` ~ `Opening Gross`,
  data = datos
)

summary(modelo7_simple)

# 7b. Si el Opening Gross representa el 25% del Total U.S. Gross,
# entonces:
# Opening Gross = 0.25 * Total U.S. Gross
# Por lo tanto:
# Total U.S. Gross = 4 * Opening Gross
# La pendiente teorica deberia ser igual a 4.

pendiente_teorica <- 1 / 0.25

pendiente_teorica

coef(modelo7_simple)

confint(
  modelo7_simple,
  "Opening Gross",
  level = 0.95
)

# Corregir nombre del coeficiente de Opening Gross

nombre_opening <- grep(
  "Opening Gross",
  names(coef(modelo7_simple)),
  value = TRUE
)

nombre_opening

# Intervalo de confianza del 95% para la pendiente

confint(
  modelo7_simple,
  parm = nombre_opening,
  level = 0.95
)

# 7c. La pendiente estimada es aproximadamente 3.12.
# El intervalo de confianza del 95% para la pendiente va
# aproximadamente de 2.69 a 3.56.
#
# Como el valor teorico de 4 no pertenece a este intervalo,
# se rechaza la hipotesis de que la pendiente sea igual a 4.
# Por lo tanto, con este modelo se rechaza la regla segun la cual
# el Opening Gross representa exactamente el 25% del Total U.S. Gross.

# ============================================================
# 7d. Critica del analisis anterior
# ============================================================

# La regla del 25% implica una relacion exacta:
# Total U.S. Gross = 4 * Opening Gross.
#
# Por lo tanto, no solamente supone una pendiente igual a 4,
# sino tambien un intercepto igual a cero.
#
# La regresion anterior permite un intercepto diferente de cero,
# por lo que comprobar solamente si la pendiente es igual a 4
# no representa completamente la regla que se quiere evaluar.

# Diagnosticos del modelo lineal simple

par(mfrow = c(2, 2))
plot(modelo7_simple)
par(mfrow = c(1, 1))
summary(modelo7_simple)$coefficients

# ============================================================
# 7e. Modelo sin intercepto
# ============================================================

modelo7_final <- lm(
  `Total U.S. Gross` ~ 0 + `Opening Gross`,
  data = datos
)

summary(modelo7_final)

# ============================================================
# 7f. Evaluar nuevamente la regla del 25%
# H0: pendiente = 4
# H1: pendiente diferente de 4
# ============================================================

nombre_opening_final <- grep(
  "Opening Gross",
  names(coef(modelo7_final)),
  value = TRUE
)

beta_7_final <- coef(modelo7_final)[nombre_opening_final]

se_7_final <- summary(modelo7_final)$coefficients[
  nombre_opening_final,
  "Std. Error"
]

t_7_final <- (beta_7_final - 4) / se_7_final

gl_7_final <- df.residual(modelo7_final)

pvalor_7_final <- 2 * pt(
  -abs(t_7_final),
  df = gl_7_final
)

beta_7_final
se_7_final
t_7_final
pvalor_7_final

# ============================================================
# 7g. Proporcion de variacion explicada
# ============================================================

r2_7_final <- summary(modelo7_final)$r.squared

r2_7_final

r2_7_final * 100

# 7e. Dado que el intercepto del modelo simple no fue
# estadisticamente significativo, se estima un nuevo modelo
# sin intercepto, obligando a que la recta pase por el origen.

# Modelo final:
# Total U.S. Gross = beta * Opening Gross

# 7f. El p-value obtenido es 1.861334e-07,
# claramente menor que 0.05.
#
# Por lo tanto, se rechaza H0: beta = 4.
# Incluso utilizando el modelo sin intercepto, existe evidencia
# estadistica para rechazar la regla de que el Opening Gross
# corresponde exactamente al 25% del Total U.S. Gross.

# 7g. El R-cuadrado del modelo sin intercepto es 0.916965.
# Por lo tanto, aproximadamente el 91.70% de la variacion
# del Total U.S. Gross es explicada por el Opening Gross.

# Nota: al tratarse de una regresion sin intercepto,
# R utiliza un R-cuadrado no centrado (uncentered R-squared).