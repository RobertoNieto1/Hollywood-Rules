# Caso Hollywood Rules
# Preguntas 5 a 7

library(readxl)

datos <- read_excel(
  "Hollywood.xls",
  sheet = "Exhibit 1"
)

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