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

