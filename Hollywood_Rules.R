datos <- readxl::read_excel("Hollywood.xls", sheet = 1)
# Revisar los primeros datos
head(datos)

# Revisar la estructura
str(datos)
# Pregunta 1

# Estadísticos de las variables solicitadas
summary(datos[, c("Opening Gross",
                  "Total U.S. Gross",
                  "Total Non-U.S. Gross",
                  "Opening Theatres")])

# Mínimo
sapply(datos[, c("Opening Gross",
                 "Total U.S. Gross",
                 "Total Non-U.S. Gross",
                 "Opening Theatres")], min)

# Promedio
sapply(datos[, c("Opening Gross",
                 "Total U.S. Gross",
                 "Total Non-U.S. Gross",
                 "Opening Theatres")], mean)

# Máximo
sapply(datos[, c("Opening Gross",
                 "Total U.S. Gross",
                 "Total Non-U.S. Gross",
                 "Opening Theatres")], max)

# Número de comedias
sum(datos$Genre == "Comedy")

# Número de películas R-rated
sum(datos$MPAA_D == 1)

# Pregunta 2

# 2a. Calcular ROI de cada película
datos$ROI <- (datos$`Total U.S. Gross` - datos$Budget) / datos$Budget

# Ver el ROI de cada película
datos[, c("Movie", "Budget", "Total U.S. Gross", "ROI")]

# ROI promedio
mean(datos$ROI)


# 2b. Intervalo de confianza del 95% para el ROI promedio
t.test(datos$ROI, conf.level = 0.95)


# 2c. Prueba de hipótesis: ¿el ROI promedio es mayor al 12%?
t.test(datos$ROI, mu = 0.12, alternative = "greater")