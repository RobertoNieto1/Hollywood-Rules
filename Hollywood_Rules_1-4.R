library(readxl)
Hollywood <- read_excel("GitHub/Hollywood-Rules/Hollywood.xls", sheet = "Exhibit 1")

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

# Pregunta 3

# 3a. Comparar Total U.S. Gross entre comedias y no comedias
datos$Comedia <- datos$Genre == "Comedy"

t.test(`Total U.S. Gross` ~ Comedia, data = datos)
# 3a: No hay diferencia estadísticamente significativa entre comedias y no comedias.



# 3b. Comparar ROI entre comedias y no comedias
t.test(ROI ~ Comedia, data = datos)
# 3b: Sí hay diferencia estadísticamente significativa en el ROI entre comedias y no comedias.

# Pregunta 4

# 4a. Comparar Total U.S. Gross entre películas R-rated y no R-rated
datos$R_Rated <- datos$MPAA_D == 1

t.test(`Total U.S. Gross` ~ R_Rated, data = datos)
#4a: No hay diferencia estadísticamente significativa entre películas R-rated y no R-rated.