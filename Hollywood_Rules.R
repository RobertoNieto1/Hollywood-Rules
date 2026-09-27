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
