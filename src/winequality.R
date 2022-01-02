# Importar el dataset
wine_data <- read.csv("winequality-white.csv", sep = ";")
head(wine_data)

# Comprobar el número de registros y las clases de las columnas
sapply(wine_data, class)

# Comprobación inicial de outliers en cada variable
for(var in seq_len(ncol(wine_data))) {
  boxplot(wine_data[,var], xlab = names(wine_data)[var])
}

# Outliers en la variable "quality"
boxplot(wine_data$quality, main = "Calidad", ylab = "Puntuación", las=1)
barplot(table(wine_data$quality), xlab = "Calidad", ylab = "Nº valoraciones", las=1, col = gray.colors(7))

# Otros ejemplos de columnas con outliers a destacar
boxplot(wine_data$citric.acid, main = "Ácido cítrico", ylab = "g/dm3", las=1)
boxplot(wine_data$density, main = "Densidad", ylab = "g/cm3", las=1)
boxplot(wine_data$pH, main = "pH", las=1)

# Comprobación de normalidad para cada variable numérica
apply(wine_data, 2, shapiro.test)

# Comprobación de homocedasticidad
wine_data_hc <- apply(wine_data, 2, function (x)  fligner.test(x ~ quality, data=wine_data))
sapply(wine_data_hc, function (x) x$p.value)

# Comprobación de correlación con el resto de variables
cor(wine_data[-12], wine_data$quality)

# Contraste de hipótesis, Kruskal-Wallis
apply(wine_data, 2, function (x) kruskal.test(x ~ quality, data=wine_data))

# Regresión lineal
plot(wine_data$quality, wine_data$sulphates, xlab = "Calidad", ylab = "Sulfatos", las = 1)
plot(wine_data$quality, wine_data$citric.acid, xlab = "Calidad", ylab = "Ácido cítrico", las = 1)
summary(lm(quality ~ sulphates, data=wine_data))
summary(lm(quality ~ citric.acid, data=wine_data))

apply(wine_data, 2, function (x) summary(lm(quality ~ x, data=wine_data)))

# Correlación
cor.test(wine_data$quality,wine_data$sulphates, method="spearman")
cor.test(wine_data$quality,wine_data$citric.acid, method="spearman")

cor_data <- cor(wine_data, method = "spearman")
sort(cor_data["quality",])

# Visualización de resultados
wine_data_d <- wine_data[wine_data$density<1.01,]
plot(wine_data_d$quality, wine_data_d$density, xlab = "Calidad", ylab = "Densidad", las = 1)
plot(wine_data$quality, wine_data$chlorides, xlab = "Calidad", ylab = "Cloruros", las = 1)
plot(wine_data$quality, wine_data$alcohol, xlab = "Calidad", ylab = "Alcohol", las = 1)


