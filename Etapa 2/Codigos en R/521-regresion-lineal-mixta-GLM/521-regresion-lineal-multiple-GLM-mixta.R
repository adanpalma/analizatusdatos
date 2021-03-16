# librerias necesarias
library(ggplot2)
library(ggpubr)
library(plotly)
library(car)

# leer y reconfigurar la tabla de datos
data(mtcars)
df <- mtcars[,c("wt","disp","hp","mpg","vs","am")]
df[,"am"] <- factor(df[,"am"])
df[,"vs"] <- factor(df[,"vs"])
df[,4] <- 1/df[,4] # estoy calculando el consumo
names(df) <- c("peso","cilindrada","Caballos","consumo","vs","am")
    # La variable x1 es el peso
    # La variable x2 es la cilindrada
    # La variable x3 son los caballos
    # La variable y es el consumo

# Descripción
pairs(df)


# modelo1 - con vs
modelo <- glm(formula = consumo ~ peso + Caballos + vs ,family = gaussian, df)
summary(modelo)

par(mfrow = c(2, 2))
plot(modelo)


# modelo2 con am
modelo <- glm(formula = consumo ~ peso + Caballos + am ,family = gaussian, df)
summary(modelo)

par(mfrow = c(2, 2))
plot(modelo)