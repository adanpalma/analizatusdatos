# importar datos
datos <- mtcars

# seleccionamos dos variables cuantitativas
df <- datos[c(1,6)]
dfactor <- as.factor(datos[,9])


# diagrama de dispersión con la función base y el data frame de las dos variables
plot(df,main = "Título del Gráfico" ,xlab = "Título Eje X",ylab = "Título Eje Y",col=dfactor)
legend("bottomright",c("manual", "auto"),fill = c("red","black"))




# seleccionamos dos variables cuantitativas + factor en el mismo data.frame
df <- datos[c(1,6,9)]
df[,3] <- as.factor(df[,3])


library(ggpubr)
# diagrama de dispersion con un df de dos variables con linea de regresion y intervalos de confianza
name1 <- names(df)[1]
name2 <- names(df)[2]
factorname <- names(df)[3]
ggscatter(df, x = name1, y = name2,color=factorname,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Título Eje X", ylab = "Título Eje Y")



library(ggpubr)
# diagrama de dispersion con un df de dos variables
name1 <- names(df)[1]
name2 <- names(df)[2]
ggscatter(df, x = name1, y = name2,color=factorname,
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Título Eje X", ylab = "Título Eje Y")