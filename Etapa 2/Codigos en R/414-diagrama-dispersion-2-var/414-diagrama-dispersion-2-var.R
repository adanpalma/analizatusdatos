# importar datos
datos <- mtcars

# seleccionamos dos variables cuantitativas
df <- datos[c(1,6)]


# diagrama de dispersión con la función base y el data frame de las dos variables
plot(df,main = "Título del Gráfico" ,xlab = "Título Eje X",ylab = "Título Eje Y")


library(ggpubr)
# diagrama de dispersion con un df de dos variables con linea de regresion y intervalos de confianza
name1 <- names(df)[1]
name2 <- names(df)[2]
ggscatter(df, x = name1, y = name2, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Título Eje X", ylab = "Título Eje Y")



library(ggpubr)
# diagrama de dispersion con un df de dos variables
name1 <- names(df)[1]
name2 <- names(df)[2]
ggscatter(df, x = name1, y = name2, 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Título Eje X", ylab = "Título Eje Y")