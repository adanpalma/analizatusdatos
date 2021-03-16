# importar datos
datos <- mtcars


# seleccionamos dos variables cuantitativas + factor en el mismo data.frame
df <- datos[c(1,3,4)]


library(ggpubr)
# diagrama de dispersion con un df de dos variables con linea de regresion y intervalos de confianza
name1 <- names(df)[1]
name2 <- names(df)[2]
sizename <- names(df)[3]
ggscatter(df, x = name1, y = name2,size=sizename,
          xlab = "Título Eje X", ylab = "Título Eje Y")
