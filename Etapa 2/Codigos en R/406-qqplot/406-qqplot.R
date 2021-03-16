
# importar datos
datos <- iris


# seleccionamos una variable en forma de vector (valores)
dvector = iris[,1]



# qqplot (normal distribution)
qqnorm(dvector, pch = 1, frame = FALSE)
qqline(dvector, col = "steelblue", lwd = 2)



library(car)
# qqplot paquete car
qqPlot(dvector)




library(ggpubr)
# qqplot con ggpubr (se necesita leer en forma de data.frame de una variable)

#...leemos la variables
nombres <- names(datos) # nombres de las variables
sel = 1 # seleccionamos la columna 1
df = as.data.frame(datos[,sel])
names(df)=nombres[sel]



#...hacemos el qqplot
name1 <- names(df)
ggqqplot(df, x = names(df),color = "#FF6666",add.params = list(color = "black"))+
  xlab("Distribución Normal") + ylab("Cuartiles reales") +
  theme_minimal() +
  ggtitle("Título del qqplot") +
  theme(plot.title = element_text(hjust = 0.5))

