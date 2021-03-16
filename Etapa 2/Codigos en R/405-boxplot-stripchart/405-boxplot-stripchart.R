
# importar datos
datos <- iris


# seleccionamos una variable en forma de vector (valores)
dvector = datos[,4]



# boxplot + stripchart
boxplot(dvector)
stripchart(t(dvector),vertical = TRUE,pch=19,add=TRUE)



# boxplot con más cositas
boxplot(dvector, main = "Título del Gráfico",xlab = "Título Eje X",ylab = "Título Eje Y")
stripchart(t(dvector),vertical = TRUE,pch=19,add=TRUE)



library(ggpubr)
# boxplot con ggpubr (se necesita leer en forma de data.frame de una variable)

#...leemos la variables
nombres <- names(datos) # nombres de las variables
sel = 3 # seleccionamos la columna 3
df = as.data.frame(datos[,sel])
names(df)=nombres[sel]



#...hacemos el boxplot + stripchart (jitter)
name1 <- names(df)
ggboxplot(df,y = name1,color = "#00AFBB",add = "jitter")+
  ggtitle("Título del gráfico") + xlab("Título Eje X") + ylab("Título Eje y")






library(plotly)

# boxplot con plotly (se necesita leer en forma de data.frame de una variable)

#...leemos la variables
nombres <- names(datos) # nombres de las variables
sel = 3 # seleccionamos la columna 3
df = as.data.frame(datos[,sel])
names(df)=nombres[sel]

#...hacemos el Boxplot con plotly
name1 <- names(df)
plot_ly(y = df[,name1], type = "box",name = name1,boxpoints = "all")%>%
  layout(title = "Título del Gráfico",yaxis = list(title = "Título Eje Y"),xaxis = list(title = "Título Eje X"))

