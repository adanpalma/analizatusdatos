
# importar datos
datos <- iris


# seleccionamos una variable en forma de vector (valores)
dvector = iris[,4]



# boxplot
boxplot(dvector)



# boxplot con más cositas
boxplot(dvector, main = "Título del Gráfico",xlab = "Título Eje X",ylab = "Título Eje Y")





library(ggpubr)
# boxplot con ggpubr (se necesita leer en forma de data.frame de una variable)

#...leemos la variables
nombres <- names(datos) # nombres de las variables
sel = 3 # seleccionamos la columna 3
df = as.data.frame(datos[,sel])
names(df)=nombres[sel]


#...hacemos el boxplot
name1 <- names(df)
ggboxplot(df,y = name1,color = "#00AFBB")+
  ggtitle("Título del gráfico") + xlab("Título Eje X") + ylab("Título Eje y")

#...hacemos el boxplot
ggboxplot(df,y = name1,color = "#00AFBB",add = "dotplot")+
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
plot_ly(y = df[,name1], type = "box",name = name1)%>%
  layout(title = "Título del Gráfico",yaxis = list(title = "Título Eje Y"),xaxis = list(title = "Título Eje X"))

