
# importar datos
datos <- iris


# seleccionamos una variable en forma de vector (valores)
dvector = iris[,4]



# boxplot
boxplot(dvector)



# boxplot con m�s cositas
boxplot(dvector, main = "T�tulo del Gr�fico",xlab = "T�tulo Eje X",ylab = "T�tulo Eje Y")





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
  ggtitle("T�tulo del gr�fico") + xlab("T�tulo Eje X") + ylab("T�tulo Eje y")

#...hacemos el boxplot
ggboxplot(df,y = name1,color = "#00AFBB",add = "dotplot")+
  ggtitle("T�tulo del gr�fico") + xlab("T�tulo Eje X") + ylab("T�tulo Eje y")






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
  layout(title = "T�tulo del Gr�fico",yaxis = list(title = "T�tulo Eje Y"),xaxis = list(title = "T�tulo Eje X"))

