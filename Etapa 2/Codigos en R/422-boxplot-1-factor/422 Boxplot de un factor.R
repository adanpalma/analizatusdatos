# importar datos
datos = mtcars

# leemos un data frame con la primera columna una variable cuantitativa y la segunda variable el factor
df = datos[c(1,10)]
df[,2] = as.factor(df[,2])

# boxplot de un factor con base
boxplot(df[,1]~df[,2],data = df, main = "Titulo del grafico")


library(ggpubr)
# boxplot de un factor con ggpubr
ggboxplot(df, x = names(df)[2], y = names(df)[1],
          color = names(df)[2])+
  ggtitle("Titulo Grafico")

library(ggpubr)
# StripChart con el paquete ggpubr
ggstripchart(df, x = names(df)[2], y = names(df)[1],
             color = names(df)[2])

library(plotly)
# Boxplot con puntos con plotly
name1 <- names(df)[1]
name2 <- names(df)[2]
plot_ly(y = df[,name1], x = df[,name2], type = "box",name = name1, boxpoints = "all")%>%
  layout(title = "Titulo del grafico",yaxis = list(title = "Nombre Medida"),xaxis = list(title = "Nombre del factor"))