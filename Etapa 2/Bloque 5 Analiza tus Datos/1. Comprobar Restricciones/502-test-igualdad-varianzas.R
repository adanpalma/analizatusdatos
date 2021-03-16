# importar datos
datos = iris

# seleccionamos una variable cuantativa y un factor
df <- iris[c(1,5)]


# 1 DESCRIPCI?N
# diagrama de error, boxplot de un factor

library(plotly)
# Boxplot con puntos con plotly
name1 <- names(df)[1]
name2 <- names(df)[2]
plot_ly(y = df[,name1], x = df[,name2], type = "box",name = name1, boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1, "en relaci?n al grupo ",name2),yaxis = list(title = name1),xaxis = list(title = name2))

library(ggpubr)
# Dibujamos el diagrama de error con la media +/- la dev estandar
ggerrorplot(df, x = names(df)[2], y = names(df)[1], 
            desc_stat = "mean_sd")+
  ggtitle("Titulo del grafico")


# 2 Test de igualdad de Varianzas
# Test de Levene
leveneTest(Sepal.Length ~ Species, data = df)










# Para dos grupos

# seleccionamos una variable cuantativa y un factor
df <- iris[c(1,5)]

# Solamente seleccionamos los grupos de versicolor y virginica
rowSel <- df[2]=="versicolor"|df[2]=="virginica"

# df filtrado
df = df[rowSel,]


# 1 DESCRIPCI?N
# diagrama de error, boxplot de un factor

library(plotly)
# Boxplot con puntos con plotly
name1 <- names(df)[1]
name2 <- names(df)[2]
plot_ly(y = df[,name1], x = df[,name2], type = "box",name = name1, boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1, "en relaci?n al grupo ",name2),yaxis = list(title = name1),xaxis = list(title = name2))

library(ggpubr)
# Dibujamos el diagrama de error con la media +/- la dev estandar
ggerrorplot(df, x = names(df)[2], y = names(df)[1], 
            desc_stat = "mean_sd")+
  ggtitle("Titulo del grafico")


# 2 Test de igualdad de Varianzas
# Test de Levene
leveneTest(Sepal.Length ~ Species, data = df)