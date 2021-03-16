# importar datos
datos = iris

# seleccionamos la variable que vamos a analizar
df <- iris[1]
dvector <- df[,1]

# 1 DESCRIPCIÓN
# boxplot, histograma y qqplot
hist(dvector,breaks = 20,freq = FALSE, main = "Título del Gráfico",xlab = "Título Eje X",ylab = "Título Eje Y")

boxplot(dvector)

library(car)
# qqplot paquete car
qqPlot(dvector)


library(plotly)
library(ggplot2)
library(ggpubr)
# la entrada es un df con la variable cuantitativa
name1 <- names(df)
# Histograma de frecuencias con plotly
p1<-plot_ly(x = df[,name1], type = "histogram",name = name1,marker=list(color='#FF6666'))%>%
  layout(title = paste("Histograma de Frecuencias de",name1),yaxis = list(title = name1),bargap=0.005*(max(df[,name1])-min(df[,name1])))

# Boxplot
p2<-plot_ly(y = df[,name1], type = "box",name = name1,boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1),yaxis = list(title = name1))

# QQplot
p3<-ggqqplot(df, x = names(df),color = "#FF6666",add.params = list(color = "black"))+
  xlab("Distribución Teórica Normal") + ylab("Cuartiles reales") +
  theme_minimal() +
  ggtitle(paste("QQ-plot de ", name1,sep = "")) +
  theme(plot.title = element_text(hjust = 0.5))
p3 <- ggplotly(p3)
p <- subplot(p1, p2, p3)%>%
  layout(title = paste("Histograma de Frecuencias Boxplot y QQplot para la variable  ", name1,sep = ""))
p



# 2 Test de NORMALIDAD
shapiro.test(dvector) # fíjate que esta función trabaja con los valores NO los dataframes
