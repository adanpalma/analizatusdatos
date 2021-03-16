# leer la tabla de datos
library(mlbench)
library(caret)
library(corrplot) # attach the BostonHousing dataset data(BostonHousing)
library(ggplot2)
library(ggpubr)
library(plotly)

data(BostonHousing)

# descripción masiva
pairs(BostonHousing)

# seleccionamos las dos variables del modelo
df <- BostonHousing[,c(6,13)]
df[,2] <- log(df[,2])

# diagrama de dispersión con la linea de tendencia
name1 <- names(df)[1]
name2 <- names(df)[2]
ggscatter(df, x = name1, y = name2, 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = name1 ,ylab = name2)



# regresión lineal simple
modelo <- lm(formula = lstat ~ rm, df)
summary(modelo)


# revision residuos
#*******************************************************************************
# · Scatter 2D para ver que no tenemos autocorrelación
# · Mirar si se distribuyen normalmente
# Scatter 2D
plot(modelo$residuals)
residuos <- as.numeric(modelo$residuals)
df <- data.frame(Residuos = residuos)
name1 <- names(df)

# Scatter 2D de los residuos
p <- plot_ly(x = seq(1,nrow(df)),y = df[,1],mode = "markers", type = "scatter")%>%
  layout(title = "Residuos Regresión Lineal",yaxis = list(title = "Residuos"),yaxis = list(title = "Índice")) 

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
subplot(p, p1, p2, p3)%>%
  layout(title = paste("Comprobacion residuos"))

# Prueba Distribución Normal
shapiro.test(residuos)

