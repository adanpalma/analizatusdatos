# librerias necesarias
library(ggplot2)
library(ggpubr)
library(plotly)
library(car)

# leer y reconfigurar la tabla de datos
data(mtcars)
df <- mtcars[,c("wt","disp","hp","mpg")]
df[,4] <- 1/df[,4] # estoy calculando el consumo
names(df) <- c("peso","cilindrada","Caballos","consumo")
    # La variable x1 es el peso
    # La variable x2 es la cilindrada
    # La variable x3 son los caballos
    # La variable y es el consumo

# Descripción
pairs(df)


modelo <- lm(formula = consumo ~ peso + Caballos + cilindrada, df)
summary(modelo)


# Revisión de los residuos
#*******************************************************************************
# · Scatter 2D para ver que no tenemos autocorrelación
# · Mirar si se distribuyen normalmente
# · Mirar la colinealidad con el coeficiente VIF
# Scatter 2D
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


# Colinealidad del Modelo
vif(modelo) # parece que hay colinealidad --> quitaremos la cilindrada


# MODELO 2
#*******************************************************************************

# leer y reconfigurar la tabla de datos
data(mtcars)
df <- mtcars[,c("wt","disp","hp","mpg")]
df[,4] <- 1/df[,4] # estoy calculando el consumo
names(df) <- c("peso","cilindrada","Caballos","consumo")
# La variable x1 es el peso
# La variable x2 es la cilindrada
# La variable x3 son los caballos
# La variable y es el consumo

# Quitamos la variable cilindrado porque sabemos que está muy relacionada con el consumo
# y queremos ver otras variables que influyen.
modelo2 <- lm(formula = consumo ~ peso + Caballos, df)
summary(modelo2)

# Revision de los residuos
#*******************************************************************************
# · Scatter 2D para ver que no tenemos autocorrelación
# · Mirar si se distribuyen normalmente
# · Mirar la colinealidad con el coeficiente VIF

# Scatter 2D
residuos <- as.numeric(modelo2$residuals)
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

# Colinealidad con el VIF
vif(modelo2) # <4 vamos bien!!!

BIC(modelo)
BIC(modelo2)