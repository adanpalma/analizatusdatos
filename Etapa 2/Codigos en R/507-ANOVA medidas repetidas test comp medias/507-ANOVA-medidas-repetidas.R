# importar datos
datos <- read.table("Depresion.csv",header = TRUE,sep = ";")

# seleccionamos una variable 2 medidas pareadas
dfTOT <- datos

# creamos un data frame con las dos variables en la misma columna y un factor con el nombre de las variables
df <- data.frame(
  Depresion = c(dfTOT[,1],dfTOT[,2]),
  Mes = c(rep("0",length(dfTOT[,1])),rep("6",length(dfTOT[,2]))),
  ID = c(seq(1,length(dfTOT[,1])),seq(1,length(dfTOT[,1])))
  )


# 1- Descripción

# Boxplot de 1 factor y diagrama de medias

library(ggpubr)
# El diagrama de error con el IC de la media de dos factores
ggline(df, x = names(df)[2], y = names(df)[1], 
       add = c("mean_ci", "jitter"),
       palette = "jco")+
  ggtitle("Titulo del grafico")


library(plotly)
# Boxplot con puntos con plotly
name1 <- names(df)[1]
name2 <- names(df)[2]
plot_ly(y = df[,name1], x = df[,name2], type = "box",name = name1, boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1, "en relación al grupo ",name2),yaxis = list(title = name1),xaxis = list(title = name2))





# 2 Comparar los tres grupos

library(lme4)
library(lmerTest)


# --> TEST PARAMÉTRICO - ANOVA DE MEDIDAS REPETIDAS (si son normales todas las variables)
rmaModel <- lmer(Depresion ~ Mes + (1|ID), data = df)
anova(rmaModel)

shapiro.test(residuals(rmaModel)) 

# --> TEST NO PARAMÉTRICO - FRIEDMAN TEST
friedman.test(df[,1],group=df[,2],blocks = df[,3])

