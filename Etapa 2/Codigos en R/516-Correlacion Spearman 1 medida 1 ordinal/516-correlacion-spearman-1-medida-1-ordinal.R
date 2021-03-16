library(haven)
# importar datos
datos <- read_sav(file.choose())

# Seleccionamos las dos variables ordinales
df <- datos[c(9,8)]

#df[,2]<-as.factor(unlist(df[,2]))

# 1. Descriptiva


# Diagrama de barras de dos variables categóricas apiladas

library(ggpubr)
# El diagrama de error con el IC de la media de dos factores
ggline(df, x = names(df)[2], y = names(df)[1], 
       add = c("mean_ci", "jitter"),
       palette = "jco")+
  ggtitle("Titulo del grafico")


# 2. COrrelación de dos variables Ordinales 
library(Hmisc)
# Coeficiente de Kendall
m <- rcorr(as.matrix(df), type="spearman")


