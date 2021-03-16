library(haven)
# importar datos
datos <- read_sav(file.choose())

# Seleccionamos las dos variables ordinales
df <- datos[c(13,8)]



# 1. Descriptiva

# Diagramas de error
library(ggplot2)

# Diagrama de barras de dos variables categóricas apiladas

library(ggpubr)
# El diagrama de error con el IC de la media de dos factores
ggline(df, x = names(df)[2], y = names(df)[1], 
       add = c("mean_ci", "jitter"),
       palette = "jco")+
  ggtitle("Titulo del grafico")


# 2. COrrelación de dos variables Ordinales 

# Coeficiente de Kendall
m <- cor(as.matrix(df), method="kendall", use="pairwise")
cor.test(unlist(df[,1]),unlist(df[,2]), method="kendall")


