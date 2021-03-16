# Cargar paquetes de funciones
library(readxl)

# Leer datos
datos <- read_excel("espalda-para-R.xlsx")

# Manipular la tabla de Datos
diff_ODI = datos[,9]-datos[,10] #calculo la variable diff_ODI
names(diff_ODI) = "Diff_ODI" #cambio el nombre de la variable a "diff_ODI"
datos = cbind(datos,diff_ODI) # añado una columna con la nueva variable diff_ODI

# Descripción
summary(datos)
colMeans(datos[,c(3,4,5,12)])

# Gráficos
hist(datos[,12])
hist(datos[,9],main = "Histograma del Mes 0")

# Análisis