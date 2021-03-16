#cargar Librerias
library(readxl)

#setear directorios
setwd("C:/Users/apalmad/Desktop/Analiza tus Datos/Bloque 2/Empieza con tu Software")



# Leer Datos

datos <- read_excel("espalda-para-R.xlsx")


#  Manipular tabla de datos
diff_oddi <- datos[,9] - datos[,10]
names(diff_oddi) <- "DiffOdi"
datos <- cbind(datos,diff_oddi)




# DESCRIPCION
summary(datos)

colMeans(datos[,c(3,4,5,12)])

# GRAFICOS 
hist(datos[,12],main="Hist Diff Odi")
boxplot(datos[,12],main = "Box Plot Diff Odi")


# ANALISIS

