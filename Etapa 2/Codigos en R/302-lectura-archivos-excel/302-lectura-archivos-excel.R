# leer un archivo excel abalone.xlsx lectura con path fijo
datos1 <- read_excel("C:/Users/Anna i Jordi/Desktop/carpeta trabajo/datos/abalone.xlsx",sheet = "abalone")

# leer un archivo excel con la selecci�n manual del archivo
datos2 <- read_excel(file.choose(),sheet = "abalone")

# leer un archivo excel y visualizar los datos le�dos
datos3 <- read_excel("espalda.xlsx",sheet = "Datos")
View(datos3)
