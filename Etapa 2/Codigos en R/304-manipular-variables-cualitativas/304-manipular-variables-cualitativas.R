# leer el archivo Excel espalda.xlsx
library(readxl)
datos <- read_excel("espalda.xlsx")

# convertir una variable num a cualitativa
df <- datos[,11]

df$Tratamiento_Num <- factor(df$Tratamiento_Num,levels=c("0","1"),
                             labels=c("Convencional","Avanzado"))

# convertir una variable de datos en cualitativa
datos$Tratamiento_Num <- factor(datos$Tratamiento_Num,levels=c("0","1"),
                             labels=c("Convencional","Avanzado"))

# convertir la variable sexo (num por defecto) en cualitativa
datos$Sexo <- factor(datos$Sexo,levels=c("0","1"),
                     labels=c("Hombre","Mujer"))