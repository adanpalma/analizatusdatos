# leemos el archivo excel espalda.xlsx
library(readxl)
datos <- read_excel("espalda.xlsx")

# Convertir la variables ODImes0 a ordinal
  # 0-20 --> Minima
  # 20-40 --> Moderada
  # 40-60 --> Intensa
  # 60-80 --> Discapacidad
  # +80 --> Maxima

varNum <- as.numeric(unlist(datos[,9]))
ODImes0_grupos <- cut(varNum,breaks = c(0,20,40,60,80,100),labels = c("Minima","Moderada","Intensa","Discapacidad","Maxima"))

datos <- cbind(datos,ODImes0_grupos)



# Convertir la variables ODImes1 a ordinal
  # 0-20 --> Minima
  # 20-40 --> Moderada
  # 40-60 --> Intensa
  # 60-80 --> Discapacidad
  # +80 --> Maxima

varNum <- as.numeric(unlist(datos$`ODI Mes1`))
ODImes1_grupos <- cut(varNum,breaks = c(0,20,40,60,80,100),labels = c("Minima","Moderada","Intensa","Discapacidad","Maxima"))

datos <- cbind(datos,ODImes1_grupos)