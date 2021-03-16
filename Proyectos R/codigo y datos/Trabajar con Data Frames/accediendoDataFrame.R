#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
#********************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("xlsx")

# Instala los paquetes sinó los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])


#********************************************************************
# 1.2 CARGAR PAQUETES O CREAR FUNCIONES
#********************************************************************
# Carga los paquetes sinó los tienes cargados
lapply(.packages, require, character.only=TRUE)


# LEER LOS DATOS EN FORMATO DATAFRAME de UN excel
misDatos <- read_excel(file.choose(), sheet = 1)

# VISUALIZAR LOS DATOS EN LA CONSOLE
head(misDatos)

# NÚMERO DE VARIABLES Y NÚMERO DE OBSERVACIONES
nrow(misDatos)
ncol(misDatos)

# ACCEDER A LOS DATOS
misDatos[c(1,2,3)] #acceder a las 3 primeras columnas
misDatos$Puntos[1]
misDatos$Equipos[1]
misDatos$Equipos[2]

# ACCEDER A LOS NOMBRES
Puntos <- misDatos[3][1] # crear una nueva dataframe con la variable puntos
tresPrimerasVariables <- misDatos[c(1,2,3)]
names(Puntos)
names(tresPrimerasVariables)


# ACCEDER A LOS VALORES
a <- Puntos[[1]]
tusDatos[[2]]

# ACCEDER A LA VARIABLE PUNTOS y PARTIDOS GANADOS
puntosPGanados <- misDatos[c(3,5)]



##############
#
#  Parte de Adan
#############
library(openxlsx)
library(dplyr)
library(psych)
library(markdown)


archivo <- openxlsx::read.xlsx(file.choose())
str(archivo)


colnames(archivo)
head(archivo,10)
anio2017 <- na.omit(archivo$"2017")
anio2018 <- na.omit(archivo$"2018")

hist(anio2017)
hist(anio2018)
polygon(anio2017)



typeof(anio2017)

dfanios <-data.frame(anio1 = rnorm(1000,5,1), anio2 = rnorm(1000,5,1))
describe(dfanios, na.rm = TRUE,IQR = TRUE)


#############
# creando un data frame con estadistica descriptiva usando apply
############

estadescriptiva <- t(do.call(data.frame,
list(Std       = apply(dfanios,2,sd),
     Mean      = apply(dfanios,2,mean),
     Median    = apply(dfanios,2,median),
     Max       = apply(dfanios,2,max),
     Min       = apply(dfanios,2,min),
     IQ1       = apply(dfanios,2,quantile,prob = 0.25),
     IQ3       = apply(dfanios,2,quantile,prob = 0.75),
     rango     = apply(dfanios,2,quantile,prob = 0.75) - apply(dfanios,2,quantile,prob = 0.25) ,
     Registros = apply(dfanios,2,length))
))
estadescriptiva


##################
#Exportar datos a excel, txt y csv
#################

write.xlsx(estadescriptiva,"Estadistica Descriptiva.xlsx",sheet.name = "Descriptiva", col.names = TRUE, row.names =TRUE, append = FALSE )

write.csv2(estadescriptiva, "Estadi_Descriptiva.csv")

write.table(estadescriptiva,"Estadi_Descriptiva.txt",sep=';')



###########################################
# RMARKDOWN y NOTEBOOK PARA GENERAR DOC WORD
##########################################






