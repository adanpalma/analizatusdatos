#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
#********************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("ggplot2", "plotly", "xlsx","scales","stringr","readr","dplyr","psych","readxl","ggpubr")

##
# Instala los paquetes sinÃ³ los tienes instalados
##
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

#********************************************************************
# 1.2 CARGAR PAQUETES O CREAR FUNCIONES
#********************************************************************
# Carga los paquetes sinÃ³ los tienes cargados
lapply(.packages, require, character.only=TRUE)

###
# Seteo el Directorio....
##
setwd("C:/Users/apalmad/Desktop/Analiza tus Datos/Bloque 2/1.0 Lee tu Tabla de Datos")


##
#Texto Separado por comas sin columns names
##

#Creo un vector concatenando col... con un arreglo de numeros y quedaria , col1,col2,col3 ...
cnames <- paste(c(rep("col",16)),c(1:16))


df_ejemplodatos <-  read.csv("ejemplo-datos.txt",header = FALSE,col.names = c(cnames))
View(df_ejemplodatos)

##
#Text con la primera fila de nombre de columnas
##
df_ejemplodatosheader <- read.csv("ejemplo-datos-header.txt",header = TRUE, sep = ",")
View(df_ejemplodatosheader)

##
#text con skip
##
ejemplo-datos-header-skip.txt
df_ejemploheadrskip <- read.csv("ejemplo-datos-header-skip.txt",skip = 8,header = TRUE ,sep = ',')
View(df_ejemploheadrskip)

###
# ejemplo-datos-separador-punto-coma.txt
##
df_ejemplodatos_ptoycoma <- read.csv("ejemplo-datos-separador-punto-coma.txt", header = FALSE, sep = ';')
View(df_ejemplodatos_ptoycoma)


###
# EuStockMarkets CSV HEADER Y SEP CON COMA
##
df_ejemplodatos_ptoycoma <- read.csv("EuStockMarkets.CSV", header = TRUE, sep = ',')
View(df_ejemplodatos_ptoycoma)

###
# Quitando la primera columna
##
df_ejemplodatos_ptoycoma <- df_ejemplodatos_ptoycoma[-1]
View(df_ejemplodatos_ptoycoma)


###
# mtcars.csv
##
df_mtcarscsv <- read.csv("mtcars.csv", header = TRUE, sep = ',')
View(df_mtcarscsv)

###
###Colocando nombre a las filas
###
row_names <-  unlist(df_mtcarscsv[1])

df_mtcarscsv <- df_mtcarscsv[-1]
rownames(df_mtcarscsv) <- row_names
View(df_mtcarscsv)

####
# Leyendo archivos excel y visualizacion
####
abalone  <- read_excel("abalone.xlsx", sheet = "abalone")
View(abalone)


###
# Leyendo  excel y convirtiendo una columna sexo (0,1) a factor 0 Male, 1 Female
###

espalda <-  read_excel("espalda.xlsx")
dfs <- espalda[,2]
dfs$Sexo <-  factor(dfs$Sexo,levels = c("0","1"),labels = c("Male","Female"))
espalda$Sexo <-  dfs$Sexo

###
# convirtiendo una columna numerica Tratamiento (0,1) a factor 0 -> Convencional, 1 -> Avanzado
###
espalda$Tratamiento_Num <- factor(espalda$Tratamiento_Num,levels = c("0","1"),labels = c("Convencional","Avanzado"))
View(espalda)


###
# convirtiendo una columna numerica OdiMes0 en grupos de rangos (categorica) (0-20% > Minima, 20% -40% -> MOderada...etc)
###

espalda$OdiMes0Grupo <- cut(espalda$`ODI Mes0`,breaks = c(0,20,40,60,80,Inf),labels = c("Minimo","Moderado","Intensa","Discapacidad","Maxima"))
espalda

###
# convirtiendo una columna numerica OdiMes1 en grupos de rangos (categorica) (0-20% > Minima, 20% -40% -> MOderada...etc)
###
espalda$OdiMes1Grupo <- cut(espalda$`ODI Mes1`,breaks = c(0,20,40,60,80,Inf), labels=c("Minimo","Moderado","Intensa","Discapacidad","Maxima"))


####
# Creando variables a partir de otras, operaciones matematicas  como log, Scalandolas (Estandarizando, Normaliando,etc)
####

###
# Leyendo el archivo
###
espalda <-  read_excel("espalda.xlsx")
View(espalda)

#factorizo la variable Sexo 0-> Hombre, 1 -> Mujer
espalda$Sexo <- factor(espalda$Sexo,levels = c("0","1"), labels = c("Hombre","Mujer"))
View(espalda)

#Creo una variable usando el logaritmo de peso
dflogpeso <-  log(espalda$Peso)
espalda$logpeso <- dflogpeso

View(espalda)

#Creo una vriable que sale de multiplicar dos variables logpeso * altura
dflogpesoxaltura <- espalda$logpeso * espalda$Altura

##
##Normalizando y Estanadarizando variables
##

# Escalar Variables --- Estandarizo el Peso Media = 0 , Std= 1...(quita las unidades)
Peso_estand <-  scale(espalda$Peso,center = TRUE, scale = TRUE) #center = true (media 0), scale = true (std 1)
mean(Peso_estand) #Media cero 0
sd(Peso_estand)    # desv standar 1

hist(espalda$Peso) # Sin 
hist(Peso_estand)

#Escalar Variables MEDIA CERO Y STDEV NO ES UNO
Peso_estand <-  scale(espalda$Peso,center = TRUE, scale = FALSE) #center = true (media 0), scale = true (std 1)
mean(Peso_estand) #Media cero 0
sd(Peso_estand)    # desv standar NO ES 1
hist(espalda$Peso) # Sin 
hist(Peso_estand)

#Escalar Variables MEDIA NO ESCERO Y STDEV ES UNO
Peso_estand <-  scale(espalda$Peso,center = FALSE, scale = TRUE) #center = false (media <> 0), scale = true (std 1)
mean(Peso_estand) #Media NO ES cero 0
sd(Peso_estand)    # desv standar 1
hist(espalda$Peso) # Sin 
hist(Peso_estand)
