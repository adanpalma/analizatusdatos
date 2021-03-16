#
# ETAPA 2: ANALIZA LO MÁS SIMPLE
#
#*******************************************************************************
#   PASO 0: CONSIGUE TU BASE DE DATOS
#   PASO 1: QUÉ ESTÁS BUSCANDO
#   PASO 2: UTILIZA EL MAPA PARA ORIENTARTE
#   PASO 3: APLICA LA PLANTILLA
#*******************************************************************************
#-------------------------------------------------------------------------------

# INSTALAR PAQUETES DE FUNCIONES
#*******************************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("gplots","corrplot")

# Instala los paquetes sinó los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# CARGAR PAQUETES O CREAR FUNCIONES
#*******************************************************************************
# Carga los paquetes sinó los tienes cargados
lapply(.packages, require, character.only=TRUE)
#-------------------------------------------------------------------------------

#*******************************************************************************
# PASO 0: CONSIGUE TU BASE DATOS Y LÉELA CON INTELIGENCIA
#*******************************************************************************

# PASO 0.1 - LEE TUS DATOS (IMPORTAR)
#-------------------------------------------------------------------------------
# Utiliza este comando para leer archivos con TABULACIÓN como separador
misDatos <- read.delim(file.choose(),header = TRUE,row.names = 1)
# Convierto los datos en una tabla
miTabla <- as.table(as.matrix(misDatos))

# Utiliza este comando para leer archivos con separador
# misDatos <- read.csv(file.choose(),header = TRUE,sep = ';')


# Nota: Asegurate que tienes los nombres de las variables puestas


#*******************************************************************************
# PASO 1: QUÉ ESTÁS BUSCANDO
#*******************************************************************************

# La PREGUNTA: inferir si hay dependencia entre filas (tipo de trabajo de casa) y
# columnas (quén hace estas tareas)


#*******************************************************************************
# PASO 2: UTILIZA UN MAPA PARA ORIENTARTE
#*******************************************************************************

# En este caso comparamos dos categorías: podemos utilizar Chi cuadrado, Chi cuadrado
# correción de Yates y el F exacto de Fisher

# En la práctica:
# . Si el 80% de las celdas de la tabla tienen más de 5 >> Chi cuadrado
# .	Si no cumple con la primera restricción >> Chi Cuadrado con correción de Yates

# Los pasos para este tipo de Análisis son:
# PASO 2.1 EXPLORAR LAS CELDAS
# PASO 2.2 DECIDIR EL TEST
# PASO 2.3 ANALIZA LA IMPORTANCIA DE CADA CELDA

# PASO 2.1: EXPLORAR LAS CELDAS
#*******************************************************************************
# Un tipo de mapa de circulos. balloonplot
balloonplot(t(miTabla), main ="Tareas de Casa", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)


# PASO 2.2 DECIDIR EL TEST
#*******************************************************************************
# El 76% de las celdas tienen valores mayores que 5. Podríamos aplicar Chi cuadrado
# justito. Aplicamos también la correción de Yates y vemos lal diferencia.
# 
# Test de Chi Cuadrado
chisq <- chisq.test(misDatos,correct = FALSE)
chisq
# Test de Chi Cuadrado con la correción de Yates
chisqYates <- chisq.test(misDatos,correct = TRUE)
chisqYates

# Si tubieramos dos 2x2 puedes utilizar el test Exacto de Fisher
# fisher.test(latabladeContingencia)

# PASO 2.3 ANALIZA LA IMPORTANCIA DE CADA CELDA
#*******************************************************************************
coefPearsonResiduos <- chisqYates$residuals
corrplot(chisqYates$residuals, is.cor = FALSE)
# Contribucion en tanto por ciento (%)
contrib <- 100*chisqYates$residuals^2/chisqYates$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

#*******************************************************************************
# PASO 3: APLICA LA PLANTILLA
#*******************************************************************************

# En un word....
