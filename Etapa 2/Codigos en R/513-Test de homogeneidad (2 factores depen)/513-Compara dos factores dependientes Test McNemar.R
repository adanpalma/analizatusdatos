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
.packages = c("foreign","gplots")

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
#misDatos <- read.delim(file.choose(),header = TRUE,row.names = 1)

# Utiliza este comando para leer archivos con separador
misDatos <- read.csv(file.choose(),header = TRUE,sep = ';')
varEstudio <- misDatos

# Nota: Asegurate que tienes los nombres de las variables puestas


#*******************************************************************************
# PASO 1: QUÉ ESTÁS BUSCANDO
#*******************************************************************************

# La PREGUNTA: si estan relacionadas los dos variables categóricas.
# ¿Hay una relación entre antes y después?


#*******************************************************************************
# PASO 2: UTILIZA UN MAPA PARA ORIENTARTE
#*******************************************************************************

# Las dos categorías son dependientes. Porque la observación es del mismo sujeto
# y se estan poniendo dos situaciones.
# Las variables son dicotómicos: Test de McNemar


# Los pasos para este tipo de Análisis son:
# PASO 2.1 EXPLORAR LA TABLA DE CONTINGENCIAS
# PASO 2.2 ANALIZA LA IMPORTANCIA DE CADA CELDA

# PASO 2.1: EXPLORAR LA TABLA DE CONTINGENCIAS
#*******************************************************************************
# Un tipo de mapa de circulos. balloonplot
table(varEstudio)
# es una tabla 2x2 con variables relacionadas
balloonplot(table(varEstudio), main ="Relación entre celdas", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# PASO 2.2 DECIDIR EL TEST
#*******************************************************************************
# Test de McNemar
mcnemar.test(varEstudio[,1], varEstudio[,2], correct = FALSE)

#*******************************************************************************
# PASO 3: APLICA LA PLANTILLA
#*******************************************************************************

# En un word....
