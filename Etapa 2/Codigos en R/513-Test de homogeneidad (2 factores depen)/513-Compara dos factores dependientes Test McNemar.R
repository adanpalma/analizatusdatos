#
# ETAPA 2: ANALIZA LO M�S SIMPLE
#
#*******************************************************************************
#   PASO 0: CONSIGUE TU BASE DE DATOS
#   PASO 1: QU� EST�S BUSCANDO
#   PASO 2: UTILIZA EL MAPA PARA ORIENTARTE
#   PASO 3: APLICA LA PLANTILLA
#*******************************************************************************
#-------------------------------------------------------------------------------

# INSTALAR PAQUETES DE FUNCIONES
#*******************************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("foreign","gplots")

# Instala los paquetes sin� los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# CARGAR PAQUETES O CREAR FUNCIONES
#*******************************************************************************
# Carga los paquetes sin� los tienes cargados
lapply(.packages, require, character.only=TRUE)
#-------------------------------------------------------------------------------

#*******************************************************************************
# PASO 0: CONSIGUE TU BASE DATOS Y L�ELA CON INTELIGENCIA
#*******************************************************************************

# PASO 0.1 - LEE TUS DATOS (IMPORTAR)
#-------------------------------------------------------------------------------
# Utiliza este comando para leer archivos con TABULACI�N como separador
#misDatos <- read.delim(file.choose(),header = TRUE,row.names = 1)

# Utiliza este comando para leer archivos con separador
misDatos <- read.csv(file.choose(),header = TRUE,sep = ';')
varEstudio <- misDatos

# Nota: Asegurate que tienes los nombres de las variables puestas


#*******************************************************************************
# PASO 1: QU� EST�S BUSCANDO
#*******************************************************************************

# La PREGUNTA: si estan relacionadas los dos variables categ�ricas.
# �Hay una relaci�n entre antes y despu�s?


#*******************************************************************************
# PASO 2: UTILIZA UN MAPA PARA ORIENTARTE
#*******************************************************************************

# Las dos categor�as son dependientes. Porque la observaci�n es del mismo sujeto
# y se estan poniendo dos situaciones.
# Las variables son dicot�micos: Test de McNemar


# Los pasos para este tipo de An�lisis son:
# PASO 2.1 EXPLORAR LA TABLA DE CONTINGENCIAS
# PASO 2.2 ANALIZA LA IMPORTANCIA DE CADA CELDA

# PASO 2.1: EXPLORAR LA TABLA DE CONTINGENCIAS
#*******************************************************************************
# Un tipo de mapa de circulos. balloonplot
table(varEstudio)
# es una tabla 2x2 con variables relacionadas
balloonplot(table(varEstudio), main ="Relaci�n entre celdas", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# PASO 2.2 DECIDIR EL TEST
#*******************************************************************************
# Test de McNemar
mcnemar.test(varEstudio[,1], varEstudio[,2], correct = FALSE)

#*******************************************************************************
# PASO 3: APLICA LA PLANTILLA
#*******************************************************************************

# En un word....
