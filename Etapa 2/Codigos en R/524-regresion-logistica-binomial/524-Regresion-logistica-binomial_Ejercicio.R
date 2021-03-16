#
# ETAPA 3: PROFUNDIZA EL ANÁLISIS
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
.packages = c("readxl")

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
# misDatos <- read.delim(file.choose(),header = TRUE)

# Utiliza este comando para leer archivos con separador
# misDatos <- read.csv(file.choose(),header = TRUE,sep = ';')

# Voy a utilizar un archivo Excel de un ejemplo de la Fundació Sant Joan de Déu
misDatos <- read_excel(file.choose())



# PASO 0.2 - IDENTIFICA VARIABLES NUMÉRICAS, CATEGÓRICAS
#-------------------------------------------------------------------------------
# Puedes utilizar este comando para diferenciar tres tipos de variables:
# 1-Numeric: variables numéricas reales
# 2-Integer: son variables con números enteros.
#           Puede ser numérica discreta o categórica (tienes que decidir donde crees que será)
# 3-Factor: variables con caracteres. Siempre seran categóricas.
str(misDatos)
variables <- split(names(misDatos),sapply(misDatos, function(x) paste(class(x), collapse=" ")))

# La variable edad es numérica
varNumericas <- c(variables$numeric)

# Las variables categóricas son los factores.
varCategoricas <- variables$integer

#*******************************************************************************
# PASO 1: QUÉ ESTÁS BUSCANDO
#*******************************************************************************

# La PREGUNTA: Encontrar un modelo lineal que puedes calcular la probabilidad de ser
# un enfermo grave teniendo en cuenta:

# Modelo 1: teniendo en cuenta la presion sistolica
# Modelo 2: teniendo en cuenta la presion sistolica, el sexo, la edad


#*******************************************************************************
# PASO 2: UTILIZA UN MAPA PARA ORIENTARTE
#*******************************************************************************

# Como la variable y es una variable dicotómica 0 o 1 vamos a utilizar la 
# regresion logistica


#*******************************************************************************
# ANÁLISIS: Regresión lineal LOGISTICA
#*******************************************************************************


# MODELO 1 : en función de la Presion sistolica
#*******************************************************************************


# EJEMPLO 1: REGRESIÓN LOGÍSTICA SIMPLE
varEstudio <- misDatos[,c("TAS_m","IAH30")]
varEstudio$IAH30 <- factor(varEstudio$IAH30) # paso a factor para que sea categórico 
# Regresión Logística
regresionLogisticaSimple <- glm(IAH30 ~ TAS_m, data = varEstudio, family = "binomial")
summary(regresionLogisticaSimple)
# Probabilidad de ser un paciente grave con un valor de presion arterial de 170 y de 185
plogis(predict(regresionLogisticaSimple, newdata = data.frame(TAS_m=c(170,185)), type = "response"))





# EJEMPLO 2: REGRESIÓN LOGÍSTICA MÚLTIPLE
varEstudio <- misDatos[,c("TAS_m","edad","sexo","IAH30")]
varEstudio$IAH30 <- factor(varEstudio$IAH30) # paso a factor para que sea categórico
varEstudio$sexo <- factor(varEstudio$sexo) # paso a factor para que sea categórico 

# Regresión Logística
regresionLogisticaMultiple <- glm(IAH30 ~ TAS_m + edad + sexo, data = varEstudio, family = "binomial")
summary(regresionLogisticaMultiple)


# Probabilidad de ser un paciente grave si:
# Edad = 67
# Presion Sistolica = 150
# Sexo = 1
plogis(predict(regresionLogisticaMultiple, newdata = data.frame(TAS_m=c(150),edad = 67,sexo = factor(1)), type = "response"))
# Probabilidad de ser un paciente grave si:
# Edad = 67
# Presion Sistolica = 150
# Sexo = 0
plogis(predict(regresionLogisticaMultiple, newdata = data.frame(TAS_m=c(150),edad = 67,sexo = factor(0)), type = "response"))


#*******************************************************************************
# PASO 3: UTILIZA LA PLANTILLA
#*******************************************************************************

# En un word....
