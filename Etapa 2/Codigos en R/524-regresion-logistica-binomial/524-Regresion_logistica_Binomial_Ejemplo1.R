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
.packages = c("readxl","InformationValue","ResourceSelection","lme4")

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

# Queremos relacionar variables con un modelo lineal generalizado

# Objetivo:
#***********
# Encontrar un modelo lineal que puedes calcular la probabilidad de ser
# un enfermo grave teniendo en cuenta:

# Modelo 1: teniendo en cuenta la presion sistolica
# Modelo 2: teniendo en cuenta la presion sistolica, el sexo, la edad



# Estructura del modelo
#***********************
#   ·Variable Respuesta: IAH30 -> CATEGÓRICA DICOTÓMICA
#   ·Variables Predictoras: TAS_m,sexo,edad
#   ·Modelo: logístico
#   ·Familia: binomial

# Consideraciones
# *****************
#   ·Que las variables predictoras no dependan entre sí (NO colinealidad)
#   ·Que haya una dependencia lineal entre el odd ratio de la variable de salida y las de entrada



#*******************************************************************************
# ANÁLISIS: Regresión lineal LOGISTICA
#*******************************************************************************

# MODELO 1 : en función de la Presion sistolica
#*******************************************************************************
# PASO 3.1. Cálculo Modelo Logístico
#*******************************************************************************

# EJEMPLO 1: REGRESIÓN LOGÍSTICA SIMPLE
varEstudio <- misDatos[,c("TAS_m","IAH30")]
varEstudio$IAH30 <- factor(varEstudio$IAH30) # paso a factor para que sea categórico 
# Regresión Logística
regresionLogisticaSimple <- glm(IAH30 ~ TAS_m, data = varEstudio, family = "binomial")
summary(regresionLogisticaSimple)

# Ejemplo pasar de odd ratio a probabilidad. Inversa de la función de enlace
# Probabilidad de ser un paciente grave con un valor de presion arterial de 170 y de 185
plogis(predict(regresionLogisticaSimple, newdata = data.frame(TAS_m=c(170,185)), type = "response"))

#*******************************************************************************
# PASO 3.2. Calidad del Modelo Logístico
#*******************************************************************************
m1 <- regresionLogisticaSimple
#  Hosmer and Lemeshow goodness of fit (GOF) test H1: el modelo NO ajusta con los datos
hoslem.test(m1$y, fitted(m1), g=10)
# El modelo ajusta bien con los datos
# plot(varEstudio$TAS_m,plogis(predict(m1, varEstudio)))

prediccion <- plogis(predict(m1, varEstudio))
# a. Error de Clasificacion
misClassError(varEstudio$TAS_m, prediccion, threshold = 0.5)
# b. # a. CURVA ROC -> válido para modelos logísticos con variables dicotómicas
plotROC(varEstudio$IAH30, prediccion)
# c. Conconrdancia
Concordance(varEstudio$IAH30, prediccion)



# MODELO 2 : en función de la Presion sistolica la edad y el sexo
#*******************************************************************************
# PASO 3.1. Cálculo Modelo Logístico
#*******************************************************************************

# EJEMPLO 2: REGRESIÓN LOGÍSTICA MÚLTIPLE
varEstudio <- misDatos[,c("TAS_m","edad","sexo","IAH30")]
varEstudio$IAH30 <- factor(varEstudio$IAH30) # paso a factor para que sea categórico
varEstudio$sexo <- factor(varEstudio$sexo) # paso a factor para que sea categórico 

# Regresión Logística
regresionLogisticaMultiple <- glm(IAH30 ~ TAS_m + edad + sexo, data = varEstudio, family = "binomial")
summary(regresionLogisticaMultiple)

# Ejemplos de cálculo de probabilidades con la inversa de la función de enlace:

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
# PASO 3.2. Calidad del Modelo Logístico
#*******************************************************************************
m2 <- regresionLogisticaMultiple
#  Hosmer and Lemeshow goodness of fit (GOF) test H1: el modelo NO ajusta con los datos
hoslem.test(m2$y, fitted(m2), g=10)
# El modelo ajusta bien con los datos
# plot(varEstudio$TAS_m,plogis(predict(m1, varEstudio)))

prediccion <- plogis(predict(m2, varEstudio))
# a. Error de Clasificacion
misClassError(varEstudio$TAS_m, prediccion, threshold = 0.5)
# b. # a. CURVA ROC -> válido para modelos logísticos con variables dicotómicas
plotROC(varEstudio$IAH30, prediccion)
# c. Conconrdancia
Concordance(varEstudio$IAH30, prediccion)


# MODELO 3 : en función de la Presion sistolica la edad
#*******************************************************************************
# PASO 3.1. Cálculo Modelo Logístico
#*******************************************************************************

# EJEMPLO 2: REGRESIÓN LOGÍSTICA MÚLTIPLE
varEstudio <- misDatos[,c("TAS_m","edad","sexo","IAH30")]
varEstudio$IAH30 <- factor(varEstudio$IAH30) # paso a factor para que sea categórico
varEstudio$sexo <- factor(varEstudio$sexo) # paso a factor para que sea categórico 

# Regresión Logística
regresionLogisticaMultiple2 <- glm(IAH30 ~ TAS_m + edad, data = varEstudio, family = "binomial")
summary(regresionLogisticaMultiple2)

# Ejemplos de cálculo de probabilidades con la inversa de la función de enlace:

# Probabilidad de ser un paciente grave si:
# Edad = 67
# Presion Sistolica = 150
# Sexo = 1
plogis(predict(regresionLogisticaMultiple2, newdata = data.frame(TAS_m=c(150),edad = 67,sexo = factor(1)), type = "response"))
# Probabilidad de ser un paciente grave si:
# Edad = 67
# Presion Sistolica = 150
# Sexo = 0
plogis(predict(regresionLogisticaMultiple2, newdata = data.frame(TAS_m=c(150),edad = 67,sexo = factor(0)), type = "response"))


#*******************************************************************************
# PASO 3.2. Calidad del Modelo Logístico
#*******************************************************************************
m3 <- regresionLogisticaMultiple2
#  Hosmer and Lemeshow goodness of fit (GOF) test H1: el modelo NO ajusta con los datos
hoslem.test(m3$y, fitted(m3), g=10)
# El modelo ajusta bien con los datos
# plot(varEstudio$TAS_m,plogis(predict(m1, varEstudio)))

prediccion <- plogis(predict(m3, varEstudio))
# a. Error de Clasificacion
misClassError(varEstudio$TAS_m, prediccion, threshold = 0.5)
# b. # a. CURVA ROC -> válido para modelos logísticos con variables dicotómicas
plotROC(varEstudio$IAH30, prediccion)
# c. Conconrdancia
Concordance(varEstudio$IAH30, prediccion)


#*******************************************************************************
# PASO 3.3 Consistencia del modelo - esquema
#*******************************************************************************
# 4. Analizamos la consistencia del modelo
# Es un modelo que es diferente a la regresión lineal.
# No hace falta que los residuos sean normales para que sea válido
#
# Aspectos a considerar:
#
# 1. Las variables predictivas
#   . ¿Las variables de entrada son significativas? (p-valores < 0.05)
#   . ¿Necesitamos más predictoras? (hay otras variables que pueden influir...)
#   . ¿Necesitamos interacciones cruzadas? variable1*variable2
#   . ¿Hay colinearidad entre variables? VIF < 4
# Advertencia: cuando más pequeño sea el modelo más fácil es de interpretar. Apuesta por modelos sencillos
#
# 2. Restricciones
#   . No hay colinearidad entre variables predictoras
#   . Linealidad entre log Odds vs variables dependientes

# 3. Exactitud del modelo (accuracy) --> Machine Learning
#   . Curva de ROC para regresiones logísticas de variables dicotómicas
#   . Misclassification Error (cuanto más pequeño mejor es el modelo)
#   . Concordancia (cuando más cercano a 1# a. CURVA ROC -> válido para modelos logísticos con variables dicotómicas)

#*******************************************************************************
# PASO 3.4 Comparación del modelo con BIC
#*******************************************************************************
#Para utilizar la comparativa puedes utilizar el ANOVA test
anova(m1, m2, test = "Chisq") # diferentes m1 y m2
anova(m1, m3, test = "Chisq") # diferentes m1 y m3
anova(m2, m3, test = "Chisq") # iguales m2 y m3 --> la variable sexo no está aportando nada al modelo

BIC(m1)
BIC(m2)
BIC(m3)
# El mejor modelo es el 1

#*******************************************************************************
# PASO 3: UTILIZA LA PLANTILLA
#*******************************************************************************

# En un word....
