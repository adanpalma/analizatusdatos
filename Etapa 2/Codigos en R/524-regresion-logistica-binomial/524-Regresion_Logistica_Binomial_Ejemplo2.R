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
.packages = c("aod","ggplot2","plotly","InformationValue","ResourceSelection")

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

# Voy a utilizar la base de datos cars que está en R
misDatos <- read.csv(file.choose())


# Nota: Asegurate que tienes los nombres de las variables puestas


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

# La PREGUNTA: Encontrar un modelo que te permite explicar la variable respuesta
# y en función de las variables predictoras.
# ¿Cuál es la problemática?

# Cuatro variables:
# - Admit: yes or not si es admitido o no a la universidad (cat. dicotómica)
# - Gre: Graduate Record Exam scores es numérica
# - GPA: Grade Point Average es numérica
# - Rank: prestigio del colegio (1 = bajo, 2 = medio, 3= alto, 4 = muy alto) Var. ordinal)

# Ver si el prestigio de la universidad y las notas del alumno influent en si es adminito o no
# en la universidad. Admit = 1 admitido y Admit = 0 NO admitido
varEstudio <- misDatos

varEstudio[,1] <- factor(varEstudio[,1])
varEstudio[,4] <- factor(varEstudio[,4])



#*******************************************************************************
# PASO 2: UTILIZA UN MAPA PARA ORIENTARTE
#*******************************************************************************

# Queremos relacionar variables con un modelo lineal generalizado

# Objetivo:
#***********
# Un investigador quiere ver si GRE (Graduate Record Exam scores),
# GPA (grade point average) y el prestigio del colegio dónde ha estudiado,
# efecta en la probabilidades de ser admitido.
# The response variable, admit/don't admit, is a binary variable.

# Estructura del modelo
#***********************
#   ·Variable Respuesta: admit -> CATEGÓRICA DICOTÓMICA
#   ·Variables Predictoras: gre,GPA,rank
#   ·Modelo: logístico
#   ·Familia: binomial

# Consideraciones
# *****************
#   ·Que las variables predictoras no dependan entre sí (NO colinealidad)
#   ·Que haya una dependencia lineal entre el odd ratio de la variable de salida y las de entrada


# PASO 2.1: EXPLORACIÓN - MATRIXPLOT
#*******************************************************************************
# 2.1 Estadística descriptiva
head(varEstudio)
summary(varEstudio)
sapply(varEstudio, sd)
sapply(varEstudio, mean)
# Tabla de contingencia
xtabs(~admit + rank, data = varEstudio)

# Matrixplot de las variable numéricas
pairs(varEstudio[c(2,3)],pch = 21, bg = c("red", "green3")[factor(varEstudio$admit)],oma=c(4,4,6,12))
par(xpd=TRUE)
legend(0.91, 0.7, as.vector(c("NO admitido" , "admitido")),fill=c("red", "green3"))


#*******************************************************************************
# ANÁLISIS: Modelo Logístico Binomial
#*******************************************************************************


# MODELO 
#*******************************************************************************
# PASO 3.1. Cálculo Modelo Logístico
#*******************************************************************************
mylogit <- glm(admit ~ gre + gpa + rank, data = varEstudio, family = "binomial")
# Resultados del modelo logístico
summary(mylogit) # todos los coeficientes son significativos!!!


#*******************************************************************************
# PASO 3.2. Calidad del Modelo Logístico
#*******************************************************************************
#  Hosmer and Lemeshow goodness of fit (GOF) test
hoslem.test(mylogit$y, fitted(mylogit), g=10)

prediccion <- plogis(predict(mylogit, varEstudio))
# a. Error de Clasificacion
misClassError(varEstudio$admit, prediccion, threshold = 0.5)
# b. # a. CURVA ROC -> válido para modelos logísticos con variables dicotómicas
plotROC(varEstudio$admit, prediccion)
# c. Conconrdancia
Concordance(varEstudio$admit, prediccion)


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
vif(logitMod) # < 4 no tenemos colinealidad

# 3. Exactitud del modelo (accuracy) --> Machine Learning
#   . Curva de ROC para regresiones logísticas de variables dicotómicas
#   . Misclassification Error (cuanto más pequeño mejor es el modelo)
#   . Concordancia (cuando más cercano a 1# a. CURVA ROC -> válido para modelos logísticos con variables dicotómicas)




# MODELO 2
#*******************************************************************************
# PASO 3.1. Cálculo Modelo Logístico
#*******************************************************************************
mylogit <- glm(admit ~ rank, data = varEstudio, family = "binomial")
# Resultados del modelo logístico
summary(mylogit) # todos los coeficientes son significativos!!!


#*******************************************************************************
# PASO 3.2. Calidad del Modelo Logístico
#*******************************************************************************
#  Hosmer and Lemeshow goodness of fit (GOF) test
hoslem.test(mylogit$y, fitted(mylogit), g=10)

prediccion <- plogis(predict(mylogit, varEstudio))
# a. Error de Clasificacion
misClassError(varEstudio$admit, prediccion, threshold = 0.5)
# b. # a. CURVA ROC -> válido para modelos logísticos con variables dicotómicas
plotROC(varEstudio$admit, prediccion)
# c. Conconrdancia
Concordance(varEstudio$admit, prediccion)


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
vif(logitMod) # < 4 no tenemos colinealidad

# 3. Exactitud del modelo (accuracy) --> Machine Learning
#   . Curva de ROC para regresiones logísticas de variables dicotómicas
#   . Misclassification Error (cuanto más pequeño mejor es el modelo)
#   . Concordancia (cuando más cercano a 1# a. CURVA ROC -> válido para modelos logísticos con variables dicotómicas)




#*******************************************************************************
# BONUS - ENTRENANDO UN MODELO
#*******************************************************************************
# TRAINING y TEST DATA
# Create Training Data
input_ones <- varEstudio[which(varEstudio$admit == 1), ]  # all 1's
input_zeros <- varEstudio[which(varEstudio$admit == 0), ]  # all 0's
set.seed(500)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 

# ENTRENAMOS EL MODELO LOGÍSTICO
logitMod <- glm(admit ~ gre + gpa + rank, data=trainingData, family=binomial(link="logit"))
summary(logitMod)

# CALCULAMOS LA CURVA ROC
predicted <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted <- predict(logitMod, testData, type="response")  # predicted scores

plotROC(testData$admit, predicted)