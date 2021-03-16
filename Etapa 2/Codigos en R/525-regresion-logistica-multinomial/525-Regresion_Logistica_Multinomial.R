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
.packages = c("foreign","nnet","ggplot2","reshape2","InformationValue","generalhoslem")

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
misDatos <- read.dta(file.choose())


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

# OBJETIVO: 
# Los estudiantes que ingresan a la escuela secundaria hacen elecciones de programas
# entre el programa general, el programa vocacional y el programa académico.
# Su elección depende del resultado de un test de nivel y su estado económico social.

# El conjunto de datos contiene variables en 200 estudiantes.
#   · La variable respuesta es categórica, tipo de programa. --> "prog"
#   · Las variables predictoras son el estado socioeconómico --> "ses"
#   · Y nota del test, una variable continua --> "write"

varEstudio <- misDatos[c("prog","ses","write")]


#*******************************************************************************
# PASO 2: UTILIZA UN MAPA PARA ORIENTARTE
#*******************************************************************************

# Queremos relacionar variables con un modelo lineal generalizado

# Objetivo:
#***********
# Un investigador quiere ver si la elección

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
with(varEstudio, table(ses, prog))
with(varEstudio, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x), SD = sd(x)))))


#*******************************************************************************
# ANÁLISIS: Modelo Logístico Multinomial
#*******************************************************************************


# MODELO 
#*******************************************************************************
# PASO 3.1. Cálculo Modelo Logístico
#*******************************************************************************
varEstudio$prog2 <- relevel(varEstudio$prog, ref = "academic")
modelo1Multinomial <- multinom(prog2 ~ ses + write, data = varEstudio)

# Resultados
summary(modelo1Multinomial)

# P-valores de los coeficientes
z <- summary(modelo1Multinomial)$coefficients/summary(modelo1Multinomial)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p



#*******************************************************************************
# PASO 3.2. Calidad del Modelo Logístico
#*******************************************************************************
# Log Likelihood goodness of fit
logitgof(varEstudio$prog2, fitted(modelo1Multinomial))

prediccion <- fitted(modelo1Multinomial)



# Visualizando los modelos:

dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41), write = rep(c(30:70),3))
## store the predicted probabilities for each value of ses and write
pp.write <- cbind(dwrite, predict(modelo1Multinomial, newdata = dwrite, type = "probs", se = TRUE))
## calculate the mean probabilities within each level of ses
by(pp.write[, 3:5], pp.write$ses, colMeans)

lpp <- melt(pp.write, id.vars = c("ses", "write"), value.name = "probability")
names(lpp)[3] <- "Level"
names(lpp)[4] <- "Probability"

head(lpp)  # view first few rows
ggplot(lpp, aes(x = write, y = Probability, colour = Level)) +
  geom_line() + facet_grid(.~ses, labeller="label_both")


# Boxplot de las probabilidades predecidas
plot(varEstudio$prog,prediccion[,1])
plot(varEstudio$prog,prediccion[,2])
plot(varEstudio$prog,prediccion[,3])

# a. Error de Clasificacion
misClassError(varEstudio$prog2==unique(varEstudio$prog2)[1], prediccion[,1], threshold = c(0.33))
misClassError(varEstudio$prog2==unique(varEstudio$prog2)[2], prediccion[,2], threshold = c(0.33))
misClassError(varEstudio$prog2==unique(varEstudio$prog2)[3], prediccion[,3], threshold = c(0.33))

# b. # a. CURVA ROC -> por categorías
actuals <- varEstudio$prog2==unique(varEstudio$prog2)[1]
plotROC(actuals, prediccion[,1])

actuals <- varEstudio$prog2==unique(varEstudio$prog2)[2]
plotROC(actuals, prediccion[,2])

actuals <- varEstudio$prog2==unique(varEstudio$prog2)[3]
plotROC(actuals, prediccion[,3])

# c. Conconrdancia -> por categorías
actuals <- varEstudio$prog2==unique(varEstudio$prog2)[1]
Concordance(actuals, prediccion[,1])

actuals <- varEstudio$prog2==unique(varEstudio$prog2)[2]
Concordance(actuals, prediccion[,2])

actuals <- varEstudio$prog2==unique(varEstudio$prog2)[3]
Concordance(actuals, prediccion[,3])


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
varEstudio$prog2 <- relevel(varEstudio$prog, ref = "academic")
modelo2Multinomial <- multinom(prog2 ~ write, data = varEstudio)

# Resultados
summary(modelo2Multinomial)

# P-valores de los coeficientes
z <- summary(modelo2Multinomial)$coefficients/summary(modelo2Multinomial)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#*******************************************************************************
# PASO 3.2. Calidad del Modelo Logístico
#*******************************************************************************
prediccion <- fitted(modelo2Multinomial)
# Boxplot de las probabilidades predecidas
plot(varEstudio$prog,prediccion[,1])

# a. Error de Clasificacion
misClassError(varEstudio$prog2==unique(varEstudio$prog2)[1], prediccion[,1], threshold = c(0.5))
misClassError(varEstudio$prog2==unique(varEstudio$prog2)[2], prediccion[,2], threshold = c(0.5))
misClassError(varEstudio$prog2==unique(varEstudio$prog2)[3], prediccion[,3], threshold = c(0.5))

# b. # a. CURVA ROC -> por categorías
actuals <- varEstudio$prog2==unique(varEstudio$prog2)[1]
plotROC(actuals, prediccion[,1])

actuals <- varEstudio$prog2==unique(varEstudio$prog2)[2]
plotROC(actuals, prediccion[,2])

actuals <- varEstudio$prog2==unique(varEstudio$prog2)[3]
plotROC(actuals, prediccion[,3])

# c. Conconrdancia -> por categorías
actuals <- varEstudio$prog2==unique(varEstudio$prog2)[1]
Concordance(actuals, prediccion[,1])

actuals <- varEstudio$prog2==unique(varEstudio$prog2)[2]
Concordance(actuals, prediccion[,2])

actuals <- varEstudio$prog2==unique(varEstudio$prog2)[3]
Concordance(actuals, prediccion[,3])