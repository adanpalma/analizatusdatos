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
.packages = c("aod","ggplot2","plotly","InformationValue","MASS","generalhoslem","foreign")

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
head(misDatos)

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

# Un estudio analiza los factores que influyen en la decisión de aplicar
# a la escuela de postgrado. Se pregunta a los jóvenes universitarios
# si es poco probable "unlikely", algo probable "somewhatt likely" o muy probable "very likely" que se quieran apuntar
# a la escuela de postgrado. Por lo tanto, nuestra variable respuesta
# tiene tres categorías. Los datos sobre el estado educativo de los padres,
# si la institución de pregrado es pública o privada, y el "GPA" ("great point average") actual también se recogen.
# Los investigadores tienen razones para creer que las "distancias" entre estos tres puntos no son iguales.
# Por ejemplo, la "distancia" entre "improbable" y "algo probable" puede ser
# más corta que la distancia entre "algo probable" y "muy probable".

varEstudio <- misDatos

varEstudio[,1] <- factor(varEstudio[,1])
varEstudio[,2] <- factor(varEstudio[,2])
varEstudio[,3] <- factor(varEstudio[,3])



#*******************************************************************************
# PASO 2: UTILIZA UN MAPA PARA ORIENTARTE
#*******************************************************************************

# Queremos relacionar variables con un modelo lineal generalizado

# Objetivo:
#***********
# Un estudio analiza los factores que influyen en la decisión de aplicar
# a la escuela de postgrado. Se pregunta a los jóvenes universitarios
# si es poco probable "unlikely", algo probable "somewhatt likely" o muy probable "very likely" que se quieran apuntar
# a la escuela de postgrado. Por lo tanto, nuestra variable respuesta
# tiene tres categorías. Los datos sobre el estado educativo de los padres,
# si la institución de pregrado es pública o privada, y el "GPA" ("great point average") actual también se recogen.
# Los investigadores tienen razones para creer que las "distancias" entre estos tres puntos no son iguales.
# Por ejemplo, la "distancia" entre "improbable" y "algo probable" puede ser
# más corta que la distancia entre "algo probable" y "muy probable".

# Variables:
#************
# Variable Respuesta: apply (probabilidades de acceder al postgrado )-> ordinal
# Variable Entrada:
#   · pared (Variable 0/1 que indica si al menos uno de los padres tiene un título de posgrado)
#   · gpa (puntuación en la universidad de 0 a 4)
#   · public (o = Publica y 1 = Privada)


# Estructura del modelo
#***********************
#   ·Variable Respuesta: apply -> CATEGÓRICA ORDINAL
#   ·Variables Predictoras: pared, public, gpa
#   ·Modelo: logístico Ordinal
#   ·Familia: multinomial

# Consideraciones
# *****************
#   ·Que las variables predictoras no dependan entre sí (NO colinealidad)
#   ·Que haya una dependencia lineal entre el odd ratio de la variable de salida y las de entrada


# PASO 2.1: EXPLORACIÓN - MATRIXPLOT
#*******************************************************************************
# 2.1 Estadística descriptiva
# Tabla de contingencias
lapply(varEstudio[, c("apply", "pared", "public")], table)
ftable(xtabs(~ public + apply + pared, data = varEstudio))
# Resumen numérico de la variable numérica
summary(varEstudio$gpa)
sd(varEstudio$gpa)
# boxplot de la variable GPA por grupos
ggplot(varEstudio, aes(x = apply, y = gpa)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(pared ~ public, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


#*******************************************************************************
# ANÁLISIS: Modelo Logístico Ordinal
#*******************************************************************************


# MODELO 
#*******************************************************************************
# PASO 3.1. Cálculo Modelo Logístico
#*******************************************************************************
modelo1LogisticoOrdinal <- polr(apply ~ pared + public + gpa, data = varEstudio, Hess=TRUE)
# Resultados del modelo logístico
summary(modelo1LogisticoOrdinal) # todos los coeficientes son significativos!!!

# P-valores de los coeficientes
(ctable <- coef(summary(modelo1LogisticoOrdinal)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# Intervalo de confianza de los coeficientes
(ci <- confint(modelo1LogisticoOrdinal)) # 

#*******************************************************************************
# PASO 3.2. Calidad del Modelo Logístico
#*******************************************************************************
lipsitz.test(modelo1LogisticoOrdinal)

prediccion <- fitted(modelo1LogisticoOrdinal)

# PROBABILIDADES DEL MODELO - ANÁLISIS DE RESULTADOS
newdat <- data.frame(
  pared = as.factor(rep(0:1, 200)),
  public = as.factor(rep(0:1, each = 200)),
  gpa = rep(seq(from = 1.9, to = 4, length.out = 100), 4))

newdat <- cbind(newdat, predict(modelo1LogisticoOrdinal, newdat, type = "probs"))

##show first few rows
head(newdat)

lnewdat <- melt(newdat, id.vars = c("pared", "public", "gpa"),
                variable.name = "Level", value.name="Probability")
names(lnewdat)[4] <- "Level"
names(lnewdat)[5] <- "Probability"

## view first few rows
head(lnewdat)

ggplot(lnewdat, aes(x = gpa, y = Probability, colour = Level)) +
  geom_line() + facet_grid(pared ~ public, labeller="label_both")

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
#   . Linealidad entre log Odds ordinales vs variables dependientes

# 3. Exactitud del modelo (accuracy) --> Machine Learning
#   . Curva de ROC para regresiones logísticas de variables dicotómicas
#   . Misclassification Error (cuanto más pequeño mejor es el modelo)
#   . Concordancia (cuando más cercano a 1# a. CURVA ROC -> válido para modelos logísticos con variables dicotómicas)

# Le quitaremos la variable public como predictora
modelo2LogisticoOrdinal <- polr(apply ~ pared + gpa, data = varEstudio, Hess=TRUE)
summary(modelo2LogisticoOrdinal)
lipsitz.test(modelo2LogisticoOrdinal)
# P-valores de los coeficientes
(ctable <- coef(summary(modelo2LogisticoOrdinal)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# Intervalo de confianza de los coeficientes
(ci <- confint(modelo1LogisticoOrdinal)) # 
