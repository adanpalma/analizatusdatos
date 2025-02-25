#
# ETAPA 3: PROFUNDIZA EL AN�LISIS
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
.packages = c("aod","ggplot2","plotly","InformationValue","MASS","generalhoslem","foreign")

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
# misDatos <- read.delim(file.choose(),header = TRUE)

# Utiliza este comando para leer archivos con separador
# misDatos <- read.csv(file.choose(),header = TRUE,sep = ';')

# Voy a utilizar la base de datos cars que est� en R
misDatos <- read.dta(file.choose())
head(misDatos)

# Nota: Asegurate que tienes los nombres de las variables puestas


# PASO 0.2 - IDENTIFICA VARIABLES NUM�RICAS, CATEG�RICAS
#-------------------------------------------------------------------------------
# Puedes utilizar este comando para diferenciar tres tipos de variables:
# 1-Numeric: variables num�ricas reales
# 2-Integer: son variables con n�meros enteros.
#           Puede ser num�rica discreta o categ�rica (tienes que decidir donde crees que ser�)
# 3-Factor: variables con caracteres. Siempre seran categ�ricas.
str(misDatos)
variables <- split(names(misDatos),sapply(misDatos, function(x) paste(class(x), collapse=" ")))

# La variable edad es num�rica
varNumericas <- c(variables$numeric)

# Las variables categ�ricas son los factores.
varCategoricas <- variables$integer


#*******************************************************************************
# PASO 1: QU� EST�S BUSCANDO
#*******************************************************************************

# La PREGUNTA: Encontrar un modelo que te permite explicar la variable respuesta
# y en funci�n de las variables predictoras.
# �Cu�l es la problem�tica?

# Un estudio analiza los factores que influyen en la decisi�n de aplicar
# a la escuela de postgrado. Se pregunta a los j�venes universitarios
# si es poco probable "unlikely", algo probable "somewhatt likely" o muy probable "very likely" que se quieran apuntar
# a la escuela de postgrado. Por lo tanto, nuestra variable respuesta
# tiene tres categor�as. Los datos sobre el estado educativo de los padres,
# si la instituci�n de pregrado es p�blica o privada, y el "GPA" ("great point average") actual tambi�n se recogen.
# Los investigadores tienen razones para creer que las "distancias" entre estos tres puntos no son iguales.
# Por ejemplo, la "distancia" entre "improbable" y "algo probable" puede ser
# m�s corta que la distancia entre "algo probable" y "muy probable".

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
# Un estudio analiza los factores que influyen en la decisi�n de aplicar
# a la escuela de postgrado. Se pregunta a los j�venes universitarios
# si es poco probable "unlikely", algo probable "somewhatt likely" o muy probable "very likely" que se quieran apuntar
# a la escuela de postgrado. Por lo tanto, nuestra variable respuesta
# tiene tres categor�as. Los datos sobre el estado educativo de los padres,
# si la instituci�n de pregrado es p�blica o privada, y el "GPA" ("great point average") actual tambi�n se recogen.
# Los investigadores tienen razones para creer que las "distancias" entre estos tres puntos no son iguales.
# Por ejemplo, la "distancia" entre "improbable" y "algo probable" puede ser
# m�s corta que la distancia entre "algo probable" y "muy probable".

# Variables:
#************
# Variable Respuesta: apply (probabilidades de acceder al postgrado )-> ordinal
# Variable Entrada:
#   � pared (Variable 0/1 que indica si al menos uno de los padres tiene un t�tulo de posgrado)
#   � gpa (puntuaci�n en la universidad de 0 a 4)
#   � public (o = Publica y 1 = Privada)


# Estructura del modelo
#***********************
#   �Variable Respuesta: apply -> CATEG�RICA ORDINAL
#   �Variables Predictoras: pared, public, gpa
#   �Modelo: log�stico Ordinal
#   �Familia: multinomial

# Consideraciones
# *****************
#   �Que las variables predictoras no dependan entre s� (NO colinealidad)
#   �Que haya una dependencia lineal entre el odd ratio de la variable de salida y las de entrada


# PASO 2.1: EXPLORACI�N - MATRIXPLOT
#*******************************************************************************
# 2.1 Estad�stica descriptiva
# Tabla de contingencias
lapply(varEstudio[, c("apply", "pared", "public")], table)
ftable(xtabs(~ public + apply + pared, data = varEstudio))
# Resumen num�rico de la variable num�rica
summary(varEstudio$gpa)
sd(varEstudio$gpa)
# boxplot de la variable GPA por grupos
ggplot(varEstudio, aes(x = apply, y = gpa)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(pared ~ public, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


#*******************************************************************************
# AN�LISIS: Modelo Log�stico Ordinal
#*******************************************************************************


# MODELO 
#*******************************************************************************
# PASO 3.1. C�lculo Modelo Log�stico
#*******************************************************************************
modelo1LogisticoOrdinal <- polr(apply ~ pared + public + gpa, data = varEstudio, Hess=TRUE)
# Resultados del modelo log�stico
summary(modelo1LogisticoOrdinal) # todos los coeficientes son significativos!!!

# P-valores de los coeficientes
(ctable <- coef(summary(modelo1LogisticoOrdinal)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# Intervalo de confianza de los coeficientes
(ci <- confint(modelo1LogisticoOrdinal)) # 

#*******************************************************************************
# PASO 3.2. Calidad del Modelo Log�stico
#*******************************************************************************
lipsitz.test(modelo1LogisticoOrdinal)

prediccion <- fitted(modelo1LogisticoOrdinal)

# PROBABILIDADES DEL MODELO - AN�LISIS DE RESULTADOS
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
# Es un modelo que es diferente a la regresi�n lineal.
# No hace falta que los residuos sean normales para que sea v�lido
#
# Aspectos a considerar:
#
# 1. Las variables predictivas
#   . �Las variables de entrada son significativas? (p-valores < 0.05)
#   . �Necesitamos m�s predictoras? (hay otras variables que pueden influir...)
#   . �Necesitamos interacciones cruzadas? variable1*variable2
#   . �Hay colinearidad entre variables? VIF < 4
# Advertencia: cuando m�s peque�o sea el modelo m�s f�cil es de interpretar. Apuesta por modelos sencillos
#
# 2. Restricciones
#   . No hay colinearidad entre variables predictoras
#   . Linealidad entre log Odds ordinales vs variables dependientes

# 3. Exactitud del modelo (accuracy) --> Machine Learning
#   . Curva de ROC para regresiones log�sticas de variables dicot�micas
#   . Misclassification Error (cuanto m�s peque�o mejor es el modelo)
#   . Concordancia (cuando m�s cercano a 1# a. CURVA ROC -> v�lido para modelos log�sticos con variables dicot�micas)

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
