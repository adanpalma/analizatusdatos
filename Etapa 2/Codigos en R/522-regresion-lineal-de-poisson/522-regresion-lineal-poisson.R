

# INSTALAR PAQUETES DE FUNCIONES
#*******************************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("aod","ggplot2","plotly","InformationValue","MASS","sandwich")

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
misDatos <- read.csv(file.choose())
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

# La cantidad de premios obtenidos por los estudiantes en una escuela secundaria.
# Los predictores de la cantidad de premios ganados incluyen el tipo de programa
# en el que el estudiante estuvo inscrito (por ejemplo, vocacional, general o acad�mico)
# y la nota en su examen final de matem�ticas.

# num_awards es la variable respuesta (de salida) e indica la cantidad de premios obtenidos por los estudiantes
# en una escuela secundaria en un a�o,
# las matem�ticas son una variable predictora continua y representan las notas de los estudiantes en su examen final de matem�ticas
# y prog es una variable predictora categ�rica con tres niveles el tipo de programa en el que los estudiantes se inscribieron.
# Est� codificada como 1 = "General", 2 = "Acad�mico" y 3 = "Vocacional".

varEstudio <- misDatos
# identificamos la variable prog como factor
varEstudio <- within(varEstudio, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})



#*******************************************************************************
# PASO 2: UTILIZA UN MAPA PARA ORIENTARTE
#*******************************************************************************

# Queremos relacionar variables con un modelo lineal generalizado

# Objetivo:
#***********
# La cantidad de premios obtenidos por los estudiantes en una escuela secundaria.
# Los predictores de la cantidad de premios ganados incluyen el tipo de programa
# en el que el estudiante estuvo inscrito (por ejemplo, vocacional, general o acad�mico)
# y la nota en su examen final de matem�ticas.

# num_awards es la variable respuesta (de salida) e indica la cantidad de premios obtenidos por los estudiantes
# en una escuela secundaria en un a�o,
# las matem�ticas son una variable predictora continua y representan las notas de los estudiantes en su examen final de matem�ticas
# y prog es una variable predictora categ�rica con tres niveles el tipo de programa en el que los estudiantes se inscribieron.
# Est� codificada como 1 = "General", 2 = "Acad�mico" y 3 = "Vocacional".


# Variables:
#************
# Variable Respuesta: num_awards (n�mero de alumnos premiados) -> num. discreta y contaje
# Variable Entrada:
#   � math nota del ex�men de mates (num�rica discreta)
#   � prog (Tipo de programa -> 1 = "General", 2 = "Acad�mico" y 3 = "Vocacional") Categ�rica

# Estructura del modelo
#***********************
#   �Variable Respuesta:  num_awards -> CONTAJE
#   �Variables Predictoras: prog y math
#   �Modelo: Poisson
#   �Familia: Poisson (logar�tmica)

# Consideraciones
# *****************
#   �Que las variables predictoras no dependan entre s� (NO colinealidad)
#   �Que haya una dependencia lineal entre logaritmo de la variable respuesta con las predictoras


# PASO 2.1: EXPLORACI�N - MATRIXPLOT
#*******************************************************************************
# 2.1 Estad�stica descriptiva
# Resumen
summary(varEstudio)

# Media y desviaci�n por grupo de programa
with(varEstudio, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

# Distribuci�n de la variable respuesta -> Es un contaje (barplot)
ggplot(varEstudio, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")


#*******************************************************************************
# AN�LISIS: Modelo Poisson (regresi�n de una variable contaje)
#*******************************************************************************


# MODELO 
#*******************************************************************************
# PASO 3.1. C�lculo Modelo Poisson
#*******************************************************************************
modelo1Poisson <- glm(num_awards ~ prog + math, family="poisson", data=varEstudio)
# Resultados del modelo log�stico
summary(modelo1Poisson) # todos los coeficientes son significativos!!!

# Intervalo de confianza de los coeficientes con Los errores est�ndar robustos
m1 <- modelo1Poisson
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
r.est

#*******************************************************************************
# PASO 3.2. Calidad del Modelo Log�stico
#*******************************************************************************
# �El modelo es aceptable?
# Podemos utilizar la Chi Cuadrado goodness of fit
with(modelo1Poisson, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

# p-valor es grande tenemos un modelo aceptable
# Si no es aceptable se puede mirar la sobre dispersi�n de la variable respuesta �C�mo?
# Mirando el coeficiente de dispersi�n. Si es < 1 no tenemos sobredispersion
summary(glm(num_awards ~ prog + math, family="quasipoisson", data=varEstudio))
# Tenemos un poquito de sobre dispersi�n. Est� en el borde.
# Si tenemos sobredispersi�n puedes trabajar con el modelo bonimal negativo. Voy a hacer los dos

# Plot residuos
plot(residuals(modelo1Poisson))

#*************************************************************
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
#   . Linealidad entre la variable y logar�tmica y las variables predictoras
#   � La sobredispersi�n <1 para las de Poisson. Si no utilizar binomial Negativa

# 3. Exactitud del modelo (accuracy) --> Machine Learning
#   . El test de goodness of fit de Chi Cuadrado de los residuos
#   � Podemos comparar modelos con la ANOVA o el BIC

# Analizamos el efecto de la variable prog
# Vamos a comparar dos modelos
#   - m1 con la variable prog
#   - m2 sin la variable prog
m1 <- glm(num_awards ~ prog + math, family="poisson", data=varEstudio)
m2 <- update(m1 , . ~ . - prog) # le quitamos la variable prog al modelo 1
anova(m2,m1,test="Chisq")

m2 <- glm(num_awards ~ math,family="poisson", data=varEstudio)
summary(m2)
with(m2, cbind(res.deviance = deviance, df = df.residual,
                           p = pchisq(deviance, df.residual, lower.tail=FALSE)))


# BONUS: Plot de los valores predecidos del modelo 1
## calculate and store predicted values
varEstudio$phat <- predict(m1, type="response")

## order by program and then by math
varEstudioSort <- varEstudio[with(varEstudio, order(prog, math)), ]

## create the plot
ggplot(varEstudioSort, aes(x = math, y = phat, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Math Score", y = "Expected number of awards")


# Los modelos son diferentes por lo tanto tiene influencia la variable prog






# MODELO2: BINOMIAL NEGATIVA
#*******************************************************************************
# PASO 3.1. C�lculo Modelo Poisson
#******************************************************************************* 
modelo1PoissonNeg <- glm.nb(num_awards ~ prog + math, link = log, data=varEstudio)
# Resultados del modelo log�stico
summary(modelo1PoissonNeg) # todos los coeficientes son significativos!!!

# Intervalo de confianza de los coeficientes con Los errores est�ndar robustos
m1 <- modelo1PoissonNeg
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
r.est

#*******************************************************************************
# PASO 3.2. Calidad del Modelo Log�stico
#*******************************************************************************
# �El modelo es aceptable?
# Podemos utilizar la Chi Cuadrado goodness of fit
with(modelo1PoissonNeg, cbind(res.deviance = deviance, df = df.residual,
                           p = pchisq(deviance, df.residual, lower.tail=FALSE)))

# p-valor es grande tenemos un modelo aceptable
# Si no es aceptable se puede mirar la sobre dispersi�n de la variable respuesta �C�mo?
# Mirando el coeficiente de dispersi�n. Si es < 1 no tenemos sobredispersion
summary(glm(num_awards ~ prog + math, family="quasipoisson", data=varEstudio))
# Tenemos un poquito de sobre dispersi�n. Est� en el borde.
# Si tenemos sobredispersi�n puedes trabajar con el modelo bonimal negativo. Voy a hacer los dos

# Plot residuos
plot(residuals(modelo1Poisson))

#*************************************************************
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
#   . Linealidad entre la variable y logar�tmica y las variables predictoras
#   � La sobredispersi�n <1 para las de Poisson. Si no utilizar binomial Negativa

# 3. Exactitud del modelo (accuracy) --> Machine Learning
#   . El test de goodness of fit de Chi Cuadrado de los residuos
#   � Podemos comparar modelos con la ANOVA o el BIC

# Analizamos las diferencias entre un modelo de Poisson y Binomial Negativa
# Vamos a comparar dos modelos
#   - m1 con la variable prog
#   - m2 sin la variable prog
m1 <- glm(num_awards ~ prog + math, family="poisson", data=varEstudio)
m2 <- glm.nb(num_awards ~ prog + math, link = log, data=varEstudio)
# Resultados del modelo log�stico
anova(m1,m2,test="Chisq")
# Son id�nticos