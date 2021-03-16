

# INSTALAR PAQUETES DE FUNCIONES
#*******************************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("aod","ggplot2","plotly","InformationValue","MASS","sandwich")

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

# La cantidad de premios obtenidos por los estudiantes en una escuela secundaria.
# Los predictores de la cantidad de premios ganados incluyen el tipo de programa
# en el que el estudiante estuvo inscrito (por ejemplo, vocacional, general o académico)
# y la nota en su examen final de matemáticas.

# num_awards es la variable respuesta (de salida) e indica la cantidad de premios obtenidos por los estudiantes
# en una escuela secundaria en un año,
# las matemáticas son una variable predictora continua y representan las notas de los estudiantes en su examen final de matemáticas
# y prog es una variable predictora categórica con tres niveles el tipo de programa en el que los estudiantes se inscribieron.
# Está codificada como 1 = "General", 2 = "Académico" y 3 = "Vocacional".

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
# en el que el estudiante estuvo inscrito (por ejemplo, vocacional, general o académico)
# y la nota en su examen final de matemáticas.

# num_awards es la variable respuesta (de salida) e indica la cantidad de premios obtenidos por los estudiantes
# en una escuela secundaria en un año,
# las matemáticas son una variable predictora continua y representan las notas de los estudiantes en su examen final de matemáticas
# y prog es una variable predictora categórica con tres niveles el tipo de programa en el que los estudiantes se inscribieron.
# Está codificada como 1 = "General", 2 = "Académico" y 3 = "Vocacional".


# Variables:
#************
# Variable Respuesta: num_awards (número de alumnos premiados) -> num. discreta y contaje
# Variable Entrada:
#   · math nota del exámen de mates (numérica discreta)
#   · prog (Tipo de programa -> 1 = "General", 2 = "Académico" y 3 = "Vocacional") Categórica

# Estructura del modelo
#***********************
#   ·Variable Respuesta:  num_awards -> CONTAJE
#   ·Variables Predictoras: prog y math
#   ·Modelo: Poisson
#   ·Familia: Poisson (logarítmica)

# Consideraciones
# *****************
#   ·Que las variables predictoras no dependan entre sí (NO colinealidad)
#   ·Que haya una dependencia lineal entre logaritmo de la variable respuesta con las predictoras


# PASO 2.1: EXPLORACIÓN - MATRIXPLOT
#*******************************************************************************
# 2.1 Estadística descriptiva
# Resumen
summary(varEstudio)

# Media y desviación por grupo de programa
with(varEstudio, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

# Distribución de la variable respuesta -> Es un contaje (barplot)
ggplot(varEstudio, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")


#*******************************************************************************
# ANÁLISIS: Modelo Poisson (regresión de una variable contaje)
#*******************************************************************************


# MODELO 
#*******************************************************************************
# PASO 3.1. Cálculo Modelo Poisson
#*******************************************************************************
modelo1Poisson <- glm(num_awards ~ prog + math, family="poisson", data=varEstudio)
# Resultados del modelo logístico
summary(modelo1Poisson) # todos los coeficientes son significativos!!!

# Intervalo de confianza de los coeficientes con Los errores estándar robustos
m1 <- modelo1Poisson
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
r.est

#*******************************************************************************
# PASO 3.2. Calidad del Modelo Logístico
#*******************************************************************************
# ¿El modelo es aceptable?
# Podemos utilizar la Chi Cuadrado goodness of fit
with(modelo1Poisson, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

# p-valor es grande tenemos un modelo aceptable
# Si no es aceptable se puede mirar la sobre dispersión de la variable respuesta ¿Cómo?
# Mirando el coeficiente de dispersión. Si es < 1 no tenemos sobredispersion
summary(glm(num_awards ~ prog + math, family="quasipoisson", data=varEstudio))
# Tenemos un poquito de sobre dispersión. Está en el borde.
# Si tenemos sobredispersión puedes trabajar con el modelo bonimal negativo. Voy a hacer los dos

# Plot residuos
plot(residuals(modelo1Poisson))

#*************************************************************
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
#   . Linealidad entre la variable y logarítmica y las variables predictoras
#   · La sobredispersión <1 para las de Poisson. Si no utilizar binomial Negativa

# 3. Exactitud del modelo (accuracy) --> Machine Learning
#   . El test de goodness of fit de Chi Cuadrado de los residuos
#   · Podemos comparar modelos con la ANOVA o el BIC

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
# PASO 3.1. Cálculo Modelo Poisson
#******************************************************************************* 
modelo1PoissonNeg <- glm.nb(num_awards ~ prog + math, link = log, data=varEstudio)
# Resultados del modelo logístico
summary(modelo1PoissonNeg) # todos los coeficientes son significativos!!!

# Intervalo de confianza de los coeficientes con Los errores estándar robustos
m1 <- modelo1PoissonNeg
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
r.est

#*******************************************************************************
# PASO 3.2. Calidad del Modelo Logístico
#*******************************************************************************
# ¿El modelo es aceptable?
# Podemos utilizar la Chi Cuadrado goodness of fit
with(modelo1PoissonNeg, cbind(res.deviance = deviance, df = df.residual,
                           p = pchisq(deviance, df.residual, lower.tail=FALSE)))

# p-valor es grande tenemos un modelo aceptable
# Si no es aceptable se puede mirar la sobre dispersión de la variable respuesta ¿Cómo?
# Mirando el coeficiente de dispersión. Si es < 1 no tenemos sobredispersion
summary(glm(num_awards ~ prog + math, family="quasipoisson", data=varEstudio))
# Tenemos un poquito de sobre dispersión. Está en el borde.
# Si tenemos sobredispersión puedes trabajar con el modelo bonimal negativo. Voy a hacer los dos

# Plot residuos
plot(residuals(modelo1Poisson))

#*************************************************************
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
#   . Linealidad entre la variable y logarítmica y las variables predictoras
#   · La sobredispersión <1 para las de Poisson. Si no utilizar binomial Negativa

# 3. Exactitud del modelo (accuracy) --> Machine Learning
#   . El test de goodness of fit de Chi Cuadrado de los residuos
#   · Podemos comparar modelos con la ANOVA o el BIC

# Analizamos las diferencias entre un modelo de Poisson y Binomial Negativa
# Vamos a comparar dos modelos
#   - m1 con la variable prog
#   - m2 sin la variable prog
m1 <- glm(num_awards ~ prog + math, family="poisson", data=varEstudio)
m2 <- glm.nb(num_awards ~ prog + math, link = log, data=varEstudio)
# Resultados del modelo logístico
anova(m1,m2,test="Chisq")
# Son idénticos