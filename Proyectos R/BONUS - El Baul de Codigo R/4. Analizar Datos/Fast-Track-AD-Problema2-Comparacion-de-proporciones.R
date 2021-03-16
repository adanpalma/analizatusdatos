#*******************************************************************************

# PROBLEMA NÚMERO 2 - COMPARACIÓN DE PROPORCIONES

#*******************************************************************************
#-------------------------------------------------------------------------------


#*******************************************************************************
# PASO 0 - LEER TUS DATOS
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************


# El problema: tienes dos grupos:
#   .	Grupo A sujetos con cáncer de pulmón: n = 500
#   .	Grupo B sujetos sanos: n = 500
# 
# Por otro lado tienes el número de fumadores:
#   .	Group A de pacientes de cáncer de pulmón: n = 500, Tiene 490 fumadores, es decir una proporción de  pA=490/500
#   .	Group B, los sujetos sanos: n = 500, con 400 fumadores, pB=400/500=80

# Construimos la tabla de contingencias:
tablaContingencias <- matrix(c(490, 400,10,100),2,2)
colnames(tablaContingencias) <- c("Fumadores","No Fumadores")
rownames(tablaContingencias) <- c("Cancer","No Cancer")

#*******************************************************************************
# PASO 1 - ENTENDER TUS DATOS - DEFINIR EL OBJETIVO
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

# 1.1. ¿De dónde provienen la base de datos?

# Son datos de una muestra inventada de fumadores y no fumadroes que han padecido cáncer
# de pulmón.

# 1.2. ¿Qué variables tienes?

# Son dos variables categóricas:
# Fumador, NO fumador
# Cáncer de Pulmón, NO han padecido la enfermedad

# 1.3. ¿Qué objetivo tienes para este estudio?
# La PREGUNTA: si la proporción de fumadores es la misma en los grupos de sujetos
# sanos y con cáncer de pulmón


#*******************************************************************************
# PASO 2 - EXPLORAR LOS DATOS (EXCEL)
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************


#*******************************************************************************
# PASO 3 - ANÁLISIS
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

# En este caso comparamos 2 PROPORCIONES es decir tablas 2x2:
# podemos utilizar el Z-test cuando tienes muestras grandes
# Test exacto de Fisher para muestras pequeñas, alguna menore de 5



# Definimos la propabilidad teórica de interés
#*******************************************************************************
# Si la cantidad de fumadores es más grande en el grupo sanos o enfermos de 
# cáncer de pulmón.

# Queremos ver si es diferente.
# En la x: ponemos Los FUMADORES y en la n: la proporción de cáncer y NO cáncer
prop.test(x = c(490, 400), n = c(500, 500))
# Lo puedes hacer con la tabla de contingencias
prop.test(tablaContingencias)
# Estadísticamente las proporción es diferente al 50%

# Alternativa 1: Podemos calcular si es mayor: p>50%
prop.test(x = c(490, 400), n = c(500, 500),
          alternative = "greater")
#  lo puedes hacer con la tabla de contingencias
prop.test(tablaContingencias,alternative = "greater")

# Alternativa 2: Podemos calcular si es menos:p>50%
prop.test(x = c(490, 400), n = c(500, 500),
          alternative = "less")
#  lo puedes hacer con la tabla de contingencias
prop.test(tablaContingencias,alternative = "less")

# También puedes utilizar el test Exacto de Fisher:
fisher.test(tablaContingencias)


#*******************************************************************************
# PASO 3: APLICA LA PLANTILLA
#*******************************************************************************

# En un word....
