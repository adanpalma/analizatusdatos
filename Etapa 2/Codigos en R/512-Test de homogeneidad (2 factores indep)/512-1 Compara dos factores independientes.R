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

#*******************************************************************************
# PASO 0: CONSIGUE TU BASE DATOS Y L�ELA CON INTELIGENCIA (PROBLEMA)
#*******************************************************************************

# El problema: tienes dos grupos:
#   .	Grupo A sujetos con c�ncer de pulm�n: n = 500
#   .	Grupo B sujetos sanos: n = 500
# 
# Por otro lado tienes el n�mero de fumadores:
#   .	Group A de pacientes de c�ncer de pulm�n: n = 500, Tiene 490 fumadores, es decir una proporci�n de  pA=490/500
#   .	Group B, los sujetos sanos: n = 500, con 400 fumadores, pB=400/500=80

# Construimos la tabla de contingencias:
tablaContingencias <- matrix(c(490, 400,10,100),2,2)
colnames(tablaContingencias) <- c("Fumadores","No Fumadores")
rownames(tablaContingencias) <- c("Cancer","No Cancer")

#*******************************************************************************
# PASO 1: QU� EST�S BUSCANDO
#*******************************************************************************

# La PREGUNTA: si la proporci�n de fumadores es la misma en las dos poblaciones,
# fumadores y NO fumadores


#*******************************************************************************
# PASO 2: UTILIZA UN MAPA PARA ORIENTARTE
#*******************************************************************************

# En este caso comparamos 2 PROPORCIONES es decir tablas 2x2:
# podemos utilizar el Z-test cuando tienes muestras grandes
# Test exacto de Fisher para muestras peque�as, alguna menore de 5



# Definimos la propabilidad te�rica de inter�s
#*******************************************************************************
# Si la cantidad de fumadores es m�s grande en el grupo sanos o enfermos de 
# c�ncer de pulm�n.

# Queremos ver si es diferente.
# En la x: ponemos Los FUMADORES y en la n: la proporci�n de c�ncer y NO c�ncer
prop.test(x = c(490, 400), n = c(500, 500))
# Lo puedes hacer con la tabla de contingencias
prop.test(tablaContingencias)


# Alternativa 1: Podemos calcular si es mayor: pA>pB
prop.test(x = c(490, 400), n = c(500, 500),
          alternative = "greater")
#  lo puedes hacer con la tabla de contingencias
prop.test(tablaContingencias,alternative = "greater")

# Alternativa 2: Podemos calcular si es menos:pA<pB
prop.test(x = c(490, 400), n = c(500, 500),
          alternative = "less")
#  lo puedes hacer con la tabla de contingencias
prop.test(tablaContingencias,alternative = "less")

# Tambi�n puedes utilizar el test Exacto de Fisher:
fisher.test(tablaContingencias)


#*******************************************************************************
# PASO 3: APLICA LA PLANTILLA
#*******************************************************************************

# En un word....
