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

# Tienes una poblaci�n de ratones que contiene la misma proporci�n de machos y hembras (p = 0,5 = 50%).
# Algunos de estos ratones (n = 160) han desarrollado un c�ncer espont�neo.
# Se sabe que 95 son machos y 65 hembras.




#*******************************************************************************
# PASO 1: QU� EST�S BUSCANDO
#*******************************************************************************

# La PREGUNTA: La pregunta de investigaci�n es si el c�ncer afecta m�s a machos que hembras.

#*******************************************************************************
# PASO 2: UTILIZA UN MAPA PARA ORIENTARTE
#*******************************************************************************

# En este caso comparamos 1 VARIABLE CATEG�RICA DE 2 GRUPOS CON UNA PROPORCI�N: podemos comparar
# la proporci�n con el Z-test para comparar una proporci�n experimental con una
# te�rica cuando tenemos solo 2 grupos (categ�rica dicot�mica)

# En la pr�ctica:
# . Definimos la propabilidad te�rica de inter�s
# � Hacemos el Z-test


# Definimos la propabilidad te�rica de inter�s
#*******************************************************************************
# El 50% es la probabilidad de inter�s de la proporci�n de c�ncer en machos y hembras
# Queremos ver si es diferente.
prop.test(x = 95, n = 160, p = 0.5, 
          correct = FALSE)
# Estad�sticamente las proporci�n es diferente al 50%

# Alternativa 1: Podemos calcular si es mayor: p>50%
prop.test(x = 95, n = 160, p = 0.5, correct = FALSE,
          alternative = "greater")

# Alternativa 2: Podemos calcular si es menos:p>50%
prop.test(x = 95, n = 160, p = 0.5, correct = FALSE,
          alternative = "less")


#*******************************************************************************
# PASO 3: APLICA LA PLANTILLA
#*******************************************************************************

# En un word....
