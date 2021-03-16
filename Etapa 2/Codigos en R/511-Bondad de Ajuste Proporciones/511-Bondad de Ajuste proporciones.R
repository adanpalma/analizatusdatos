#
# ETAPA 2: ANALIZA LO MÁS SIMPLE
#
#*******************************************************************************
#   PASO 0: CONSIGUE TU BASE DE DATOS
#   PASO 1: QUÉ ESTÁS BUSCANDO
#   PASO 2: UTILIZA EL MAPA PARA ORIENTARTE
#   PASO 3: APLICA LA PLANTILLA
#*******************************************************************************
#-------------------------------------------------------------------------------

#*******************************************************************************
# PASO 0: CONSIGUE TU BASE DATOS Y LÉELA CON INTELIGENCIA (PROBLEMA)
#*******************************************************************************

# Tienes una población de ratones que contiene la misma proporción de machos y hembras (p = 0,5 = 50%).
# Algunos de estos ratones (n = 160) han desarrollado un cáncer espontáneo.
# Se sabe que 95 son machos y 65 hembras.




#*******************************************************************************
# PASO 1: QUÉ ESTÁS BUSCANDO
#*******************************************************************************

# La PREGUNTA: La pregunta de investigación es si el cáncer afecta más a machos que hembras.

#*******************************************************************************
# PASO 2: UTILIZA UN MAPA PARA ORIENTARTE
#*******************************************************************************

# En este caso comparamos 1 VARIABLE CATEGÓRICA DE 2 GRUPOS CON UNA PROPORCIÓN: podemos comparar
# la proporción con el Z-test para comparar una proporción experimental con una
# teórica cuando tenemos solo 2 grupos (categórica dicotómica)

# En la práctica:
# . Definimos la propabilidad teórica de interés
# · Hacemos el Z-test


# Definimos la propabilidad teórica de interés
#*******************************************************************************
# El 50% es la probabilidad de interés de la proporción de cáncer en machos y hembras
# Queremos ver si es diferente.
prop.test(x = 95, n = 160, p = 0.5, 
          correct = FALSE)
# Estadísticamente las proporción es diferente al 50%

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
