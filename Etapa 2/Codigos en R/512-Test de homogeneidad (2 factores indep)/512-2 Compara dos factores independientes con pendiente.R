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

varEstudio <- esoph
table(varEstudio [,c("agegp", "ncases")])
casos_Si <- tapply(varEstudio[,"ncases"],varEstudio[,"agegp"], sum)
casos_No <- tapply(varEstudio[,"ncontrols"],varEstudio[,"agegp"], sum)
casos_TOTAL <- tapply(varEstudio[,"ncontrols"]+varEstudio[,"ncases"], varEstudio[,"agegp"], sum)

tablaContingencias <- rbind(casos_Si,casos_No)

#*******************************************************************************
# PASO 1: QUÉ ESTÁS BUSCANDO
#*******************************************************************************

# La PREGUNTA: La pregunta de investigación es si es diferente las proporciones entre
# los sujetes de que tienes cáncer por edad.

# Tenemos más de dos proporciones: Z-test 

#*******************************************************************************
# PASO 2: UTILIZA UN MAPA PARA ORIENTARTE
#*******************************************************************************

# En este caso comparamos más de 3 proporciones.
# Tenemos de construir la tabla de contingencia de los casos Si y los casos que NO
# y comparar proporciones.


# EL TEST DE IGUALDAD DE PROPORCIONES
#*****************************************************************************
# casos_Si: es el vector de casos de cáncer por edad
# casos_No: es el vector de casos de NO cáncer por edad
prop.test(casos_Si, casos_TOTAL)


# PUEDES MIRAR LA Tendencia lineal si la variable categórica es ORDINAL
#*****************************************************************************
# Puedes probar una tendencia lineal en las proporciones usando prop.trend.test. La
# hipótesis nula es que no hay tendencia en las proporciones; La alternativa es que
# hay un aumento / disminución lineal en la proporción a medida que sube / baja en
# categorías. Nota: solo desearía realizar esta prueba si su variable categórica era
# una variable ordinal. Usted no haría esto, por ejemplo, la afiliación a un partido
# político o el color de los ojos.

prop.trend.test(casos_Si, casos_TOTAL)

#*******************************************************************************
# PASO 3: APLICA LA PLANTILLA
#*******************************************************************************

# En un word....
