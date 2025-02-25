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

varEstudio <- esoph
table(varEstudio [,c("agegp", "ncases")])
casos_Si <- tapply(varEstudio[,"ncases"],varEstudio[,"agegp"], sum)
casos_No <- tapply(varEstudio[,"ncontrols"],varEstudio[,"agegp"], sum)
casos_TOTAL <- tapply(varEstudio[,"ncontrols"]+varEstudio[,"ncases"], varEstudio[,"agegp"], sum)

tablaContingencias <- rbind(casos_Si,casos_No)

#*******************************************************************************
# PASO 1: QU� EST�S BUSCANDO
#*******************************************************************************

# La PREGUNTA: La pregunta de investigaci�n es si es diferente las proporciones entre
# los sujetes de que tienes c�ncer por edad.

# Tenemos m�s de dos proporciones: Z-test 

#*******************************************************************************
# PASO 2: UTILIZA UN MAPA PARA ORIENTARTE
#*******************************************************************************

# En este caso comparamos m�s de 3 proporciones.
# Tenemos de construir la tabla de contingencia de los casos Si y los casos que NO
# y comparar proporciones.


# EL TEST DE IGUALDAD DE PROPORCIONES
#*****************************************************************************
# casos_Si: es el vector de casos de c�ncer por edad
# casos_No: es el vector de casos de NO c�ncer por edad
prop.test(casos_Si, casos_TOTAL)


# PUEDES MIRAR LA Tendencia lineal si la variable categ�rica es ORDINAL
#*****************************************************************************
# Puedes probar una tendencia lineal en las proporciones usando prop.trend.test. La
# hip�tesis nula es que no hay tendencia en las proporciones; La alternativa es que
# hay un aumento / disminuci�n lineal en la proporci�n a medida que sube / baja en
# categor�as. Nota: solo desear�a realizar esta prueba si su variable categ�rica era
# una variable ordinal. Usted no har�a esto, por ejemplo, la afiliaci�n a un partido
# pol�tico o el color de los ojos.

prop.trend.test(casos_Si, casos_TOTAL)

#*******************************************************************************
# PASO 3: APLICA LA PLANTILLA
#*******************************************************************************

# En un word....
