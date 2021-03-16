#*******************************************************************************
# SELECCIONAR GRUPOS
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************


misDatos <- iris

#-------------------------------------------------------------------------------


# COPIA LAS VARIABLES QUE ME INTERESAN
#-------------------------------------------------------------------------------
# Encuentro las filas que sean de versicolor o setosa

# Fila categórica: la número 5
# Los dos nombres son "versicolor" y "setosa"


# El símbolo | INDICA "OR"
filasVersicolorYSetosa <- (misDatos[,5]=="versicolor") | (misDatos[,5]=="setosa")
# Copio las filas de versicolor y setosa. La primera columna será el sepal.length, la segunda
# species
varEstudio <- misDatos[filasVersicolorYSetosa,c(1,5)]
# Ahora tienes una columna con sepal.length y la segunda de species con versicolor y setosa