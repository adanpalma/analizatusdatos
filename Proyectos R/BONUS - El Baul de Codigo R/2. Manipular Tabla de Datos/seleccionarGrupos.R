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

# Fila categ�rica: la n�mero 5
# Los dos nombres son "versicolor" y "setosa"


# El s�mbolo | INDICA "OR"
filasVersicolorYSetosa <- (misDatos[,5]=="versicolor") | (misDatos[,5]=="setosa")
# Copio las filas de versicolor y setosa. La primera columna ser� el sepal.length, la segunda
# species
varEstudio <- misDatos[filasVersicolorYSetosa,c(1,5)]
# Ahora tienes una columna con sepal.length y la segunda de species con versicolor y setosa