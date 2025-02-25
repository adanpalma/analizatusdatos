#*******************************************************************************
# Marcar Variables num�ricas y categoricas
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

misDatos <- iris
# Puedes utilizar este comando para diferenciar tres tipos de variables:
# 1-Numeric: variables num�ricas reales
# 2-Integer: son variables con n�meros enteros.
#           Puede ser num�rica discreta o categ�rica (tienes que decidir donde crees que ser�)
# 3-Factor: variables con caracteres. Siempre seran categ�ricas.
str(misDatos)
variables <- split(names(misDatos),sapply(misDatos, function(x) paste(class(x), collapse=" ")))

# La variable edad es num�rica
varNumericas <- c(variables$numeric,variables$integer)

# Las variables categ�ricas son los factores.
varCategoricas <- variables$factor