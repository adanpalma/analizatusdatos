#*******************************************************************************
# Marcar Variables numéricas y categoricas
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

misDatos <- iris
# Puedes utilizar este comando para diferenciar tres tipos de variables:
# 1-Numeric: variables numéricas reales
# 2-Integer: son variables con números enteros.
#           Puede ser numérica discreta o categórica (tienes que decidir donde crees que será)
# 3-Factor: variables con caracteres. Siempre seran categóricas.
str(misDatos)
variables <- split(names(misDatos),sapply(misDatos, function(x) paste(class(x), collapse=" ")))

# La variable edad es numérica
varNumericas <- c(variables$numeric,variables$integer)

# Las variables categóricas son los factores.
varCategoricas <- variables$factor