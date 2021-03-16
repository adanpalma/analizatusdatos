
# importar datos
datos <- iris


# seleccionamos una variable en forma de vector (valores)
dvector = datos[,2]

# dibujamos el boxplot
boxplot(dvector)


# Identificar valores atipicos
boxplot.stats(dvector)
boxplot.stats(dvector)$out

aux = boxplot.stats(dvector)

# Identificar la fila de la observacion con valores atipicos
outliers <- aux$out
id = which(dvector %in% outliers)
dvector[id]