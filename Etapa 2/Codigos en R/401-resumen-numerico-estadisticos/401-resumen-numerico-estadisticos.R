
# importar datos
datos <- iris

# escojemos una variable numerica en forma de vector de valores
dvector <- datos[,1]

# Media
mean(dvector)

# Mediana
median(dvector)

# Desviacion estanndar
sd(dvector)

# Rango intercuartilico
IQR(dvector)

# Mínimo
min(dvector)

# Máximo
max(dvector)

# Rango o amplitud
range(dvector)
diff(range(dvector))

# Num de observaciones
length(dvector)

# Error estandar
EE = sd(dvector)/sqrt(length(dvector))

# IC 95%
c(mean(dvector)-1.96*EE,mean(dvector)+1.96*EE)

# Cuartiles
quantile(dvector)

# Percentiles 90 y 95
quantile(dvector,probs = c(0.9,0.95))


# Varianza
var(dvector)
sd(dvector)^2

# Resumen
summary(dvector)



# Resumen completo en forma de tabla
# La variable de entrada se llama df y un data frame (antes eran valores)
nombres <- names(datos) # nombres de las variables
sel = 1 # seleccionamos la columna 1
df = as.data.frame(datos[,sel])
names(df)=nombres[sel]

estadisticaDescriptiva <- t(do.call(data.frame, 
                                    list(mean = apply(df, 2, mean),
                                         Desv.Estandar = apply(df, 2, sd),
                                         Mediana = apply(df, 2, median),
                                         IQR = apply(df,2,IQR),
                                         Min = apply(df, 2, min),
                                         Max = apply(df, 2, max),
                                         Rango = apply(df, 2,max)-apply(df, 2,min),
                                         Cuartil1 = apply(df,2,quantile,probs = c(0.25)),
                                         Cuartil3 = apply(df,2,quantile,probs = c(0.75)),
                                         N = apply(df,2,length),
                                         ErrorEstandar = apply(df,2,sd)/sqrt(apply(df,2,length)),
                                         IC95MediaLower = apply(df,2,mean)-1.96*apply(df,2,sd)/sqrt(apply(df,2,length)),
                                         IC95MediaUpper = apply(df,2,mean)+1.96*apply(df,2,sd)/sqrt(apply(df,2,length)),
                                         Varianza = apply(df, 2, var),
                                         Suma = apply(df,2,sum)
                                    )))


# Resumen completo de todas variables cuantitativas
df = datos[,c(1,2,3,4)] # seleccionamos todas las variables cuantitativas
estadisticaDescriptiva <- t(do.call(data.frame, 
                                    list(mean = apply(df, 2, mean),
                                         Desv.Estandar = apply(df, 2, sd),
                                         Mediana = apply(df, 2, median),
                                         IQR = apply(df,2,IQR),
                                         Min = apply(df, 2, min),
                                         Max = apply(df, 2, max),
                                         Rango = apply(df, 2,max)-apply(df, 2,min),
                                         Cuartil1 = apply(df,2,quantile,probs = c(0.25)),
                                         Cuartil3 = apply(df,2,quantile,probs = c(0.75)),
                                         N = apply(df,2,length),
                                         ErrorEstandar = apply(df,2,sd)/sqrt(apply(df,2,length)),
                                         IC95MediaLower = apply(df,2,mean)-1.96*apply(df,2,sd)/sqrt(apply(df,2,length)),
                                         IC95MediaUpper = apply(df,2,mean)+1.96*apply(df,2,sd)/sqrt(apply(df,2,length)),
                                         Varianza = apply(df, 2, var),
                                         Suma = apply(df,2,sum)
                                    )))