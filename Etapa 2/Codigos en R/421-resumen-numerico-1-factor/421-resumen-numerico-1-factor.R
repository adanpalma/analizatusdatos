
# importar datos
datos <- iris

# escojemos una variable cuantitativa y un factor
df <- datos[c(1,5)]


library(dplyr)
# estadísticos por grupos
name1 <- names(df)[1]
name2 <- names(df)[2]
dots = lapply(name2,as.symbol)

resumenNum <- df %>% group_by_at(vars(one_of(name2))) %>%
  summarise(
    count = n(),
    mean = mean(Sepal.Length),
    sd = sd(Sepal.Length),
    sum = sum(Sepal.Length),
    var = var(Sepal.Length),
    med = median(Sepal.Length),
    EE = sd(Sepal.Length)/sqrt(n()),
    IC_lower95 = mean(Sepal.Length)-1.96*sd(Sepal.Length)/sqrt(n()),
    IC_upper95 = mean(Sepal.Length)+1.96*sd(Sepal.Length)/sqrt(n()),
    max = max(Sepal.Length),
    min = min(Sepal.Length),
    iqr = IQR(Sepal.Length)
  )





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