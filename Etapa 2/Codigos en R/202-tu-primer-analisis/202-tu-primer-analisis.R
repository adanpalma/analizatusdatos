# CARGAR PAQUETES DE FUNCIONES
library(readxl)
library(ggpubr)

## LEER DATOS
datos <- read_excel("espalda-para-R.xlsx")

# Manipular la tabla de Datos
diff_ODI = datos[,9]-datos[,10] #calculo la variable diff_ODI
names(diff_ODI) = "Diff_ODI" #cambio el nombre de la variable a "diff_ODI"
datos = cbind(datos,diff_ODI) # añado una columna con la nueva variable diff_ODI

## DESCRIPCIÓN

# estadísticos descriptivos de ODI mes0, ODI mes 1 y diff_ODI
df <- datos[,c(9,10,12)]
resumen_numerico1 <- t(do.call(data.frame, 
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

resumen_numerico1

# estadisticos descriptivos de ODI mes 0, ODI mes 1 y diff_ODI GRUPO CONVENCIONAL
grupoConvencional = datos[,11]=="Convencional"

df <- datos[grupoConvencional,c(9,10,12)]
resumen_numerico_CONVENCIONAL <- t(do.call(data.frame, 
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

resumen_numerico_CONVENCIONAL


# estadisticos descriptivos de ODI mes 0, ODI mes 1 y diff_ODI GRUPO AVANZADO
grupoAvanzado = datos[,11]=="Avanzado"

df <- datos[grupoAvanzado,c(9,10,12)]
resumen_numerico_AVANZADO <- t(do.call(data.frame, 
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

resumen_numerico_AVANZADO

## GRÁFICOS

df = datos[,c(12,11)]
# Boxplot con el paquete ggpubr
ggboxplot(df, x = names(df)[2], y = names(df)[1],
          color = names(df)[2])

# StripChart con el paquete ggpubr
ggstripchart(df, x = names(df)[2], y = names(df)[1],
             color = names(df)[2])

# Histograma de densidad para comparar grupos
name1 <- names(df)[1]
name2 <- names(df)[2]
ggplot(df, aes(df[,1], fill = df[,2])) +
  geom_density(alpha = 0.2) +
  xlab("Rango de clases") + ylab("Densidad") +
  theme_minimal() +
  ggtitle(paste("Histograma de Densidad", name1,"comparando por grupos de ",name2)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=""))


## ANÁLISIS

# T-test con igualdad de varianzas con un data frame
df = datos[,c(12,11)]
t.test(Diff_ODI ~ Grupo, data = df, var.equal = FALSE)
