########################
#cargar las librerias
########################
library(readxl)
library(ggpubr)
library(dplyr)

#setear directorio
setwd("C:/Users/apalmad/Desktop/Analiza tus Datos/Bloque 2/Empieza con tu Software")

######################
#leer datos
######################
datos <- read_excel("espalda-para-R.xlsx")

#modifica tabla de datos
diffodi <-  datos[,9] - datos[,10]
names(diffodi) <- "diffodi"
datos <-  cbind(datos,diffodi)
df <- datos[,c(9,10,12)]

############################
#describir los datos
#estadisticos descriptivos odi mes 0, odi mes 1, dif oddi
############################
ResumenNumerico <- t(do.call(data.frame,
                                list(mean     = apply(df,2,mean),
                                     stddev   = apply(df,2,sd),
                                     mediana  = apply(df,2,median),
                                     iqr      = apply(df,2,IQR),
                                     Min      = apply(df,2,min),
                                     Max      = apply(df,2,max),
                                     Rango    = apply(df,2,max) - apply(df,2,min),
                                     Cuartil1 = apply(df,2,quantile,prob = c(0.25)),
                                     Cuartil3 = apply(df,2,quantile,prob = c(0.75)),
                                     N        = apply(df,2,length),
                                     ErrStandard    = apply(df,2,sd) / sqrt(apply(df,2,length)),
                                     IC95MediaLower = apply(df,2,mean) - 1.96 * (apply(df,2,sd) / sqrt(apply(df,2,length))),
                                     IC95MediaUpper = apply(df,2,mean) + 1.96 * (apply(df,2,sd) / sqrt(apply(df,2,length))),
                                     Varianza       =  apply(df,2,var),
                                     Suma         = apply(df,2,sum))))
ResumenNumerico


# GRAFICOS  solo la varible diff odi sin agrupar
hist(datos[,12],main="Hist Diff Odi")
boxplot(datos[,12],main = "Box Plot Diff Odi")

grupoconvencional <- datos[,11] == "Convencional"
                           
df <- datos[grupoconvencional,c(9,10,12)]
ResumenNumConven <- t(do.call(data.frame,
                             list(mean = apply(df,2,mean),
                                  stddev = apply(df,2,sd),
                                  mediana = apply(df,2,median),
                                  iqr    = apply(df,2,IQR),
                                  Min = apply(df,2,min),
                                  Max = apply(df,2,max),
                                  Rango = apply(df,2,max) - apply(df,2,min),
                                  Cuartil1 = apply(df,2,quantile,prob = c(0.25)),
                                  Cuartil3 = apply(df,2,quantile,prob = c(0.75)),
                                  N = apply(df,2,length),
                                  ErrStandard  = apply(df,2,sd) / sqrt(apply(df,2,length)),
                                  IC95MediaLower = apply(df,2,mean) - 1.96 * (apply(df,2,sd) / sqrt(apply(df,2,length))),
                                  IC95MediaUpper = apply(df,2,mean) + 1.96 * (apply(df,2,sd) / sqrt(apply(df,2,length))),
                                  Varianza =  apply(df,2,var),
                                  Suma = apply(df,2,sum))))


ResumenNumConven


grupoavanzado <- datos[,11] == "Avanzado"

df <- datos[grupoavanzado,c(9,10,12)]
ResumenNumAvanzado <- t(do.call(data.frame,
                              list(mean = apply(df,2,mean),
                                   stddev = apply(df,2,sd),
                                   mediana = apply(df,2,median),
                                   iqr    = apply(df,2,IQR),
                                   Min = apply(df,2,min),
                                   Max = apply(df,2,max),
                                   Rango = apply(df,2,max) - apply(df,2,min),
                                   Cuartil1 = apply(df,2,quantile,prob = c(0.25)),
                                   Cuartil3 = apply(df,2,quantile,prob = c(0.75)),
                                   N = apply(df,2,length),
                                   ErrStandard  = apply(df,2,sd) / sqrt(apply(df,2,length)),
                                   IC95MediaLower = apply(df,2,mean) - 1.96 * (apply(df,2,sd) / sqrt(apply(df,2,length))),
                                   IC95MediaUpper = apply(df,2,mean) + 1.96 * (apply(df,2,sd) / sqrt(apply(df,2,length))),
                                   Varianza =  apply(df,2,var),
                                   Suma = apply(df,2,sum))))


ResumenNumAvanzado

###############
# Descripcion de Diff Oddi Separado por Grupo
##############
descrixgrupo <- data.frame(cbind(ResumenNumConven[,3],ResumenNumAvanzado[,3]))
names(descrixgrupo)[1] <- "Convencional"
names(descrixgrupo)[2] <- "Avanzado"
descrixgrupo




############
# Graficos
############
#Histogrmas  & Box Plot para comparar tratamieto (grupo) para la variable diff oddi

df <-  datos[,c(12,11)]
#HISTOGRAMAS
hist(df[grupoconvencional,1],main="Histrograma Tratamiento Convencional")
hist(df[grupoavanzado,1],main="Histrograma Tratamiento Avanzado")

#BOX PLOT
ggboxplot(df,x=names(df)[2],y = names(df)[1],color = names(df)[2])

#graficos de puntos para ver la agrupacion por grupo tratamiento
ggstripchart(df,x=names(df)[2],y = names(df)[1],color = names(df)[2])


#Histograma de Densidad para Oddi Diff por grupo
df <-  datos[,c(12,11)]
namevar1 = names(df[1])
nameg1  = names(df[2])
tipogrupos <-  df %>%
count(Grupo)

  


ggplot(df,mapping = aes(x=df[,1],fill=df[,2]))+
  geom_density(alpha = 0.2) +
  xlab("Tratamientos ") + ylab("Densidad") +
  theme_minimal() +
  labs(title = " Histograma Densidad ",
      subtitle = paste( "Tratamiento ",tipogrupos[2,1]," - ",tipogrupos[1,1]),
      caption = "Data source: Analiza tus Datos",
      x = "Tratamientos", y = "Densidad",
      tag = "Primeros Programas con R") +
          theme(plot.title    = element_text(size =12, face = "bold", hjust = 0.5  ),
                plot.subtitle = element_text(size = 9, hjust = 0.5),
                plot.caption  = element_text(size = 7),
                plot.tag      = element_text(size = 7),
                axis.text     = element_text(size=8),
                axis.title    = element_text(size=8,face="bold"),
                legend.title  = element_text(size = 8,face = "bold"),
                legend.text   = element_text( size = 7))+
       guides(fill = guide_legend(title=nameg1))
 
###################
# Contraste de Hipotesis
##################


t.test(diffodi ~ Grupo, data = df)

                        
                                     