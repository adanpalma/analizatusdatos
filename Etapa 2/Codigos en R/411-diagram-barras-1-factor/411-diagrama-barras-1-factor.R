

# importar datos - leer datos - ejemplo 1
datos <- iris
df <- datos[5]




# La entrada es el fata frame de la variable cualitativa llamada counts
counts <- table(df)



# Diagrama de barras de 1 factor - base

barplot(counts, main="Título del diagrama de barras", 
        xlab="Título del eje X")




# tabla de frecuencias de un factor
tfrec <- as.data.frame(table(df))
colnames(tfrec) <- c("Species","Frecuencia")

df <- tfrec # la entrada es un data frame de la tabla de frecuencias de 1 factor



library(ggpubr)

# Diagrama de barras de 1 factor - paquete ggpubr

name1 <- colnames(tfrec[1])
ggbarplot(df, x = name1, y = "Frecuencia")




df <- datos[5] # la entrada es un data frame del factor de estudio


library(ggplot2)

# Diagrama de barras de frecuencia de una variable numérica
name1 <- names(df)[1]
p <- ggplot(df, aes(x=df[,name1])) +
           geom_bar(colour="black", fill="#DD8888", width=.2)+
           xlab(name1) + ylab("Frecuencia Absoluta") +
           theme_minimal() +
           ggtitle(paste("Diagrama de Barras de la variable ",name1)) +
           theme(plot.title = element_text(hjust = 0.5))




library(plotly)

# Diagrama de barras de frecuencia de una variable numérica
name1 <- names(df)[1]
ggplotly(p)





# importar datos - leer datos - ejemplo 2
datos <- HairEyeColor # son datos tipo tabla (no es usual leer los datos así)
df_bruto <- as.data.frame(HairEyeColor) # los convertimos a data frame

# preproceso de datos
hair_eye_col <- df_bruto[rep(row.names(df_bruto), df_bruto$Freq), 1:3]
rownames(hair_eye_col) <- 1:nrow(hair_eye_col)
head(hair_eye_col) # tabla de frecuencias de 3 variables cualitativas

df <- hair_eye_col # este el data frame que deberías tener (por ejemplo seleccionando 3 variables cualitativas)





# La entrada es el fata frame de la variable cualitativa llamada counts
counts <- table(df$Hair)


# Diagrama de barras de 1 factor - base
barplot(counts, main="Título del diagrama de barras", 
        xlab="Título del eje X")





# tabla de frecuencias de un factor
tfrec <- as.data.frame(table(df$Eye))
colnames(tfrec) <- c("Color_Ojos","Frecuencia")

# la entrada es un data frame de la tabla de frecuencias de 1 factor "tfrec"


library(ggpubr)

# Diagrama de barras de 1 factor - paquete ggpubr

name1 <- colnames(tfrec[1])
ggbarplot(tfrec, x = name1, y = "Frecuencia")



# la variable de entrada es df
df <- hair_eye_col[1]
  
library(ggplot2)

# Diagrama de barras de frecuencia de una variable numérica
name1 <- names(df)[1]
p <- ggplot(df, aes(x=df[,name1])) +
  geom_bar(colour="black", fill="#DD8888", width=.2)+
  xlab(name1) + ylab("Frecuencia Absoluta") +
  theme_minimal() +
  ggtitle(paste("Diagrama de Barras de la variable ",name1)) +
  theme(plot.title = element_text(hjust = 0.5))

p


library(plotly)
ggplotly(p)






# tabla de frecuencias de un factor
tfrec <- table(df$Hair)
tfrec <- as.data.frame(tfrec)
colnames(tfrec) <- c("Hair","Frec")



df <- tfrec



library(ggpubr)

# Diagrama de barras de 1 factor - paquete ggpubr

name1 <- colnames(df[1])
ggbarplot(df, x = name1, y = "Frec")



df <- hair_eye_col[1]

library(ggplot2)

# Diagrama de barras de frecuencia de una variable numérica
name1 <- names(df)[1]
ggplot(df, aes(x=df[,name1])) +
  geom_bar(colour="black", fill="#DD8888", width=.2)+
  xlab(name1) + ylab("Frecuencia Absoluta") +
  theme_minimal() +
  ggtitle(paste("Diagrama de Barras de la variable ",name1)) +
  theme(plot.title = element_text(hjust = 0.5))
