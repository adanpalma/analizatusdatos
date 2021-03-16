
# importar datos
datos <- iris


# seleccionamos una variable en forma de vector (valores)
dvector = datos[,2]


# histograma (valores)
hist(dvector)



# histograma con más cositas
hist(dvector,breaks = 20,main = "Histograma de la variable 1",xlab = "Título Eje X",ylab = "Título Eje Y")



# histograma de densidad
hist(dvector,breaks = 20,freq = FALSE)



# tabla de frecuencias
p = hist(dvector,breaks = 20)
tablaFrec = data.frame(clases=p$mids,frec=p$counts,densidad=p$density)






library(ggplot2)


# histograma con ggplot2 (se necesita leer en forma de data.frame de una variable)

nombres <- names(datos) # nombres de las variables
sel = 3 # seleccionamos la columna 3
df = as.data.frame(datos[,sel])
names(df)=nombres[sel]

# Histograma de frecuencias con ggplot2
name1 <- names(df)
x <- df[,name1]
n <- length(x)
#binwidth <- 3.49*sd(x)/(n^(1/3))
binwidth <- 0.25
ggplot(df,aes(x=df[,name1])) + 
  geom_histogram(binwidth = binwidth,
                 colour="black", fill="#FF6666") +
  geom_vline(aes(xintercept=mean(x, na.rm=T,colour = "Mean")),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  xlab("Título del eje X") + ylab("Título del eje Y") +
  theme_minimal() +
  ggtitle("Título del Gráfico") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual("Legend title", values = c("black", "red"))





library(plotly)

# Histograma de frecuencias con plotly 
  # (leer un data frame df de una variable numérica)

nombres <- names(datos) # nombres de las variables
sel = 4 # seleccionamos la columna 4
df = as.data.frame(datos[,sel])
names(df)=nombres[sel]

name1 <- names(df)
plot_ly(x = df[,name1], type = "histogram",name = name1,marker=list(color='#FF6666'))%>%
  layout(title = "Título del Gráfico",yaxis = list(title = "Título del eje X"),bargap=0.005*(max(df[,name1])-min(df[,name1])))

