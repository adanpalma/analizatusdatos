
# importar datos
datos <- iris


# seleccionamos una variable en forma de vector (valores)
dvector = iris[,3]



# histograma de densidad
hist(dvector,breaks = 20,freq = FALSE)



# histograma con más cositas
hist(dvector,breaks = 20,freq = FALSE, main = "Título del Gráfico",xlab = "Título Eje X",ylab = "Título Eje Y")



# tabla de frecuencias
p = hist(dvector,breaks = 20)
tablaFrec = data.frame(clases=p$mids,frec=p$counts,densidad=p$density)






library(ggplot2)


# histograma de densidad con ggplot2 (se necesita leer en forma de data.frame de una variable)

nombres <- names(datos) # nombres de las variables
sel = 3 # seleccionamos la columna 3
df = as.data.frame(datos[,sel])
names(df)=nombres[sel]

# Histograma de densidad ...
name1 <- names(df)
x <- df[,name1]
n <- length(x)
#binwidth <- 3.49*sd(x)/(n^(1/3))
binwidth <- 0.25
ggplot(df,aes(x=df[,name1])) + 
  geom_histogram(aes(y=..density..,colour = "Densidad"),      # Histogram with density instead of count on y-axis
                 binwidth = binwidth,
                 colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(x, na.rm=T,colour = "Mean")),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  geom_density(alpha=.2, fill="#FF6666")  + # Overlay with transparent density plot
  xlab("Título del eje x") + ylab("Densidad") +
  theme_minimal() +
  ggtitle("Título del gráfico") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual("Legend title", values = c("black", "red"))






library(plotly)

# Histograma de frecuencias con plotly 
  # (leer un data frame df de una variable numérica)

nombres <- names(datos) # nombres de las variables
sel = 4 # seleccionamos la columna 4
df = as.data.frame(datos[,sel])
names(df)=nombres[sel]

# Histograma de densidad con plotly
name1 <- names(df)
plot_ly(x = df[,name1], type = "histogram",name = name1,marker=list(color='#FF6666'), histnorm = "probability density")%>%
  layout(title = paste("Histograma de Densidad",name1),yaxis = list(title = name1),bargap=0.005*(max(df[,name1])-min(df[,name1])))
