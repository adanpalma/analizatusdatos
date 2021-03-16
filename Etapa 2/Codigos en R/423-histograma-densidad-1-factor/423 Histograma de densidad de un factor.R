# importar datos
datos = mtcars

# leemos un data frame con la primera columna una variable cuantitativa y la segunda variable el factor
df = datos[c(1,10)]
df[,2] = as.factor(df[,2])


library(ggplot2)
# Histograma por factor con ggplot2
p<-ggplot(df, aes(x=df[,1]))+
  geom_histogram(color="black", fill="white")+
  facet_grid(df[,2] ~ .)
p



library(ggplot2)
# Histograma de densidad para comparar grupos
name1 <- names(df)[1]
name2 <- names(df)[2]
ggplot(df, aes(df[,1], fill = df[,2])) +
  geom_density(alpha = 0.2) +
  xlab("Titulo eje X") + ylab("Densidad") +
  theme_minimal() +
  ggtitle("Titulo del grafico") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=""))

