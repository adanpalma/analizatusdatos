# importar datos
datos = iris

# seleccionamos una variable cuantativa y un factor
diffPet_Sep <-iris[c(1)]-iris[c(3)]
names(diffPet_Sep) <- "P_S.Length"
dfTOT <- cbind(diffPet_Sep,datos[5])


# Seleccionamos solo los de setosa
aux <- dfTOT[,2]=="setosa"
df <- dfTOT[aux,][1]
valorCritico <- 3 # 3 cm


# 1- Descripción

library(ggplot2)
# Histograma de frecuencias con ggplot2
name1 <- names(df)
x <- df[,name1]
n <- length(x)
binwidth <- 3.49*sd(x)/(n^(1/3))
ggplot(df,aes(x=df[,name1])) + 
  geom_histogram(binwidth = binwidth,
                 colour="black", fill="#FF6666") +
  geom_vline(aes(xintercept=mean(valorCritico, na.rm=T,colour = "Mean")),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  xlab("Rango de clases") + ylab("Frecuencia absoluta") +
  theme_minimal() +
  ggtitle(paste("Histograma de Frecuencias", name1,sep = "")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual("Legend title", values = c("black", "red"))


# 2 Comparar la media con un valor

dvector <- df[,1]
shapiro.test(dvector)


# --> TEST PARAMÉTRICO
# Si es normal --> T-test de una muestra
t.test(dvector,mu = valorCritico)
# La pregunta puede ser menor que el valor crítico o mayor que el valor crítico:
# · Mayor que el valor crítico
t.test(dvector,mu = valorCritico,alternative = "less")
# · Menor que el valor crítico
t.test(dvector,mu = valorCritico,alternative = "greater")


# --> TEST NO PARAMÉTRICO
# Si NO es normal --> Wilcoxon de una muestra
wilcox.test(dvector,mu = valorCritico)
# La pregunta puede ser menor que el valor crítico o mayor que el valor crítico:
# · Mayor que el valor crítico
wilcox.test(dvector,mu = valorCritico,alternative = "less")
# · Menor que el valor crítico
wilcox.test(dvector,mu = valorCritico,alternative = "greater")


