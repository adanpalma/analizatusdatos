# importar datos
datos = mtcars

# leemos un data frame con la primera columna una variable cuantitativa
# la segunda variable el primer factor
# la tercera variable el segundo factor
df = datos[c(1,9,8)]
df[,2] = as.factor(df[,2])
df[,3] = as.factor(df[,3])

# Instrucciones para los plots de diagramas de error
# data: a data frame
# x, y: x and y variables for plotting
# desc_stat: descriptive statistics to be used for visualizing errors. Default value is "mean_se". Allowed values are one of , "mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_mad", "median_range"

library(ggpubr)
# Dibujamos el diagrama de error con la media +/- la dev estandar
ggerrorplot(df, x = names(df)[2], y = names(df)[1], 
            desc_stat = "mean_ci")+
  ggtitle("Titulo del grafico")

library(ggpubr)
# Dibujamos el diagrama de error con la media +/- la dev estandar
ggerrorplot(df, x = names(df)[2], y = names(df)[1], 
            desc_stat = "mean_ci",error.plot = "errorbar",add = "mean")+
  ggtitle("Titulo del grafico")


library(ggpubr)
# Dibujamos el diagrama de error con puntos de la media +/- la dev estandar
ggerrorplot(df, x = names(df)[2], y = names(df)[1], 
            desc_stat = "mean_ci",
            error.plot = "errorbar",
            add = c("jitter","mean"),add.params = list(color = "darkgray"))+
  ggtitle("Titulo del grafico")


library(ggpubr)
# Dibujamos el diagrama de error con el violin de la media +/- la dev estandar
ggerrorplot(df, x = names(df)[2], y = names(df)[1], 
            desc_stat = "mean_sd",
            error.plot = "errorbar",
            add = c("violin","mean"))+
  ggtitle("Titulo del grafico")




library(ggpubr)
# Dibujamos el diagrama de error con el violin de la media +/- la dev estandar
ggerrorplot(df, x = names(df)[2], y = names(df)[1], 
            desc_stat = "mean_sd",
            error.plot = "errorbar",
            add = c("violin","mean"))+
  ggtitle("Titulo del grafico")+ 
  stat_compare_means(comparisons = c("0","1"))+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)                  # Add global p-value



library(ggpubr)
# El diagrama de error con el IC de la media
ggline(df, x = names(df)[2], y = names(df)[1], 
       add = c("mean_ci", "jitter"))


library(ggpubr)
# El diagrama de error con el IC de la media de dos factores
ggline(df, x = names(df)[2], y = names(df)[1], 
       add = c("mean_ci", "jitter"),
       color = names(df)[3], palette = "jco")+
  ggtitle("Titulo del grafico")