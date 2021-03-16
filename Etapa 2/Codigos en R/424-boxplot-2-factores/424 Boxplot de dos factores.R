# importar datos
datos = mtcars

# leemos un data frame con la primera columna una variable cuantitativa
# la segunda variable el primer factor
# la tercera variable el segundo factor
df = datos[c(1,8,9)]
df[,2] = as.factor(df[,2])
df[,3] = as.factor(df[,3])


library(ggplot2)
# boxplot de dos factores con ggplot2
ggplot(df, aes(x=df[,2], y=df[,1], fill=df[,3])) +
  geom_boxplot() + 
  ggtitle("Titulo del grafico") +
  xlab("Titulo Factor 1") + ylab("Titulo Var Medida")+ labs(fill = "Titulo Factor 2")


library(ggplot2)
# boxplot con los puntos
ggplot(df, aes(x=df[,2], y=df[,1], fill=df[,3])) +
  geom_boxplot() + 
  ggtitle("Titulo del grafico") +
  xlab("Titulo Var Cuant") + ylab("Titulo Factor 1") +
  labs(fill = "Titulo Factor 2") + 
  geom_dotplot(binaxis='y', stackdir='center',
                         position=position_dodge(1))
