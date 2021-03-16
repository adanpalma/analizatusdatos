# importar datos
datos = ToothGrowth

# Seleccionamos la medida y dos factores
df <- datos
df[3] <- as.factor(df[,3])

table(df$supp,df$dose)

# 1- Descripción

# Boxplot de dos factores y diagrama de medias

library(ggpubr)
# El diagrama de error con el IC de la media de dos factores
ggline(df, x = names(df)[2], y = names(df)[1], 
       add = c("mean_ci", "jitter"),
       color = names(df)[3], palette = "jco")+
  ggtitle("Titulo del grafico")

library(ggplot2)
# boxplot de dos factores con ggplot2
ggplot(df, aes(x=df[,2], y=df[,1], fill=df[,3])) +
  geom_boxplot() + 
  ggtitle("Titulo del grafico") +
  xlab("Titulo Var Cuant") + ylab("Titulo Factor 1")+ labs(fill = "Titulo Factor 2")




# 2 Comparar los tres factores
# Comparar dos grupos:
# En la práctica:
# .	Exploramos los grupos: boxplots de dos niveles y la tabla de contingencias.
# .	Calculamos ANOVA directamente y vemos si cumple con las restricciones aov()
#   . Miramos si las distribuciones de todos los grupos son normales
#   · Miramos si las varianzas son iguales con el test de Levene
# · Si cumple con las 2 restricciones del método:
#   · Puedes ver qué parejas de grupos son diferentes con el método Tukey por parejas
# · Si cumple normalidad pero NO igualdad varianzas >> Welch one-way oneway.test
# · Si no cumple con las restricciones de normalidad >> KRUSKALL WALLIS (test NO paramétrico) ()

#   CALCULAMOS ANOVA ADITIVO y- MULTIPLICATIVO
#*******************************************************************************
# Para calcular ANOVA utiliza aov() y summary.aov() para ver los resultados
# Calculamos ANOVA con aov()
res.aov1 <- aov(len ~ supp + dose, data = df)
summary(res.aov1)
# En la última columna puedes ver el p-valor Pr(>F) y los asteriscos indicando
# el nivel de significancia: *** Muy significativo

# Si crees que las variables categóricas NO son independientes puedes analizar
# el efecto conjunto entre las dos categorías. Los dos factores.
res.aov2 <- aov(len ~ supp * dose, data = df)
summary(res.aov2)
# te calcula una fila más con la interacción de los factores.

#   MIRAMOS SI LOS RESIDUOSO SON NORMALES
#*******************************************************************************

#   ANOVA1 --> SIN INTERACCIÓN OK!!!
#********************************************
# TEST DE NORMALIDAD >> miramos si los residuos del test son normales
# Copiamos los residuos
aov_residuos <- residuals(object = res.aov1 )
# Shapiro-Wilk test de normalidad
shapiro.test(x = aov_residuos )
# Hacemos un qqplot de los residuos
plot(res.aov1, 2)
# Homogeneidad de los residuos
plot(res.aov1, 1)


#   ANOVA2 --> CON INTERACCIÓN OK!!!
#********************************************
# TEST DE NORMALIDAD >> miramos si los residuos del test son normales
# Copiamos los residuos
aov_residuos <- residuals(object = res.aov2 )
# Shapiro-Wilk test de normalidad
shapiro.test(x = aov_residuos )
# Hacemos un qqplot de los residuos
plot(res.aov2, 2)
# Homogeneidad de los residuos
plot(res.aov2, 1)



#   IGUALDAD DE VARIANZAS OK!!!
#*******************************************
# TEST DE IGUALDAD DE VARIANZAS >> test de Levene
leveneTest( len ~ supp, df)

leveneTest( len ~ dose, df)
# En la última columna puedes ver el p-valor Pr(>F) y los asteriscos indicando
# el nivel de significancia: *** Muy significativo


#   PAIRWISE COMPARISON
#*******************************************************************************
library(multcomp)
# PARA EL MODELO SIN INTERACCIÓN
summary(glht(res.aov1, linfct = mcp(dose = "Tukey")))

# PUEDES VER LA COMPARACIÓN POR PAREJAS DE GRUPOS CON PAIRWISE
# La primera la numérica y la segunda la categórica
pairwise.t.test(df[,1], df[,2],p.adjust.method = "BH")

pairwise.t.test(df[,1], df[,3],p.adjust.method = "BH")

#   TEST NO PARAMÉTRICO  --> Scheirer Ray Hare test
#*******************************************************************************
# ES EL TWO-SIDED ANOVA TEST NO PARAMÉTRICO
library(rcompanion)
scheirerRayHare(len ~ supp + dose,
                data=df)

# fíjate que los p-valors son mucho más elevados que con la ANOVA --> Más conservadores


# BONUS: NO BALANCEADO
#*******************************
Anova(res.aov2,type = "III")