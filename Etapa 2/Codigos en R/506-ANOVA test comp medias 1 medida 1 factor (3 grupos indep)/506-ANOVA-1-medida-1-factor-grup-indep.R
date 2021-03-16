# importar datos
datos = iris

# seleccionamos una variable cuantativa y un factor
diffPet_Sep <-iris[c(1)]-iris[c(3)]
names(diffPet_Sep) <- "P_S.Length"

# Seleccionamos la medida y la variable cualitativa
df <- cbind(diffPet_Sep,iris[5])




# 1- Descripción

# Boxplot de dos factores y diagrama de medias

library(ggpubr)
# El diagrama de error con el IC de la media de dos factores
ggline(df, x = names(df)[2], y = names(df)[1], 
       add = c("mean_ci", "jitter"),
       palette = "jco")+
  ggtitle("Titulo del grafico")


library(plotly)
# Boxplot con puntos con plotly
name1 <- names(df)[1]
name2 <- names(df)[2]
plot_ly(y = df[,name1], x = df[,name2], type = "box",name = name1, boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1, "en relación al grupo ",name2),yaxis = list(title = name1),xaxis = list(title = name2))





# 2 Comparar los tres grupos


# Hacemos directamente la ANOVA
resultados.aov <- aov(P_S.Length ~ Species, data = df)
summary.aov(resultados.aov)
      # En la última columna puedes ver el p-valor Pr(>F) y los asteriscos indicando
      # el nivel de significancia: *** Muy significativo


# Miramos la normalidad de los residuos
aov_residuos <- residuals(object = resultados.aov )
shapiro.test(x = aov_residuos )
# Hacemos un qqplot de los residuos
plot(resultados.aov, 2)


# Miramos igualdad de varianzas
leveneTest(P_S.Length ~ Species, data = df)


# --> TEST PARAMÉTRICO - ANOVA
# Si es normal y varianzas iguales --> ANOVA
aov(P_S.Length ~ Species, data = df)


# --> TEST PARAMÉTRICO ANOVA CON CORRECCIÓN DE WELCH (DESIGUALDAD DE VARIANZAS)
# Si es normal y varianzas diferentes --> ANOVA Welch
oneway.test(P_S.Length ~ Species, data = df)


# --> TEST NO PARAMÉTRICO
# Si los residuos NO son normales --> Kruskal Wallis
kruskal.test(P_S.Length ~ Species, data = df)




# PUEDES VER LA COMPARACIÓN POR PAREJAS DE GRUPOS CON PAIRWISE
# La primera la numérica y la segunda la categórica
pairwise.t.test(df[,1], df[,2],p.adjust.method = "BH")
# o el PairWise Tukey
TukeyHSD(resultados.aov)

# PUEDES VER LA COMPARACIÓN POR PAREJAS DE GRUPOS CON PAIRWISE
# La primera la numérica y la segunda la categórica
pairwise.t.test(df[,1], df[,2],p.adjust.method = "BH")

# NORMALIDAD = NO >> UTILIZA KRUSKALL-WALLIS (NO PARAMÉTRICO)
# La primera la numérica y la segunda la categórica
pairwise.wilcox.test(df[,1], df[,2],p.adjust.method = "BH")



