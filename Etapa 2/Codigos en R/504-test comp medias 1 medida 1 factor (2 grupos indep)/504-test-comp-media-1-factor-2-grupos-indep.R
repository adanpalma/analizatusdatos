# importar datos
datos = iris

# seleccionamos una variable cuantativa y un factor
diffPet_Sep <-iris[c(1)]-iris[c(3)]
names(diffPet_Sep) <- "P_S.Length"
dfTOT <- cbind(diffPet_Sep,iris[5])


# Seleccionamos solo los de setosa y versicolor
rowSel <- dfTOT[,2]=="setosa"|dfTOT[,2]=="versicolor"
df <- dfTOT[rowSel,]


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


# 2 Comparar la media con un valor


# Test de normalidad para los dos grupos

# para setosa
dvector <- df[df[,2]=="setosa",1]
shapiro.test(dvector)
# para versicolor
dvector <- df[df[,2]=="versicolor",1]
shapiro.test(dvector)


# Test de Igualdad de Varianzas - Levene
leveneTest(P_S.Length ~ Species, data = df)



# --> TEST PARAMÉTRICO
# Si es normal y varianzas iguales --> T-test de dos muestras independientes
t.test(P_S.Length ~ Species, data = df, var.equal = TRUE)


# --> TEST PARAMÉTRICO CON CORRECCIÓN DE WELCH (DESIGUALDAD DE VARIANZAS)
# Si es normal y varianzas diferentes --> T-test Welch de dos muestras independientes
t.test(P_S.Length ~ Species, data = df, var.equal = FALSE)



# --> TEST NO PARAMÉTRICO
# Si NO es normal --> Wilcoxon de dos muestras independientes
wilcox.test(P_S.Length ~ Species, data = df)



