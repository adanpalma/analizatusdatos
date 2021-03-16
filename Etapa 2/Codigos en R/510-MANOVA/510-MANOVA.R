# importar datos
datos = iris

# Seleccionamos la medida y dos factores
df <- datos[c(1,3,5)]




# Histograma de densidad para comparar grupos
name1 <- names(df)[1]
name2 <- names(df)[3]
p1<- ggplot(df, aes(df[,name1], fill = df[,name2])) +
  geom_density(alpha = 0.2) +
  xlab("Rango de clases") + ylab("Densidad") +
  theme_minimal() +
  ggtitle(paste("Histograma de Densidad", name1,"comparando por grupos de ",name2)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=""))

# Boxplot de dos grupos
name1 <- names(df)[1]
name2 <- names(df)[3]
p2<- plot_ly(y = df[,name1], x = df[,name2], type = "box",name = name1, boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1, "en relación al grupo ",name2),yaxis = list(title = name1),xaxis = list(title = name2))

subplot(p1,p2)%>%layout(title = paste("Boxplot y Histograma de Densidad de",name1, "en relación al grupo ",name2))


# Histograma de densidad para comparar grupos
name1 <- names(df)[2]
name2 <- names(df)[3]
ggplot(df, aes(df[,name1], fill = df[,name2])) +
  geom_density(alpha = 0.2) +
  xlab("Rango de clases") + ylab("Densidad") +
  theme_minimal() +
  ggtitle(paste("Histograma de Densidad", name1,"comparando por grupos de ",name2)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=""))

# Boxplot de dos grupos
name1 <- names(df)[2]
name2 <- names(df)[3]
p2<- plot_ly(y = df[,name1], x = df[,name2], type = "box",name = name1, boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1, "en relación al grupo ",name2),yaxis = list(title = name1),xaxis = list(title = name2))

subplot(p1,p2)%>%layout(title = paste("Boxplot y Histograma de Densidad de",name1, "en relación al grupo ",name2))

# PASO 2.2: MIRAMOS SI CUMPLE RESTRICCIONES
#*******************************************************************************

#***** VARIABLE SEPAL.LENGTH *****

# TEST DE NORMALIDAD >> miramos si los residuos del test son normales
# Para calcular ANOVA utiliza aov() y summary.aov() para ver los resultados
# Calculamos ANOVA con aov()
resultados.aov <- aov(Sepal.Length ~ Species, df)
summary.aov(resultados.aov)
# Copiamos los residuos
aov_residuos <- residuals(object = resultados.aov )
# Shapiro-Wilk test de normalidad
shapiro.test(x = aov_residuos )
# Hacemos un qqplot de los residuos
plot(resultados.aov, 2)

# TEST DE IGUALDAD DE VARIANZAS >> test de Levene
leveneTest(Sepal.Length ~ Species, df)
# En la última columna puedes ver el p-valor Pr(>F) y los asteriscos indicando
# el nivel de significancia: *** Muy significativo


#***** VARIABLE PETAL.LENGTH *****

# TEST DE NORMALIDAD >> miramos si los residuos del test son normales
# Para calcular ANOVA utiliza aov() y summary.aov() para ver los resultados
# Calculamos ANOVA con aov()
resultados.aov <- aov(Petal.Length ~ Species, df)
summary.aov(resultados.aov)
# Copiamos los residuos
aov_residuos <- residuals(object = resultados.aov )
# Shapiro-Wilk test de normalidad
shapiro.test(x = aov_residuos )
# Hacemos un qqplot de los residuos
plot(resultados.aov, 2)

# TEST DE IGUALDAD DE VARIANZAS >> test de Levene
leveneTest(Petal.Length ~ Species, df)
# En la última columna puedes ver el p-valor Pr(>F) y los asteriscos indicando
# el nivel de significancia: *** Muy significativo

# PASO 2.3: DECIDIMOS EL TEST
#*******************************************************************************
# Los resultados demuestran que no cumplen con las restricciones pero
# par hacer el ejemplo continuamos con el MANOVA

# Ponemos una matrix con las variables numéricas a la izquiera y SPecies a la derecha
res.man <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = df)
summary(res.man)

# Vamos a ver la ANOVA por separado:
summary.aov(res.man)

