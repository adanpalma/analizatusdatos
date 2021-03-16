# importar datos
datos <- mtcars

# seleccionamos las variables cuantitativas que queramos (mas de dos)
df <- datos[c(1,3,4,5,6,7)]



library(psych)
# Matrixplot con el paquete psych
pairs.panels(df, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)




library("PerformanceAnalytics")
# Matrixplot mixto con el paquete Performance Analytics
chart.Correlation(df, histogram=TRUE, pch=19)