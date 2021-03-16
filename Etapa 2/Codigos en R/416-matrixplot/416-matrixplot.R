# importar datos
datos <- mtcars

# seleccionamos las variables cuantitativas que queramos (mas de dos)
df <- datos[c(1,3,4,5,6,7)]


# matrixplot con la función base y el data frame de + de dos variables
pairs(df)

pairs(df,pch=19)

pairs(df,pch=19,lower.panel = NULL)


# Más opciones - Panel de correlación
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}
# Crear el matrixplot
pairs(df, 
      lower.panel = panel.cor,
      upper.panel = upper.panel)


# Seguna opción para costumizar el matrixplot
upper.panel<-function(x, y){
  points(x,y, pch=19)
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}
# crear el matrixplot
pairs(df, lower.panel = NULL, 
      upper.panel = upper.panel)




library(psych)
# Matrixplot con el paquete psych
pairs.panels(df, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)