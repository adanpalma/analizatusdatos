# importar datos
datos <- mtcars

# seleccionamos las variables cuantitativas que queramos (mas de dos)
df <- datos[c(1,3,4,5,6,7)]
dffactor <- datos[c(9)]
dffactor[,1]<-as.factor(dffactor[,1])


# matrixplot con la funci�n base y el data frame de + de dos variables
pairs(df,main = "T�tulo del Gr�fico",
      pch=19,col=c("green3","red")[dffactor[,1]],
      lower.panel = NULL)
par(xpd=TRUE)
legend("bottomleft",as.vector(unique(dffactor[,1])),fill=c("green3","red"))




# M�s opciones - Panel de correlaci�n
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
  points(x,y, pch = 19, col = c("green3","red")[dffactor[,1]])
}
# Crear el matrixplot
pairs(df, 
      lower.panel = panel.cor,
      upper.panel = upper.panel)


# Seguna opci�n para costumizar el matrixplot
upper.panel<-function(x, y){
  points(x,y, pch=19, col = c("green3","red")[dffactor[,1]])
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}
# crear el matrixplot
pairs(df, lower.panel = NULL, 
      upper.panel = upper.panel)
par(xpd=TRUE)
legend("bottomleft",as.vector(unique(dffactor[,1])),fill=c("green3","red"))



library(psych)
# Matrixplot con el paquete psych
pairs.panels(df, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)