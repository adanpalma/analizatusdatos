#********************************************************************
#********************************************************************
#           MATERIAL DESCARGABLE DE CONCEPTOS CLAROS
#********************************************************************
#********************************************************************
# Autor: Jordi Oll� S�nchez
# Fecha: 08/02/2016
# E-mail: jordi@conceptosclaros.com
# Explicaci�n: Este c�dgio permite ver las secuencias de c�digo m�s utilizadas
# en la pr�ctica
#
# ESTRUCTURA DEL C�DIGO
#********************************************************************
# 1.VECTORES, MATRICES Y COMANDOS B�SICOS
# 2. ITERACI�N Y CONDICIONAL
# 3. ITERACI�N Y GR�FICO

# �QU� HACE ESTE C�DIGO?
#********************************************************************
# Define variables tipo valor, tipo vectores, tipo matriz. Vas a ver
# c�mo puedes calcular la longitud del vector, el n�mero de filas
# el n�mero de columnas de una matriz. VAs a aprender a acceder a un
# vector y una matriz. Do ejemplos de aplicaci�n con iteraci�n y condicional.


#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
#********************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("ggplot2", "plotly", "xlsx","scales","readr")


# Instala los paquetes sin� los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])


#********************************************************************
# 1.2 CARGAR PAQUETES O CREAR FUNCIONES
#********************************************************************
# Carga los paquetes sin� los tienes cargados
lapply(.packages, require, character.only=TRUE)

# El c�digo empieza aqui...

#********************************************************************
# 1. VECTORES, MATRICES Y COMANDOS B�SICOS
#    Calcula los valores divisibles por 3 del 1 al 130
#********************************************************************

# Asignar un valor a una variables llamada "a"
a <- 10
a = 10
# Definir una secuencia de 0 a 2 con pasos de 0.01
z <- seq(0,5,1)
# Acceder a la segunda posici�n del vector
z[2]
# La suma de todos los componentes del vector
sum(z)
# La media del vector
mean(z)
var(z)
# Dibujar los puntos del vector
plot(z)
# Dibujar la linea de un vector
plot(z,type='l')
# Calcular la longitud del vector
length(z)
# Definir un vector a partir de los valores dados
m <- c(0.3,9,4.5,9)
# Definir una matriz a partir de los valores de la matrix
# nrow indica el n�mero de filas y ncol el n�mero de columnas
A <- matrix(data=c(3,-2,4,9),nrow=2,ncol=2)
# Calcular el n�mero de filas
nFilas <- nrow(A)
# Calcular el n�mero de columnas
nCol <- ncol(A)
# Acceder al valor de la matriz de la fila 2 y la columna 1
A[2,1]

#********************************************************************
# 2. ITERACI�N Y CONDICIONAL
#    Calcula los valores divisibles por 3 del 1 al 130
#********************************************************************

# Definir un vector vacio
s <- vector()
# Definir un contador que estar� dentro del condicional
j <- 0
# Crear una iteraci�n for con un contador "i" que va de 1 a 130
for(i in 1:130) {
  # Condicional que te dice si la i es divisible por 2 y lo guarda en un vector s
  if (i%%3 == 0) {
    print(i)
    j = j+1
    s[j] = i
  }
  else{
  }
}


# Calcular la longitud del vector de los valores divisibles por 3
N = length(s);

#********************************************************************
# 3. ITERACI�N Y GR�FICO
#    Calcula el �rea de un c�rculo de radio  0 a 25 y lo dibuja
#********************************************************************
# Calcula el �rea de un ciruclo de radio r
# El radio r va desde 0 a 25


while( readline("Desea Calcular Area del Circulo S o N ?") !=  "N")
  {
    primerpunto <-  readline("Primer Punto de los radios")
    segundopunto <-  readline("Segundo Punto de los radios")

print(paste("Calculando area para radios desde ", primerpunto, " a ", segundopunto))

   
r <- seq(primerpunto,segundopunto,0.02)
A <- vector()
for (i in primerpunto:length(r)) {
  A[i] <- pi*r[i]^2
}
# Dibuja el area del c�rculo en funci�n del radio
plot(r,A,type='l')

}

###############
# code snippets de Adan
##############

medias = seq()
for (i in 1:1000) {
     medias[i] <-  mean(rnorm(1000,mean = 70,sd =rep(c(5),1000)))
  
}

hist(medias)
summary(medias)


errortipico = sd(medias) /sqrt(length(medias))
mediainf = mean(medias) - 1.96*errortipico
mediasup = mean(medias) + 1.96*errortipico

print(paste("Esta es la media de las medas " , mean(medias)))
print(paste("Esta es la sd de las medas " , sd(medias)))
print(paste("Error Tipo o DS Poblacion ", errortipico ))
print(paste("Rango Medias 95% ", mediainf, "   ", mediasup))




