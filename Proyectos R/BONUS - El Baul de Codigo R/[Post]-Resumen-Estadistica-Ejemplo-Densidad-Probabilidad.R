#********************************************************************
#********************************************************************
#           MATERIAL DESCARGABLE DE CONCEPTOS CLAROS
#********************************************************************
#********************************************************************
# Autor: Jordi Ollé Sánchez
# Fecha: 17/10/2016
# E-mail: jordi@conceptosclaros.com
# Explicación: Este código te permite entender con un ejemplo el concepto
# de función de densidad de probabilidad.
#
# ¿QUÉ HACE ESTE CÓDIGO?
#********************************************************************
# Este código genera 4 muestras aleatorias de una población normal con
# media = mu y standard deviation = sigma. Crea 4 muestras de n1, n2, n3 y n4
# observaciones o muestras. Calcula la longitud del intervalo de las clases
# o BINS del histograma óptimo y dibuja los histogramas. También dibuja la función densidad
# de probabilidad para la muestra de n4 (100000 observaciones)

# INPUTS DISPONIBLES
#********************************************************************
#   mu: media de la población mu <- 168
#   sigma: desviación estándar de la población sigma <- 11
#   n1,n2,n3,n4: número de observaciones para las muestras x1,x2,x3 y x4

# OUTPUTS
#********************************************************************
#   Plots de histogramas en RSTudio


# El código empieza aqui...

#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
#********************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("ggplot2")

# Instala los paquetes sinó los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])


#********************************************************************
# 1.2 CARGAR PAQUETES O CREAR FUNCIONES
#********************************************************************
# Carga los paquetes sinó los tienes cargados
lapply(.packages, require, character.only=TRUE)

#********************************************************************
# 2. VARIABLES DE ENTRADA
#********************************************************************
# Input1: #media de la población
mu <- 168
# Input2: #desviación estándar de la población
sigma <- 11
# Input3: número de observaciones para las muestras n1,n2,n3
n1 = 100
n2 = 1000
n3 = 10000
n4 = 100000

#********************************************************************
# 3. IMPORTAR/CREAR DATOS
#********************************************************************

# Creamos una muestra de 100 observaciones de una distribución normal de media mu y std sigma
x1 <- rnorm(n = n1,mean = mu,sd = sigma)

# Creamos una muestra de 1000 observaciones de una distribución normal de media mu y std sigma
x2 <- rnorm(n = n2,mean = mu,sd = sigma)

# Creamos una muestra de 10000 observaciones de una distribución normal de media mu y std sigma
x3 <- rnorm(n = n3,mean = mu,sd = sigma)

# Creamos una muestra de 100000 observaciones de una distribución normal de media mu y std sigma
x4 <- rnorm(n = n4,mean = mu,sd = sigma)

# Creamos una estructura frame de las 4 muestras
muestra1 <- data.frame(x1 = x1)
muestra2 <- data.frame(x2 = x2)
muestra3 <- data.frame(x3 = x3)
muestra4 <- data.frame(x4 = x4)


#********************************************************************
# 4. METODOLOGÍA
#********************************************************************
# consiste simplemente en calcular el ancho de las clases para las 4 muestras que hemos creado
# nos permite crear el histograma con un número de BINS óptimo. The Freedman-Diaconis rule
# http://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram
binwidthX1 <- 2*IQR(muestra1[,1])*length(muestra1[,1])^(-1/3)
binwidthX2 <- 2*IQR(muestra2[,1])*length(muestra2[,1])^(-1/3)
binwidthX3 <- 2*IQR(muestra3[,1])*length(muestra3[,1])^(-1/3)
binwidthX4 <- 2*IQR(muestra4[,1])*length(muestra4[,1])^(-1/3)


#********************************************************************
# 5. MIRAR RESULTADOS - PLOTS - HISTOGRAMAS
#********************************************************************
# Dibujamos el histograma de densiad de x1
ggplot(muestra1,aes(x=muestra1[,1])) + 
  geom_histogram(aes(y =..density..),binwidth = binwidthX1,colour="black", fill="#e2746a") +
  xlab('Altura [cm]') + ylab("Densidad de frecuencia") +
  ggtitle("Histograma de la muestra de 100 observaciones") +
  theme_minimal() # creamos el histograma y nos lo guardamos en la variable "histograma"

# Dibujamos el histograma de densiad de x2
ggplot(muestra2,aes(x=muestra2[,1])) + 
  geom_histogram(aes(y =..density..),binwidth = binwidthX2,colour="black", fill="#e2746a") +
  xlab('Altura [cm]') + ylab("Densidad de frecuencia") +
  ggtitle("Histograma de la muestra de 1000 observaciones") +
  theme_minimal() # creamos el histograma y nos lo guardamos en la variable "histograma"

# Dibujamos el histograma de densiad de x3
ggplot(muestra3,aes(x=muestra3[,1])) + 
  geom_histogram(aes(y =..density..),binwidth = binwidthX3,colour="black", fill="#e2746a") +
  xlab('Altura [cm]') + ylab("Densidad de frecuencia") +
  ggtitle("Histograma de la muestra de 10000 observaciones") +
  theme_minimal() # creamos el histograma y nos lo guardamos en la variable "histograma"

# Dibujamos el histograma de densiad de x4 (POBLACIÓN)
ggplot(muestra4,aes(x=muestra4[,1])) + 
  geom_histogram(aes(y =..density..),binwidth = binwidthX4,colour="black", fill="#e2746a") +
  xlab('Altura [cm]') + ylab("Densidad de frecuencia") +
  ggtitle("Histograma de la muestra de 100000 observaciones") +
  theme_minimal() # creamos el histograma y nos lo guardamos en la variable "histograma"

# Dibujamos el histograma de densidad de x4 + la distribución de densidad calculada
ggplot(muestra4,aes(x=muestra4[,1])) + 
  geom_histogram(aes(y =..density..,colour="Density"),binwidth = binwidthX4,colour="black", fill="white") +
  xlab('Altura [cm]') + ylab("Densidad de frecuencia") +
  geom_density(alpha=.2, fill="#FF6666")  +
  ggtitle("Histograma de la muestra de 100000 observaciones") +
  theme_minimal() # creamos el histograma y nos lo guardamos en la variable "histograma"