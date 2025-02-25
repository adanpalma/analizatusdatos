#********************************************************************
#********************************************************************
#           MATERIAL DESCARGABLE DE CONCEPTOS CLAROS
#********************************************************************
#********************************************************************
# Autor: Jordi Oll� S�nchez
# Fecha: 27/09/2016
# E-mail: jordi@conceptosclaros.com
# Explicaci�n: Este c�digo te permite ver la estructura de un script
# Tiene en cuenta la estructura siguiente:

# ESTRUCTURA DEL C�DIGO
#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
# 1.2 CARGAR PAQUETES O CREAR FUNCIONES
# 2. IMPORTAR/LEER DATOS
# 3. VARIABLES DE ENTRADA
# 4. METODOLOG�A
# 5. EXPORTAR RESULTADOS

# �QU� HACE ESTE C�DIGO?
#********************************************************************
# Este c�digo te permite leer un archivo csv separado por ;
# Te permite escojer una de las variables num�ricas de la tabla de datos
# Calcula la tabla de frecuencias y dibuja el histograma seg�n un n�mero
# de BINS especificado.

# INPUTS DISPONIBLES
#********************************************************************
#   varNum: la columna de la tabla de datos que quieres leer
#   Nbins: el n�mero de barras de tu histograma

# OUTPUTS
#********************************************************************
#   "Tabla Frecuencias.xlsx": tabla de frecuencias en formato Excel
#   Plots de los histogramas en RStudio


# El c�digo empieza aqui...

#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
#********************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("ggplot2", "plotly", "xlsx","scales")

# Instala los paquetes sin� los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])


#********************************************************************
# 1.2 CARGAR PAQUETES O CREAR FUNCIONES
#********************************************************************
# Carga los paquetes sin� los tienes cargados
lapply(.packages, require, character.only=TRUE)

# Funciones "DIY"
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#********************************************************************
# 2. IMPORTAR/LEER DATOS
#********************************************************************
# Recuerda que debes configurar antes tu working directory ;) y aseg�rate que apunta al fichero Excel
mydata <- read.table("Tabla de Datos Ejemplo.csv", header=TRUE,sep=";")


#********************************************************************
# 3. VARIABLES DE ENTRADA
#********************************************************************
# Input1: #selecciona la variable num�rica que quieras, por ejemplo la n�mero 3: "Edad"
varNum <- 3
# Input2: #selecciona el n�mero de clases que quieres obtener
Nbins <- 20

#********************************************************************
# 4. METODOLOG�A
#********************************************************************
# Preparamos los inputs para ponerlos en los plots
x <- mydata[,varNum] #guardamos la variable x (variable num�rica)
binwidth <- (max(x)-min(x))/(Nbins-1) #el ancho de las clases

# Histograma utilizando ggplot para obtener los datos para dibujar el histograma
histograma <- ggplot(mydata,aes(x=mydata[,varNum])) + 
  geom_histogram(binwidth = binwidth,colour="black", fill="#e2746a") +
  xlab(names(mydata)[varNum]) + ylab("Frecuencia absoluta (n� de repeticiones)") +
  ggtitle(paste("Histograma de la variable ", names(mydata)[varNum],sep = "")) +
  theme_minimal() # creamos el histograma y nos lo guardamos en la variable "histograma"

pg <- ggplot_build(histograma) #guardamos los datos del histograma (la tabla de frecuencias)

# Calculamos la tabla de frecuencias
valoresMinClase <- pg$data[[1]]$xmin
valoresMaxClase <- pg$data[[1]]$xmax
valoresMedioClase <- pg$data[[1]]$x
frecuenciaAbsoluta <- pg$data[[1]]$count
frecuenciaAbsolutaAcumulada <- cumsum(frecuenciaAbsoluta)
frecuenciaRelativa <- pg$data[[1]]$count/sum(pg$data[[1]]$count)
frecuenciaRelativaAcumulada <- cumsum(frecuenciaRelativa)
tablaFrecuencias <- data.frame(Valores.Min.Clase = valoresMinClase,
                               Valores.Max.Clase = valoresMaxClase,
                               Valores.Medio.Clase = valoresMedioClase,
                               Frecuencia.Absoluta = frecuenciaAbsoluta,
                               Frecuencia.Absoluta.Acumulada = frecuenciaAbsolutaAcumulada,
                               Frecuencia.Relativa = frecuenciaRelativa,
                               Frecuencia.Relativa.Acumulada = frecuenciaRelativaAcumulada)

#********************************************************************
# 5. EXPORTAR RESULTADOS
#********************************************************************
# Plot con la funci�n base hist
hist(x) #no especificamos n� de clases, por defecto 10
hist(x,Nbins) #especificamos n� de clases = Nbins

# Histograma utilizando ggplot
histograma + scale_x_continuous(breaks=round(seq(min(pg$data[[1]]$xmin),max(pg$data[[1]]$xmax),by=binwidth),digits = 1)) #corregimos los valores de las clases en el eje horizontal

# Histograma con plotly
ggplotly(histograma + scale_x_continuous(breaks=round(seq(min(pg$data[[1]]$xmin),max(pg$data[[1]]$xmax),by=binwidth),digits = 2))) # convertimos ggplot to plotly

# Exportamos la tabla de frecuencias a un excel
library(xlsx)
write.xlsx(tablaFrecuencias, "Tabla Frecuencias.xlsx")

# ... el c�digo termina aqui.
