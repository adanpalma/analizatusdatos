# INSTALAR PAQUETES DE FUNCIONES
#*******************************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("plotly")

# Instala los paquetes sinó los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# CARGAR PAQUETES O CREAR FUNCIONES
#*******************************************************************************
# Carga los paquetes sinó los tienes cargados
lapply(.packages, require, character.only=TRUE)
#-------------------------------------------------------------------------------


#*******************************************************************************

# HISTOGRAMA
#*******************************************************************************
#-------------------------------------------------------------------------------

misDatos <- iris

df <- misDatos[1] # Selecciono la primera variable

name1 <- names(df)
# Boxplot
p1<-plot_ly(y = df[,name1], type = "box",name = name1,boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1),yaxis = list(title = name1))

p1