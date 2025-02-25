# INSTALAR PAQUETES DE FUNCIONES
#*******************************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("plotly")

# Instala los paquetes sin� los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# CARGAR PAQUETES O CREAR FUNCIONES
#*******************************************************************************
# Carga los paquetes sin� los tienes cargados
lapply(.packages, require, character.only=TRUE)
#-------------------------------------------------------------------------------



#*******************************************************************************

# HISTOGRAMA DE DENSIDAD POR GRUPOS

#*******************************************************************************
#-------------------------------------------------------------------------------

misDatos <- iris


# Selecciono la variable 1 y 5. La primera num�rica y la segunda categ�rica
df <- misDatos[,c(1,5)]

# Histograma de densidad para comparar grupos
#-------------------------------------------------------------------------------
name1 <- names(df)[1]
name2 <- names(df)[2]
p1<- plot_ly(y = df[,name1], x = df[,name2], type = "box",name = name1, boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1, "en relaci�n al grupo ",name2),yaxis = list(title = name1),xaxis = list(title = name2))

p1

# El gr�fico se guarda en p1