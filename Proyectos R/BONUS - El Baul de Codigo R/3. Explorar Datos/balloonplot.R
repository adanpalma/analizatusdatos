# INSTALAR PAQUETES DE FUNCIONES
#*******************************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("gplots")

# Instala los paquetes sinó los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# CARGAR PAQUETES O CREAR FUNCIONES
#*******************************************************************************
# Carga los paquetes sinó los tienes cargados
lapply(.packages, require, character.only=TRUE)
#-------------------------------------------------------------------------------



#*******************************************************************************

# MODIFICAR NOMBRES DE LAS FILAS y LAS COLUMNAS DE UNA MATRIZ

#*******************************************************************************
#-------------------------------------------------------------------------------


# Construimos la tabla de contingencias:
tablaContingencias <- matrix(c(490, 400,10,100),2,2)
colnames(tablaContingencias) <- c("Fumadores","No Fumadores")
rownames(tablaContingencias) <- c("Cancer","No Cancer")
miTabla <- as.table(tablaContingencias) # pasamos a tipo tabla para que pueda dibujar el plot


balloonplot(miTabla, main ="Tareas de Casa", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)