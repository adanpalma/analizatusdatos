# INSTALAR PAQUETES DE FUNCIONES
#*******************************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("ggpubr")

# Instala los paquetes sinó los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# CARGAR PAQUETES O CREAR FUNCIONES
#*******************************************************************************
# Carga los paquetes sinó los tienes cargados
lapply(.packages, require, character.only=TRUE)
#-------------------------------------------------------------------------------


#*******************************************************************************

# SCATTER PLOT CON LINEA DE REGRESION

#*******************************************************************************
#-------------------------------------------------------------------------------

# Voy a utilizar la base de datos cars que está en R
misDatos <- mtcars

# Datos con la primera columna antes y la segunda después del tratamiento o modificación del proceso
varEstudio <- misDatos[,c("mpg","wt")]
varEstudio[,1] <- 1/varEstudio[,1] # estoy calculando el consumo
names(varEstudio) <- c("consumo","peso")

df <- varEstudio


# SCATTER 2D
#*******************************************************************************
df <- varEstudio
name1 <- names(df)[1]
name2 <- names(df)[2]
ggscatter(varEstudio, x = name1, y = name2, 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Consumo  -Galones / Milla", ylab = "Peso  (1000 lbs)")

