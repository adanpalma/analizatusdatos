#*******************************************************************************

# PROBLEMA N�MERO 4 - AN�LISIS DE CORRELACI�N

#*******************************************************************************
#-------------------------------------------------------------------------------


# INSTALAR PAQUETES DE FUNCIONES
#*******************************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("ggpubr","ggplot2","plotly","readxl")

# Instala los paquetes sin� los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# CARGAR PAQUETES O CREAR FUNCIONES
#*******************************************************************************
# Carga los paquetes sin� los tienes cargados
lapply(.packages, require, character.only=TRUE)
#-------------------------------------------------------------------------------


#*******************************************************************************
# PASO 0 - LEER TUS DATOS
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

# Utiliza este comando para leer archivos con TABULACI�N como separador
# misDatos <- read.delim(file.choose(),header = TRUE)

# Utiliza este comando para leer archivos con separador
# misDatos <- read.csv(file.choose(),header = TRUE,sep = ';')

# Voy a utilizar la base de datos cars que est� en R
misDatos <- mtcars

# Nota: Asegurate que tienes los nombres de las variables puestas




#*******************************************************************************
# PASO 1 - ENTENDER TUS DATOS - DEFINIR EL OBJETIVO
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

# 1.1. �De d�nde provienen la base de datos?

# Son datos de modelos de coches. Cada coche tiene el consumo, el peso y otras
# caracter�sticas como la cilindrada etc

# 1.2. �Qu� variables tienes?

# Vamos a utilziar dos variables num�ricas. Peso y consumo

# 1.3. �Qu� objetivo tienes para este estudio?
# La PREGUNTA: si hay relaci�n entre tareas del hogar y el responsable de 
# hacelas


# La PREGUNTA: Identificar relaci�n lineal entre el consumo 1/mpg (Galones por milla) y
# y el peso del coche. Deber�amos ver una relaci�n lineal creciente.
# Cuanto m�s pesa el coche m�s consume. �Lo comprobamos?




str(misDatos)
variables <- split(names(misDatos),sapply(misDatos, function(x) paste(class(x), collapse=" ")))

# La variable edad es num�rica
varNumericas <- c(variables$numeric)

# Las variables categ�ricas son los factores.
varCategoricas <- variables$integer

# Datos con la primera columna antes y la segunda despu�s del tratamiento o modificaci�n del proceso
varEstudio <- misDatos[,c("mpg","wt")]
varEstudio[,1] <- 1/varEstudio[,1] # estoy calculando el consumo
names(varEstudio) <- c("consumo","peso")



#*******************************************************************************
# PASO 2: LA EXPLORACI�N (Exel + Ballon plot)
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************


# Comparar dos grupos dependientes:

# En la pr�ctica:
# Exploraci�n con scatter de las dos variables y la linea de correlaci�n
# � Si se intuye relaciones de NO linealidad no apliques la correlaci�n. No te servir�

# Calcular la correlaci�n siguiendo:
# .	Cuando las dos variables son normales >> Correlaci�n de Pearson
# .	Cuando alguna o las dos NO son normales >> Correlaci�n de Kendall o Spearman


# SCATTER 2D
#*******************************************************************************
df <- varEstudio
name1 <- names(df)[1]
name2 <- names(df)[2]
ggscatter(varEstudio, x = name1, y = name2, 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Consumo  -Galones / Milla", ylab = "Peso  (1000 lbs)")




#*******************************************************************************
# PASO 3: EL AN�LISIS
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

# RESTRICCIONES NORMALIDAD >> Las dos variables son Normales p>0.05
#*******************************************************************************
shapiro.test(varEstudio[,1])
# Mpg es normal
shapiro.test(varEstudio[,2])
# Wt es normal

# TEST DE HIP�TESIS: Correlaci�n de Pearson
#*******************************************************************************
# La primera variable es antes y la segunda despu�s (no importa el orden)
# T-test con grupos dependientes paired = TRUE
cor.test(varEstudio[,1], varEstudio[,2], 
         method = "pearson")
# p-valor < 0.05 hay correlaci�n significativa


# Si no es normal alguna de las variables trabajamos con Kendal on Spearman
# KENDALL:
# cor.test(varEstudio[,1], varEstudio[,2], 
#         method = "kendall")
# SPEARMAN:
# cor.test(varEstudio[,1], varEstudio[,2], 
#         method = "spearman")


#*******************************************************************************
# PASO 4: CONCLUSI�N
#*******************************************************************************

# En un word....
