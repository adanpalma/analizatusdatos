#*******************************************************************************

# PROBLEMA NÚMERO 4 - ANÁLISIS DE CORRELACIÓN

#*******************************************************************************
#-------------------------------------------------------------------------------


# INSTALAR PAQUETES DE FUNCIONES
#*******************************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("ggpubr","ggplot2","plotly","readxl")

# Instala los paquetes sinó los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# CARGAR PAQUETES O CREAR FUNCIONES
#*******************************************************************************
# Carga los paquetes sinó los tienes cargados
lapply(.packages, require, character.only=TRUE)
#-------------------------------------------------------------------------------


#*******************************************************************************
# PASO 0 - LEER TUS DATOS
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

# Utiliza este comando para leer archivos con TABULACIÓN como separador
# misDatos <- read.delim(file.choose(),header = TRUE)

# Utiliza este comando para leer archivos con separador
# misDatos <- read.csv(file.choose(),header = TRUE,sep = ';')

# Voy a utilizar la base de datos cars que está en R
misDatos <- mtcars

# Nota: Asegurate que tienes los nombres de las variables puestas




#*******************************************************************************
# PASO 1 - ENTENDER TUS DATOS - DEFINIR EL OBJETIVO
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

# 1.1. ¿De dónde provienen la base de datos?

# Son datos de modelos de coches. Cada coche tiene el consumo, el peso y otras
# características como la cilindrada etc

# 1.2. ¿Qué variables tienes?

# Vamos a utilziar dos variables numéricas. Peso y consumo

# 1.3. ¿Qué objetivo tienes para este estudio?
# La PREGUNTA: si hay relación entre tareas del hogar y el responsable de 
# hacelas


# La PREGUNTA: Identificar relación lineal entre el consumo 1/mpg (Galones por milla) y
# y el peso del coche. Deberíamos ver una relación lineal creciente.
# Cuanto más pesa el coche más consume. ¿Lo comprobamos?




str(misDatos)
variables <- split(names(misDatos),sapply(misDatos, function(x) paste(class(x), collapse=" ")))

# La variable edad es numérica
varNumericas <- c(variables$numeric)

# Las variables categóricas son los factores.
varCategoricas <- variables$integer

# Datos con la primera columna antes y la segunda después del tratamiento o modificación del proceso
varEstudio <- misDatos[,c("mpg","wt")]
varEstudio[,1] <- 1/varEstudio[,1] # estoy calculando el consumo
names(varEstudio) <- c("consumo","peso")



#*******************************************************************************
# PASO 2: LA EXPLORACIÓN (Exel + Ballon plot)
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************


# Comparar dos grupos dependientes:

# En la práctica:
# Exploración con scatter de las dos variables y la linea de correlación
# · Si se intuye relaciones de NO linealidad no apliques la correlación. No te servirá

# Calcular la correlación siguiendo:
# .	Cuando las dos variables son normales >> Correlación de Pearson
# .	Cuando alguna o las dos NO son normales >> Correlación de Kendall o Spearman


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
# PASO 3: EL ANÁLISIS
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

# RESTRICCIONES NORMALIDAD >> Las dos variables son Normales p>0.05
#*******************************************************************************
shapiro.test(varEstudio[,1])
# Mpg es normal
shapiro.test(varEstudio[,2])
# Wt es normal

# TEST DE HIPÓTESIS: Correlación de Pearson
#*******************************************************************************
# La primera variable es antes y la segunda después (no importa el orden)
# T-test con grupos dependientes paired = TRUE
cor.test(varEstudio[,1], varEstudio[,2], 
         method = "pearson")
# p-valor < 0.05 hay correlación significativa


# Si no es normal alguna de las variables trabajamos con Kendal on Spearman
# KENDALL:
# cor.test(varEstudio[,1], varEstudio[,2], 
#         method = "kendall")
# SPEARMAN:
# cor.test(varEstudio[,1], varEstudio[,2], 
#         method = "spearman")


#*******************************************************************************
# PASO 4: CONCLUSIÓN
#*******************************************************************************

# En un word....
