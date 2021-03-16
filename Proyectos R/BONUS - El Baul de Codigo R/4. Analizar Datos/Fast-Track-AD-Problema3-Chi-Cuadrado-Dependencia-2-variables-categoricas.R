#*******************************************************************************

# PROBLEMA NÚMERO 3 - TEST DE INDEPENDENCIA DE CHI CUADRADO

#*******************************************************************************
#-------------------------------------------------------------------------------



# INSTALAR PAQUETES DE FUNCIONES
#*******************************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("gplots","corrplot")

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


# PASO 0.1 - LEE TUS DATOS (IMPORTAR)
#-------------------------------------------------------------------------------
# Utiliza este comando para leer archivos con TABULACIÓN como separador
misDatos <- read.delim(file.choose(),header = TRUE,row.names = 1)
# Convierto los datos en una tabla
miTabla <- as.table(as.matrix(misDatos))

# Utiliza este comando para leer archivos con separador
# misDatos <- read.csv(file.choose(),header = TRUE,sep = ';')

# Traduzco los grupos: 

rownames(miTabla) <- c("Lavar la Ropa", "Hacer la comida","Hacer la cena","Hacer el desayuno",
"Ordenar y limpiar la casa","Lavar los platos","Las compras",
"Gestiones Administrativas","Conducir","Llevar las finanzas",
"Seguros","Reparaciones","Vacaciones")
colnames(miTabla) <- c("Chica","Alternando","Chico","Conjuntamente")

# Nota: Asegurate que tienes los nombres de las variables puestas


#*******************************************************************************
# PASO 1 - ENTENDER TUS DATOS - DEFINIR EL OBJETIVO
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

# 1.1. ¿De dónde provienen la base de datos?

# Son datos de obtenidos de la red. Y es de un ejemplo de familias.
# Y las reparticiones de tareas entre los mimebores de la preja. El
# chico o la chica.
# La tabla que has leído es directamente la tabla de contingencias

# 1.2. ¿Qué variables tienes?

# Dos variables categóricas politómicas.

# 1.3. ¿Qué objetivo tienes para este estudio?
# La PREGUNTA: si hay relación entre tareas del hogar y el responsable de 
# hacelas


# La PREGUNTA: inferir si hay dependencia entre filas (tipo de trabajo de casa) y
# columnas (quén hace estas tareas)


#*******************************************************************************
# PASO 2: EXPLORAMOS LA TABLA (Exel + Ballon plot)
#*******************************************************************************

# PASO 2.1: EXPLORAR LAS CELDAS
#*******************************************************************************
# Un tipo de mapa de circulos. balloonplot
balloonplot(t(miTabla), main ="Tareas de Casa", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)


#*******************************************************************************
# PASO 3: EL ANÁLISIS
#*******************************************************************************


# En este caso comparamos dos categorías: podemos utilizar Chi cuadrado, Chi cuadrado
# correción de Yates y el F exacto de Fisher

# En la práctica:
# . Si el 80% de las celdas de la tabla tienen más de 5 >> Chi cuadrado
# .	Si no cumple con la primera restricción >> Chi Cuadrado con correción de Yates

# Los pasos para este tipo de Análisis son:
# PASO 2.1 EXPLORAR LAS CELDAS
# PASO 2.1 DECIDIR EL TEST
# PASO 2.2 ANALIZA LA IMPORTANCIA DE CADA CELDA

# PASO 2.2 DECIDIR EL TEST
#*******************************************************************************
# El 76% de las celdas tienen valores mayores que 5. Podríamos aplicar Chi cuadrado
# justito. Aplicamos también la correción de Yates y vemos lal diferencia.
# 
# Test de Chi Cuadrado
chisq <- chisq.test(misDatos,correct = FALSE)
chisq
# Test de Chi Cuadrado con la correción de Yates
chisqYates <- chisq.test(misDatos,correct = TRUE)
chisqYates

# Si tubieramos dos 2x2 puedes utilizar el test Exacto de Fisher
# fisher.test(latabladeContingencia)

# PASO 2.3 ANALIZA LA IMPORTANCIA DE CADA CELDA
#*******************************************************************************
corrplot(chisqYates$residuals, is.cor = FALSE)
# Contribucion en tanto por ciento (%)
contrib <- 100*chisqYates$residuals^2/chisqYates$statistic
round(contrib, 3)


#*******************************************************************************
# PASO 4: APLICA LA PLANTILLA DE CONTRASTE DE HIPÓTESIS
#*******************************************************************************

# En un word....
