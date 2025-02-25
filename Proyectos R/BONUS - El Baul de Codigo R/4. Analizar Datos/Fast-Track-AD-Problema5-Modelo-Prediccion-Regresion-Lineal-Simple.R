#*******************************************************************************

# PROBLEMA N�MERO 5 - AN�LISIS REGRESI�N LINEAL SIMPLE

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

# La PREGUNTA: Conseguir un modelo lineal que nos permita calcular el consumo del
# coche a partir del leso del coche.


str(misDatos)
variables <- split(names(misDatos),sapply(misDatos, function(x) paste(class(x), collapse=" ")))

# La variable edad es num�rica
varNumericas <- c(variables$numeric)

# Las variables categ�ricas son los factores.
varCategoricas <- variables$integer

# Datos con la primera columna antes y la segunda despu�s del tratamiento o modificaci�n del proceso
varEstudio <- misDatos[,c("wt","mpg")]
varEstudio[,2] <- 1/varEstudio[,2] # estoy calculando el consumo
names(varEstudio) <- c("peso","consumo")
# La variable x es el peso
# La variable y es el consumo




#*******************************************************************************
# PASO 2: LA EXPLORACI�N
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

# Regresi�n Lineal Simple:

# En la pr�ctica:
# No hay consideraciones para utilizar la regresi�n. Si en cuanto al n�emro de datos
# Cuanto m�s datos tengas m�s verz ser� y contrastado. 

# Calcular la correlaci�n siguiendo:
# .	Crear un scatter 2D con la tecta de regresi�n y el valor de la correlaci�n
# � Si la relaci�n es no lineal la recta de regresi�n no tiene mucho sentido
# .	Calcular la recta de regresi�n
# � Mirar si los residuos son normales y no siguen ning�n patr�n estra�o.
# � Mirar si los coeficientes son significativos, es decir son diferentes de 0



# SCATTER 2D + RECTA DE REGRESI�N
#*******************************************************************************
df <- varEstudio
name1 <- names(df)[1]
name2 <- names(df)[2]
ggscatter(varEstudio, x = name1, y = name2, 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Peso  (1000 libras)", ylab = "Consumo  - Galones / Milla")


#*******************************************************************************
# PASO 3: EL AN�LISIS
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

# 3.1. C�lculo regresi�n lineal
#*******************************************************************************
regresionLinealSimple <- lm(formula = consumo ~ peso, varEstudio)
summary(regresionLinealSimple)

# 3.2. Revision de los residuos
#*******************************************************************************
# � Scatter 2D para ver que no tenemos autocorrelaci�n
# � Mirar si se distribuyen normalmente
# Scatter 2D
residuos <- as.numeric(regresionLinealSimple$residuals)
df <- data.frame(Residuos = residuos)
name1 <- names(df)

# Scatter 2D de los residuos
p <- plot_ly(x = seq(1,nrow(df)),y = df[,1],mode = "markers", type = "scatter")%>%
  layout(title = "Residuos Regresi�n Lineal",yaxis = list(title = "Residuos"),yaxis = list(title = "�ndice")) 

# Histograma de frecuencias con plotly
p1<-plot_ly(x = df[,name1], type = "histogram",name = name1,marker=list(color='#FF6666'))%>%
  layout(title = paste("Histograma de Frecuencias de",name1),yaxis = list(title = name1),bargap=0.005*(max(df[,name1])-min(df[,name1])))

# Boxplot
p2<-plot_ly(y = df[,name1], type = "box",name = name1,boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1),yaxis = list(title = name1))

# QQplot
p3<-ggqqplot(df, x = names(df),color = "#FF6666",add.params = list(color = "black"))+
  xlab("Distribuci�n Te�rica Normal") + ylab("Cuartiles reales") +
  theme_minimal() +
  ggtitle(paste("QQ-plot de ", name1,sep = "")) +
  theme(plot.title = element_text(hjust = 0.5))
p3 <- ggplotly(p3)
subplot(p, p1, p2, p3)%>%
  layout(title = paste("Comprobacion residuos"))

# Prueba Distribuci�n Normal
shapiro.test(residuos)

#*******************************************************************************
# PASO 4: CONCLUSI�N
#*******************************************************************************

# En un word....
