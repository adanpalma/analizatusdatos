#*******************************************************************************

# PROBLEMA NÚMERO 6 - TEST DE NORMALIDAD
#*******************************************************************************
#-------------------------------------------------------------------------------




# INSTALAR PAQUETES DE FUNCIONES
#*******************************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("pastecs","ggplot2","ggpubr","plotly")

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
misDatos <- read.delim(file.choose(),header = TRUE)

# Utiliza este comando para leer archivos con separador
# misDatos <- read.csv(file.choose(),header = TRUE,sep = '  ')

# Nota: Asegurate que tienes los nombres de las variables puestas


# PASO 0.2 - IDENTIFICA VARIABLES NUMÉRICAS, CATEGÓRICAS
#-------------------------------------------------------------------------------
# Puedes utilizar este comando para diferenciar tres tipos de variables:
# 1-Numeric: variables numéricas reales
# 2-Integer: son variables con números enteros.
#           Puede ser numérica discreta o categórica (tienes que decidir donde crees que será)
# 3-Factor: variables con caracteres. Siempre seran categóricas.
variables <- split(names(misDatos),sapply(misDatos, function(x) paste(class(x), collapse=" ")))

# La variable edad es numérica
varNumericas <- c(variables$numeric,variables$integer)

# Las variables categóricas son los factores.
varCategoricas <- variables$factor

#*******************************************************************************
# PASO 1: QUÉ ESTÁS BUSCANDO
#*******************************************************************************

# La variable: LungCap sea una distribución Normal
varEstudio <- misDatos[varNumericas[1]]

#*******************************************************************************
# PASO 2: UTILIZA UN MAPA PARA ORIENTARTE
#*******************************************************************************

# Para saber si una variable es normal utilizaré técnicas descriptivas: qqplot, histograma, boxplot
# y un test de hipótesis de Normalidad: 
# Cuando la muestra es más grande que 30 haremos un test de shapiro Wilk para comprobar
# que no tenga mayores desviaciones de la normal.

# PASO 2.1: DESCRIPTIVA DE APOYO
#*******************************************************************************
df <- varEstudio
name1 <- names(df)
# Histograma de frecuencias con plotly
p1<-plot_ly(x = df[,name1], type = "histogram",name = name1,marker=list(color='#FF6666'))%>%
  layout(title = paste("Histograma de Frecuencias de",name1),yaxis = list(title = name1),bargap=0.005*(max(df[,name1])-min(df[,name1])))

# Boxplot
p2<-plot_ly(y = df[,name1], type = "box",name = name1,boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1),yaxis = list(title = name1))

# QQplot
p3<-ggqqplot(df, x = names(df),color = "#FF6666",add.params = list(color = "black"))+
  xlab("Distribución Teórica Normal") + ylab("Cuartiles reales") +
  theme_minimal() +
  ggtitle(paste("QQ-plot de ", name1,sep = "")) +
  theme(plot.title = element_text(hjust = 0.5))
p3 <- ggplotly(p3)
p <- subplot(p1, p2, p3)%>%
  layout(title = paste("Histograma de Frecuencias Boxplot y QQplot para la variable  ", name1,sep = ""))
p

# PASO 2.2: TEST DE HIPÓTESIS: SHAPIRO WILK
#*******************************************************************************

shapiro.test(varEstudio[,1]) # fíjate que esta función trabaja con los valores NO los dataframes

#*******************************************************************************
# PASO 3: APLICA LA PLANTILLA
#*******************************************************************************

# En un word....

