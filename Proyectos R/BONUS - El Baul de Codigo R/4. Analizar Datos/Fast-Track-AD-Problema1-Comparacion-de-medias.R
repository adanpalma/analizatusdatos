#*******************************************************************************

# PROBLEMA N�MERO 1 - COMPARACI�N DE MEDIAS

#*******************************************************************************
#-------------------------------------------------------------------------------


# INSTALAR PAQUETES DE FUNCIONES
#*******************************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("ggpubr","ggplot2","plotly")

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

misDatos <- iris
# Puedes utilizar este comando para diferenciar tres tipos de variables:
# 1-Numeric: variables num�ricas reales
# 2-Integer: son variables con n�meros enteros.
#           Puede ser num�rica discreta o categ�rica (tienes que decidir donde crees que ser�)
# 3-Factor: variables con caracteres. Siempre seran categ�ricas.
str(misDatos)
variables <- split(names(misDatos),sapply(misDatos, function(x) paste(class(x), collapse=" ")))

# La variable edad es num�rica
varNumericas <- c(variables$numeric,variables$integer)

# Las variables categ�ricas son los factores.
varCategoricas <- variables$factor
#-------------------------------------------------------------------------------



#*******************************************************************************
# PASO 1 - ENTENDER TUS DATOS - DEFINIR EL OBJETIVO
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

# 1.1. �De d�nde provienen la base de datos?

# Udalmap es un Sistema de Informaci�n municipal, cuya finalidad es mostrar con detalle
# la realidad en los municipios de la C.A. de Euskadi. Permite, a su vez, el dise�o y 
# evaluaci�n de pol�ticas p�blicas, orientadas a facilitar la toma de decisiones en m�ltiples
# �mbitos relacionados con el crecimiento y el desarrollo del territorio, en aras de un mayor
# grado de cohesi�n territorial, econ�mica, social y respeto medioambiental.
# http://datos.gob.es/es/catalogo/a16003011-indicadores-municipales-de-sostenibilidad-pib-municipal-por-persona-ocupada-base-cae100

# 1.2. �Qu� variables tienes?

# 6 Variables:
# Municipio (categ�rica) nombre de los municipios
# 2012, 2010, 2008, 2006, 2004 (num�ricas): son los datos del PIB de los municipios en los a�os marcados

# 1.3. �Qu� objetivo tienes para este estudio?
# Valorar si la longitud del s�palo es diferente entre las especies de Setosa y versicolor
#-------------------------------------------------------------------------------




#*******************************************************************************
# PASO2 - EXPLORAR TU TABLA DE DATOS
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

# COPIA LAS VARIABLES QUE ME INTERESAN
#-------------------------------------------------------------------------------
# Encuentro las filas que sean de versicolor o setosa
filasVersicolorYSetosa <- (misDatos[,varCategoricas[1]]=="versicolor") | (misDatos[,varCategoricas[1]]=="setosa")
# Copio las filas de versicolor y setosa. La primera columna ser� el sepal.length, la segunda
# species
varEstudio <- misDatos[filasVersicolorYSetosa,c(varNumericas[1],varCategoricas[1])]
# Ahora tienes una columna con sepal.length y la segunda de species con versicolor y setosa

# Nota: Asegurate que tienes los nombres de las variables puestas

df <- varEstudio

# Histograma de densidad para comparar grupos
#-------------------------------------------------------------------------------
name1 <- names(df)[1]
name2 <- names(df)[2]
p1<- ggplot(df, aes(df[,1], fill = df[,2])) +
  geom_density(alpha = 0.2) +
  xlab("Rango de clases") + ylab("Densidad") +
  theme_minimal() +
  ggtitle(paste("Histograma de Densidad", name1,"comparando por grupos de ",name2)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=""))

# Boxplot de dos grupos
#-------------------------------------------------------------------------------
name1 <- names(df)[1]
name2 <- names(df)[2]
p2<- plot_ly(y = df[,name1], x = df[,name2], type = "box",name = name1, boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1, "en relaci�n al grupo ",name2),yaxis = list(title = name1),xaxis = list(title = name2))

subplot(p1,p2)




#*******************************************************************************
# PASO3 - ANALIZAR TUS DATOS
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************


# Comparar dos grupos:

# En la pr�ctica:
# .	Cuando las distribuciones de los 2 grupos son normales y varianzas iguales >> T-test (param�trico)
# .	Cuando las distribuciones de los 2 grupos son normales y varianzas distintas >> T-test correcci�n welch (param�trico)
# � Cuando alguna distribuci�n NO es normal >> Wilcoxon (no param�trico)


# 3.1: RESTRICCIONES NORMALIDAD >> Los dos grupos son Normales p>0.05
#-------------------------------------------------------------------------------
shapiro.test(varEstudio[varEstudio[,2]=="setosa",1])
# Setosa es Normal
shapiro.test(varEstudio[varEstudio[,2]=="versicolor",1])
# Versicolor es Normal


# 3.2: RESTRICCIONES VARIANZA >> Las varianzas son DIFERENTES p<0.05
#-------------------------------------------------------------------------------
var.test(Sepal.Length ~ Species, data = varEstudio)


# TEST DE HIP�TESIS: T-test  CON CORRECI�N WELCH (est� en el package car) o BARTLETT
#-------------------------------------------------------------------------------
# lo podemos aplicar con un dataFrame: RECOMENDADO
# F�jate que ponemos el nombre de la variable Num�rica primero y despu�s la categ�rica
# a mano
# T-test con igualdad de varianzas con un data frame
t.test(Sepal.Length ~ Species, data = varEstudio, var.equal = FALSE)

# T-test con igualdad de varianzas con un data frame SERIA
# t.test(Sepal.Length ~ Species, data = varEstudio, var.equal = TRUE)

# Si tienes dos vectores num�ricos representando los dos grupos puedes hacerlo as�:
var1 <- varEstudio[varEstudio[,2]=="setosa",1]
var2 <- varEstudio[varEstudio[,2]=="versicolor",1]
t.test(var1,var2,var.equal = FALSE)
# El p-valor peque�isimo nos indica grandes diferencias entre grupos




#-------------------------------------------------------------------------------
# PASO 4 - PRESENTAR RESULTADOS y CONCLUSI�N
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

# Ser�a copiar y pegar los gr�ficos m�s interesantes y si los resultados del test
# de comparaci�n de medias
