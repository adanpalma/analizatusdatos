# INSTALAR PAQUETES DE FUNCIONES
#*******************************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("ggplot2")

# Instala los paquetes sinó los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# CARGAR PAQUETES O CREAR FUNCIONES
#*******************************************************************************
# Carga los paquetes sinó los tienes cargados
lapply(.packages, require, character.only=TRUE)
#-------------------------------------------------------------------------------



#*******************************************************************************

# HISTOGRAMA DE DENSIDAD POR GRUPOS

#*******************************************************************************
#-------------------------------------------------------------------------------

misDatos <- iris


# Selecciono la variable 1 y 5. La primera numérica y la segunda categórica
df <- misDatos[,c(1,5)]

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
p1


# El gráfico se guarda en p1