# INSTALAR PAQUETES DE FUNCIONES
#*******************************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("plotly","ggpubr","ggplot2")

# Instala los paquetes sinó los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# CARGAR PAQUETES O CREAR FUNCIONES
#*******************************************************************************
# Carga los paquetes sinó los tienes cargados
lapply(.packages, require, character.only=TRUE)
#-------------------------------------------------------------------------------


#*******************************************************************************

# QQPLOT
#*******************************************************************************
#-------------------------------------------------------------------------------

misDatos <- iris

df <- misDatos[1] # Selecciono la primera variable

# QQplot
p1<-ggqqplot(df, x = names(df),color = "#FF6666",add.params = list(color = "black"))+
  xlab("Distribución Teórica Normal") + ylab("Cuartiles reales") +
  theme_minimal() +
  ggtitle(paste("QQ-plot de ", name1,sep = "")) +
  theme(plot.title = element_text(hjust = 0.5))
p1 <- ggplotly(p1) # convertimos a plotly
p1