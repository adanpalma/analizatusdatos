#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
#********************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("ggplot2", "plotly", "xlsx","scales","readr","dplyr","psych","readxl")


# Instala los paquetes sinó los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])


#********************************************************************
# 1.2 CARGAR PAQUETES O CREAR FUNCIONES
#********************************************************************
# Carga los paquetes sinó los tienes cargados
lapply(.packages, require, character.only=TRUE)


#####################################
#
#Setear el working directory
#
# Leeo la tabla de datos
####################################
setwd("C:/Users/apalmad/Desktop/Analiza tus Datos/Proyectos R")

Datos <- read_xlsx("Caso Practico/Entiende tu Tabla de Datos/Datos.xlsx")

#################################
# Describo las variables
###############################
str(Datos)





creaboxplot <- function(Datos, var_x,var_y,Titulo,xlabel,ylabel="NA"){
  ggplot(data = Datos) +
    geom_boxplot(mapping = aes(
      x = reorder(get(var_x),get(var_y),FUN = median),
      y = get(var_y)
    )) +
    labs(title = Titulo,
         subtitle = "",
         caption = "Data source: Analiza tus Datos",
         x = xlabel,
         y = ylabel,
         tag = "Primeros Programas con R") +
    theme(plot.title    = element_text(size =12, face = "bold", hjust = 0.5  ),
          plot.subtitle = element_text(size = 9, hjust = 0.5),
          plot.caption  = element_text(size = 7),
          plot.tag      = element_text(size = 7),
          axis.text     = element_text(size=8),
          axis.title    = element_text(size=8,face="bold"))
  
  
}


creascatplot <- function(Datos, var_x,var_y,Titulo,xlabel,ylabel="NA",categoria="group"){
  ggplot(data = Datos) +
    geom_point(mapping = aes(x = get(var_x), y = get(var_y),color=get(categoria))) +
    
    labs(title = Titulo,
         subtitle = "",
         caption = "Data source: Analiza tus Datos",
         x = xlabel,
         y = ylabel,
         tag = "Primeros Programas con R") +
    theme(plot.title    = element_text(size =12, face = "bold", hjust = 0.5  ),
          plot.subtitle = element_text(size = 9, hjust = 0.5),
          plot.caption  = element_text(size = 7),
          plot.tag      = element_text(size = 7),
          axis.text     = element_text(size=8),
          axis.title    = element_text(size=8,face="bold")) +
    geom_smooth(mapping = aes(x = get(var_x), y = get(var_y)), orientation = "y")
  #  facet_grid(. ~ get(categoria))  
  
  
}

#############################
#
# Primeros Analisis
#
#############################
##
##Box Plot para comparar grupos (comparacion de medias entre grupos)
##
creaboxplot(Datos,"group","fertility","Box Plot Fertility  vs Grupos","Grupos")
creaboxplot(Datos,"group","lifeExpF","Box Plot Life Exp vs Grupos","Grupos")
creaboxplot(Datos,"group","ppgdp","Box Plot PIB vs Grupos","Grupos")
creaboxplot(Datos,"group","pctUrban","Box Plot Poblacion Urbana vs Grupos","Grupos","pctUrban")
creaboxplot(Datos,"group","infantMortality","Box Plot infantMortality vs Grupos","Grupos","infantMortality")


##
## Scatterplot para buscar asociaciones entre variables
##
creascatplot(Datos,"lifeExpF","infantMortality","infantMortality vs Esperanza de Vida","Esperanza de Vida","infantMortality")
creascatplot(Datos,"LogNepPib","infantMortality","infantMortality vs PIB","LogNepPib","infantMortality")
creascatplot(Datos,"pctUrban","infantMortality","infantMortality vs Pobl. Urabana","pctUrban","infantMortality")
creascatplot(Datos,"fertility","infantMortality","infantMortality vs Fertilidad","fertility","infantMortality")

####
# Coef. de Correlacion InfanMortality vs variables
###
print(paste("r de InfantMortality vs Fertilidad ", cor(Datos$infantMortality,Datos$fertility)))
print(paste("r de InfantMortality vs Poblacion Urbana ", cor(Datos$infantMortality,Datos$pctUrban)))
print(paste("r de InfantMortality vs PIB ", cor(Datos$infantMortality,Datos$ppgdp)))
print(paste("r de InfantMortality vs Esperzana de Vida ", cor(Datos$infantMortality,Datos$lifeExpF)))










