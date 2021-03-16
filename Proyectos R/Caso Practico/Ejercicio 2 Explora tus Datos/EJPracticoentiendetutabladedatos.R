#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
#********************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("ggplot2", "plotly", "xlsx","scales","stringr","readr","dplyr","psych","readxl","ggpubr")


# Instala los paquetes sinó los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])


#********************************************************************
# 1.2 CARGAR PAQUETES O CREAR FUNCIONES
#********************************************************************
# Carga los paquetes sinó los tienes cargados
lapply(.packages, require, character.only=TRUE)


####################################
# Setear el working directory
# Leo la tabla de datos
####################################
setwd("C:/Users/apalmad/Desktop/Analiza tus Datos/Proyectos R/Caso Practico/Ejercicio 4 EstInferencial II")
Datos <- read_xlsx("Datos.xlsx")

###############################
# Describo las variables
###############################
str(Datos)


##
# Se definen funciones
##
descriptiva <-  function(Datos,columnas,colagruparpor) {
  
  filtrar_por <-  unique(Datos[,colagruparpor])
  listaout <- list()
  
  for (g in (1:nrow(filtrar_por))) 
  {
    df <-   filter(Datos, (Datos[,colagruparpor] == str_trim(filtrar_por[g,1])) ) %>%
      select(c(colagruparpor,columnas))
    for (i in (2:ncol(df)))
    {
      lista <- list( 
        Grupo = str_trim(filtrar_por[g,1]),
        media =   apply(df[,i],2,mean),
        cutmedia =    apply(df[,i],2,mean,trim=0.05),
        StdErr   =  apply(df[,i],2,sd) / sqrt(apply(df[,i],2,length)),
        IC95LimInf =  apply(df[,i],2,mean) - (1.96 *  apply(df[,i],2,sd) / sqrt(apply(df[,i],2,length))),
        IC95LimSup =  apply(df[,i],2,mean) + (1.96 *  apply(df[,i],2,sd) / sqrt(apply(df[,i],2,length))),
        mediana=   apply(df[,i],2,median),
        sd=    apply(df[,i],2,sd),
        var =   apply(df[,i],2,var),
        obs =   apply(df[,i],2,length),
        Min =   apply(df[,i],2,min),
        Max=    apply(df[,i],2,max),
        Rango =   apply(df[,i],2,max) -  apply(df[,i],2,min),
        RangoIQR =   apply(df[,i],2,quantile,prob=0.25),
        Q1=   apply(df[,i],2,quantile,prob=0.75),
        Q3=   apply(df[,i],2,quantile,prob=0.75) - apply(df[,i],2,quantile,prob=0.25),
        skew=    apply(df[,i],2,skew),
        kurtosis =   apply(df[,i],2, kurtosi)
        
      )
      listaout[[str_trim(filtrar_por[g,1])]] <- lista
      
    }
  }
  
  #Colocando Nombres a las Columnas y Filas a la matriz estadistica descriptiva
  mt <- matrix(ncol = nrow(filtrar_por), nrow = 18 )
  colnames(mt) <- names(listaout)
  rownames(mt) <- names(listaout[[1]])
  
  for (i in names(listaout))
  {
    mt[,i] <- unlist(listaout[[i]])
  }   
  mt
}
crea_errdiagram <- function(Datos,columna,agrupapor,titulo="Titulo",xlabel="xlabel",ylabel="ylabel"){
  
  
  dfsumarize <- Datos %>%
    group_by(get(agrupapor)) %>%
    summarise(
      sd = sd(get(columna),na.rm = TRUE),
      media = mean(get(columna)))
  
  #####
  #  Creo grafico de errores o de medias
  ####
  ggplot(dfsumarize, aes(x=reorder(`get(agrupapor)`,media), y=media,color=`get(agrupapor)`)) +
    geom_line(aes(group = 1)) +
    geom_errorbar( aes(ymin=media-sd,ymax=media + sd),width = 0.2) +
    geom_point(size = 2) +
    labs(title = titulo,
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
creaboxplot <- function(Datos, var_x,var_y,Titulo,xlabel="NA",ylabel="NA"){
  ggplot(data = Datos) +
    geom_boxplot(mapping = aes(
      x = reorder(get(var_x),get(var_y),FUN = median),
      y = get(var_y),fill=get(var_x)
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
creagrafcorr <-  function(Datos,colx,coly,title,etiquetax,etiquetay,grupo=NA)  {

    
  ggscatter(Datos, x = colx, y = coly,
            color = ifelse(is.na(grupo),"black",grupo), shape = 21, size = 3,
            fill = ifelse(is.na(grupo),"lightgray",grupo),
            palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
            facet.by = ifelse(is.na(grupo),NA,grupo),
            title = title,
            xlab =  etiquetax,
            ylab =  etiquetay,
            add = "reg.line",  
            add.params = list(color = "blue", fill = "lightgray"), 
            conf.int = TRUE, 
            cor.coef = TRUE, 
            show.legend.text = TRUE,
            cor.coeff.args = list(method = "pearson", label.x.npc = "left", label.y.npc = "top", label.sep = "\n")
  ) +
    stat_regline_equation(label.x.npc = "left",label.y.npc = "top",output.type = "expression", inherit.aes = TRUE)
}


###
# Uso la funcion describeBy del package "psych" y me da todas los parametros descriptivos 
# agrupados por grupo
###

EstaDesc_FertilityxGrupo <- t(describeBy(Datos$fertility,Datos$group,mat=TRUE))
EstaDesc_FertilityxGrupo

EstaDesc_FertilityxGrupo <- t(describeBy(Datos$ppgdp,Datos$group,mat=TRUE))
EstaDesc_FertilityxGrupo

#fertility group by group
est_descript <- descriptiva(Datos,(4:4),3) 
est_descript
crea_errdiagram(Datos,"fertility","group","Fertility x Grupos","Grupos","Avg(Fertility)") #diagrama de error

#ppgdp group by group
est_descript <- descriptiva(Datos,(5:5),3) 
est_descript
crea_errdiagram(Datos,"ppgdp","group","Pib x Grupos","Grupos","Avg(PIB)") #diagrama de error

#lifeExpF group by group
est_descript <- descriptiva(Datos,(6:6),3) 
est_descript
crea_errdiagram(Datos,"lifeExpF","group","lifeExpF x Grupos","Grupos","lifeExpF)") #diagrama de error

#pctUrban group by group
est_descript <- descriptiva(Datos,(7:7),3) 
est_descript
crea_errdiagram(Datos,"pctUrban","group","pctUrban x Grupos","Grupos","pctUrban)") #diagrama de error

#infantMortality group by group
est_descript <- descriptiva(Datos,(8:8),3) 
est_descript
crea_errdiagram(Datos,"infantMortality","group","infantMortality x Grupos","Grupos","infantMortality)") #diagrama de error

# Primeros Analisis

##Box Plot para comparar grupos (comparacion de medias entre grupos)
creaboxplot(Datos,"group","fertility","Box Plot Fertility  vs Grupos","Grupos")
creaboxplot(Datos,"group","lifeExpF","Box Plot Life Exp vs Grupos","Grupos")
creaboxplot(Datos,"group","ppgdp","Box Plot PIB vs Grupos","Grupos")
creaboxplot(Datos,"group","pctUrban","Box Plot Poblacion Urbana vs Grupos","Grupos","pctUrban")
creaboxplot(Datos,"group","infantMortality","Box Plot infantMortality vs Grupos","Grupos","infantMortality")


## Scatterplot para buscar asociaciones entre variables
creascatplot(Datos,"lifeExpF","infantMortality","infantMortality vs Esperanza de Vida","Esperanza de Vida","infantMortality")
creascatplot(Datos,"LogNepPib","infantMortality","infantMortality vs PIB","LogNepPib","infantMortality")
creascatplot(Datos,"pctUrban","infantMortality","infantMortality vs Pobl. Urabana","pctUrban","infantMortality")
creascatplot(Datos,"fertility","infantMortality","infantMortality vs Fertilidad","fertility","infantMortality")

# Diagrama de Correlaciones
creagrafcorr(Datos,"fertility","infantMortality","Correlacion Fertility vs InfantMortality","Fertility","InfantMoratity","group")
creagrafcorr(Datos,"ppgdp","infantMortality","Correlacion ppgdp vs InfantMortality","ppgdp","InfantMoratity","group")
creagrafcorr(Datos,"LogNepPib","infantMortality","Correlacion LogNepPib vs InfantMortality","LogNepPib","InfantMoratity","group")
creagrafcorr(Datos,"pctUrban","infantMortality","Correlacion pctUrban vs InfantMortality","pctUrban","InfantMoratity","group")


# Coef. de Correlacion InfanMortality vs variables
print(paste("r de InfantMortality vs Fertilidad ", cor(Datos$infantMortality,Datos$fertility)))
print(paste("r de InfantMortality vs Poblacion Urbana ", cor(Datos$infantMortality,Datos$pctUrban)))
print(paste("r de InfantMortality vs PIB ", cor(Datos$infantMortality,Datos$ppgdp)))
print(paste("r de InfantMortality vs Esperzana de Vida ", cor(Datos$infantMortality,Datos$lifeExpF)))


#Matriz de Correlacion

res <- Datos %>% select(4:8)
res <- cor(res)
round(res, 2)


#####
# Ejercicio 3. Estadistica Inferencial I
# Constraste de Hpotesis (DESCRIPTIVA, BOXPLOTS, DIAGRAMAS DE ERORS Y ANOVAS
####

#infantMortality reviso descriptiva de datos, luego un blox plot,diagram error y anovas por grupo InfantMortality
dfa <- data.frame(descriptiva(Datos,(8:8),(3:3)))
creaboxplot(Datos,"group","infantMortality", "InfantMortality by Group","Grupo","InfantMortlity")
crea_errdiagram(Datos,"infantMortality","group","InfantMortality x Grupos","Grupos","Avg(InfatMortality)") #diagrama de error
#ANOVA
aovinfMortxGrupo <- (aov(infantMortality ~ group,Datos)) # Anova del grupo
summary(aovinfMortxGrupo) # Imprime suma resisuales, mean sq, Significancia
TukeyHSD(aovinfMortxGrupo) ## Anova entre los grupos para verlo por pares

# lifeExpF paso reviso descriptiva de datos, luego un blox plot,diagram error y anovas por grupo InfantMortality
dfe <- descriptiva(Datos,(6:6),(3:3))
creaboxplot(Datos,"group","lifeExpF", "lifeExpF by Group","Grupo","lifeExpF")
crea_errdiagram(Datos,"lifeExpF","group","lifeExpF x Grupos","Grupos","Avg(lifeExpF)") #diagrama de error
#ANOVA
aovlifeExpF<- (aov(lifeExpF ~ group,Datos)) # Anova del grupo
summary(aovlifeExpF) # Imprime suma resisuales, mean sq, Significancia
TukeyHSD(aovlifeExpF) ## Anova entre los grupos para verlo por pares


#####
# Ejercicio 3. Estadistica Inferencial II
# Modelos
####

# Realizo Scatter plot o diagrama de dispersion Infant Mortality vs PIB

creagrafcorr(Datos,"ppgdp","infantMortality","Diag.Dispersion InfantMortality vs PIB","ppgdp","infantMortality",NA)

Datos <- mutate(Datos,
       lneppib = log10(ppgdp))


# Realizo scatter plot o diagrama de dispersion infanct Mortality vs log(pib)
creagrafcorr(Datos,"lneppib","infantMortality","Diag.Dispersion InfantMortality vs lneppib","lneppib","infantMortality",NA)

#Realizo el diagrama de dispersi�n de InfantMortality seperado por el grupo al que pertenece 
creagrafcorr(Datos,"lneppib","infantMortality","Diag.Dispersion InfantMortality vs lneppib","lneppib","infantMortality","group")

####
# Se calcula Regresion Lineal Multiple de InfantMortality y Ln(pib) + pctUrban
###

model <- lm(infantMortality ~ lneppib + pctUrban,data = Datos)
summary(model)

creagrafcorr(Datos,"pctUrban","lneppib","Correlacion pctUrban vs pib","pctUrban","lneppib")
