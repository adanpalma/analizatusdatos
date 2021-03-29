#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
#********************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("lme4","lmerTest", "FSA","car","plotly","ggplot2", "plotly", "xlsx","scales","stringr","readr","dplyr","psych","readxl","ggpubr","PerformanceAnalytics")

.packages %in% installed.packages()
##
# Instala los paquetes sinÃ³ los tienes instalados
##
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

#********************************************************************
# 1.2 CARGAR PAQUETES O CREAR FUNCIONES
#********************************************************************
# Carga los paquetes sinÃ³ los tienes cargados
lapply(.packages, require, character.only=TRUE)

##
#USING PACKAGES FROM devtools not from CRAN
##

#install.packages("devtools")
#devtools::install_github("johannesbjork/LaCroixColoR")
#lapply("LaCroixColoR",require,character.only=TRUE)


##
# Se definen funciones
##
descriptiva <-  function(Datos,columnas,colagruparpor) {
  
  filtrar_por <-  unique(Datos[,colagruparpor])
  listaout <- list()
  
  for (g in (1:nrow(filtrar_por))) 
  {
    
    if (is.list(filtrar_por[g,1])) 
    {
      df <-   filter(Datos, (Datos[,colagruparpor] %in% (filtrar_por[g,1])) ) %>%
        dplyr::select(c(colagruparpor,columnas))    
    } else 
    {
    df <-   filter(Datos, (Datos[,colagruparpor] == str_trim(filtrar_por[g,1])) ) %>%
      dplyr::select(c(colagruparpor,columnas))
    }
    for (i in (2:ncol(df)))
    {
      lista <- list( 
        Grupo = str_trim(filtrar_por[g,1]),
        media =   format(apply(df[,i],2,mean),nsmall=1,big.mark = ","),
        cutmedia =    format(apply(df[,i],2,mean,trim=0.05),nsmall=1,big.mark = ","),
        StdErr   =    format(apply(df[,i],2,sd) / sqrt(apply(df[,i],2,length)),nsmall=1,big.mark = ","),
        IC95LimInf =  format(apply(df[,i],2,mean) - (1.96 *  apply(df[,i],2,sd) / sqrt(apply(df[,i],2,length))),nsmall=1,big.mark = ","),
        IC95LimSup =  format(apply(df[,i],2,mean) + (1.96 *  apply(df[,i],2,sd) / sqrt(apply(df[,i],2,length))),nsmall=1,big.mark = ","),
        mediana=   format(apply(df[,i],2,median),nsmall=1,big.mark = ","),
        sd=    format(apply(df[,i],2,sd),nsmall=1,big.mark = ","),
        var =   format(apply(df[,i],2,var),nsmall=1,big.mark = ","),
        obs =   apply(df[,i],2,length),
        Min =   format(apply(df[,i],2,min),nsmall=1,big.mark =","),
        Max=    format(apply(df[,i],2,max),nsmall=1,big.mark = ","),
        Rango =   format(apply(df[,i],2,max) -  apply(df[,i],2,min),nsmall = 1,big.mark = ","),
        RangoIQR =  format( apply(df[,i],2,quantile,prob=0.75) - apply(df[,i],2,quantile,prob=0.25),nsmall = 1, big.mark = ","),
        Q1=   format(apply(df[,i],2,quantile,prob=0.25),nsmall = 1, big.mark = ","),
        Q3=   format(apply(df[,i],2,quantile,prob=0.75),nsmall = 1, big.mark = ","),
        skew=    round(apply(df[,i],2,skew),2),
        kurtosis =   round(apply(df[,i],2, kurtosi))
        
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
    geom_jitter(mapping = aes(x=get(var_x),y=get(var_y)),position=position_jitter(0.2))+
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
# Seteo el Directorio....
##
setwd("~/Analiza tus Datos/Etapa 2/Bloque 5 Analiza tus Datos/3. Tablas ANOVA de 1 FACTOR MAS DE 2 GRUPOS")

####
# Cargo la data
####

df <- espalda <- read_excel("espalda.xlsx")

###
# Preparo datos, creo columnas, grupos etc
###
df$diffodi  <- df$`ODI Mes0` - df$`ODI Mes1` # agrego  variable con la mejoria


#creo factores 
df$NHD <- factor(df$NHD,labels  = c("Sin Hernia","1 Hernia","2 Hernias")) #Hernias
df$Grupo <- factor(df$Grupo,labels = c("Convencional","Experimental")) #Tipo de tratamiento



###
# Descriptiva HISTOGRAMAS, QQPLOT, BOXPLOTS
###

# 1 DESCRIPCI?N 

#Valido que me
# boxplot, histograma densidad, diagramas de error
ggplot(df) +
  geom_density(mapping = aes(x=diffodi,fill=NHD),alpha=0.2)

creaboxplot(df,"NHD","diffodi","Box Plot Diffoddi por Hernias","Hernias","diffoddi")
crea_errdiagram(df,"diffodi","NHD","Diagrama de Medias diffoddi por NHD","NHD","diffodi")



##
# CALCULO DE LA TABLA ANOVAS
##

t_aov <-  aov(diffodi ~ NHD,data=df)
summary.aov(t_aov)

##
# Calculo Normalidad de los residuales de la Anova
##
shapiro.test(t_aov$residuals)

residuales <- residuals(object = t_aov)
plot(residuales,main = "Plot de Residuales del Anova por NHD")
###
# Calculo Igualdad de Varianzas solo para no olivar el paso
# Si el Shapiro.test no sale Normal debo ir directo a Kruskall wallis
###
leveneTest(diffodi ~ NHD, data=df)


##
# Como los residuales de ANOVA NO dieron  NORMALES se usa Kruskall-wallis
# TEST NO PARAMETRICO
##
kruskal.test(diffodi ~ NHD,data=df)



#####
# Ahora se trabajara con pacientes del grupo de tratamiento = 0  (tratamiento convencional)
# para analizar si han habido diferencias entre la discapacidad antes del tratamiento  
# y despues del tratamiento mes 0 y mes 1
#
#### 

#Cargo los Datos
setwd("~/Analiza tus Datos/Etapa 2/Bloque 5 Analiza tus Datos/3. Tablas ANOVA de 1 FACTOR MAS DE 2 GRUPOS")
df <- read_excel("espalda.xlsx")
str(df)

# Creo dataframe con el valor mes0 y mes 1 como parte de una columa
# el No de mes es otra columa
#agreo el id del paciente



df_gruposrepetidos <-  data.frame(
                       Discapacidad = c(df$`ODI Mes0`[df$Grupo == 0], df$`ODI Mes1`[df$Grupo == 0]),
                       MesMedicion  = c(rep(0,length(df$`ODI Mes0`[df$Grupo == 0])), rep(1,length(df$`ODI Mes1`[df$Grupo == 0]))),
                       IdPaciente   = c(seq(1,length(df$`ODI Mes0`[df$Grupo == 0])), seq(1,length(df$`ODI Mes0`[df$Grupo == 0])))  
)



df_gruposrepetidos$MesMedicion <- factor(df_gruposrepetidos$MesMedicion, label=c("Mes 0","Mes 1"))

# 1 DESCRIPCI?N 

#Valido que me
# boxplot, histograma densidad, diagramas de error
ggplot(df_gruposrepetidos) +
  geom_density(mapping = aes(x=Discapacidad,fill=MesMedicion),alpha=0.2) +
  ggtitle("Histograma de Densidad para Grupo Repetido Tratamiento Convencional")
  

creaboxplot(df_gruposrepetidos,"MesMedicion","Discapacidad","Box Plot Discapcidad Grupo 0 Mes 0 y Mes 1","MesMedicion","Discapaciad")
crea_errdiagram(df_gruposrepetidos,"Discapacidad","MesMedicion","Diagrama de Medias dDiscapcidad Grupo 0 Mes 0 y Mes 1","MesMedicion","Discapacidad")


####
#  TEST PARAMETRICO DE ANOVA DE MEDIDAS REPETIDAS LMER (SE USA ESTE SI SON NORMALES POR LO QUE HAY QUE COMPROBARLO)
###

#lmer(PARA MODELOS LINEALES MIXTOS)
#glmer(para modelos lineales generalizados mixtos)
#nlmer(para modelos no lineales)

##Se usa lmer porque es lineal mixto
anova_medidarep <-  lmer(Discapacidad ~ MesMedicion + (1|IdPaciente), data=df_gruposrepetidos)
#Calculamos la anova del modelo lineal mixto se usa anova
anova(anova_medidarep)

#Se obtienen los residuos y luego se pasan por shapiro wilks y levenetest
residuos <- residuals(object = anova_medidarep)
shapiro.test(residuos) #Validando Normalidad

#
### Grafico los residuales para ver como se ve la distribucion 
#
dfre <- data.frame(residuos)
name1 <- names(dfre)

##QQPLOT
p3<-ggqqplot(dfre , x =name1,color = "#FF6666",add.params = list(color = "black"))+
  xlab("Distribuci?n Te?rica Normal") + ylab("Cuartiles reales") +
  theme_minimal() +
  ggtitle(paste("QQ-plot de ", name1,sep = "")) +
  theme(plot.title = element_text(hjust = 0.5))
p3 <- ggplotly(p3)
p3 
hist(dfre$residuos) ##histograma
ggplot(dfre) +geom_boxplot(mapping = aes(dfre$residuos)) # boxplot
plot(y=(seq(-6,195)),x=dfre$residuos,main="plot(residuos) VISUALMENTE NO \n PARECEN NORMALES usando PLOT")




#EN CASO DE QUE NO SALGA NORMAIDAD USO FRIEDMAN.TEST
friedman.test(df_gruposrepetidos[,1],group=df_gruposrepetidos[,2],blocks=df_gruposrepetidos[,3])





