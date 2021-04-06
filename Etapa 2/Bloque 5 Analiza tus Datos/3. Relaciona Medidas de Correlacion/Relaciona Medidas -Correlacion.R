#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
#********************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("Hmisc","lme4","lmerTest", "FSA","car","plotly","ggplot2", "plotly", "xlsx","scales","stringr","readr","dplyr","psych","readxl","ggpubr","PerformanceAnalytics")
.packages %in% installed.packages()
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
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
crea_errdiagram <- function(Datos,columna,agrupapor,titulo="Titulo",xlabel="xlabel",ylabel="ylabel",var_color ="NA"){
  dfsumarize <- Datos %>%
    group_by(get(agrupapor),get(var_color)) %>%
    summarise(sderr = sd(get(columna),na.rm = TRUE)/sqrt(n()),
      media = mean(get(columna)))

  pd <- position_dodge(0.5) #Es para que las barras de error no se overlapen y se espacien horizontalmente
  
  ggplot(dfsumarize, aes(x=`get(var_color)`, y=media,color=`get(var_color)`,group=1)) +
    geom_errorbar( aes(ymin=media-1.96*sderr,ymax=media + 1.96*sderr),width = 0.2,position =pd) +
    facet_wrap(~`get(agrupapor)`)+
    geom_point(position=pd)+
    geom_line(position = pd)+
  
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
creaboxplot <- function(Datos, var_x,var_y,Titulo,xlabel="NA",ylabel="NA",var_color="NA"){
  ggplot(data = Datos) +
    geom_boxplot(mapping = aes(
      x = reorder(get(var_x),get(var_y),FUN = median),
      y = get(var_y),
      fill=get(var_color)
    )) +
    #geom_jitter(mapping = aes(x=get(var_x),y=get(var_y),color=get(var_color)) ,position=position_jitter(0.2))+
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

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}


###
# Seteo el Directorio....
##
setwd("~/Analiza tus Datos/Etapa 2/Bloque 5 Analiza tus Datos/4. Anova DOS o mas Factores una medida")

####
# Cargo la data
####
setwd("~/Analiza tus Datos/Etapa 2/Bloque 5 Analiza tus Datos/3. Relaciona Medidas de Correlacion")
df <- read_excel("espalda.xlsx")

df <- df[,c(4,5,9)] #Selecciono  Peso, Altura y OdiMes 0



###
# Se grafica el matrix plot para describir graficamente la forma de la distribucion, coeficiente de correlacion
###

# Graficamos Matrix Plot para ver Coeficiente de Correlacion  PEARSON
pairs.panels(df,method = "pearson",main="Coeficiente Pearson")

# Graficamos Matrix Plot para ver Coeficiente de Correlacion  Spearman
pairs.panels(df,method = "spearman",main="Coeficiente Spearman")


###
# Se muestran las matrices de correlacion y p-valor
##
mtcorPearson <- rcorr(as.matrix(df),type = "pearson") ##Pearson


mtcorSpearman <- rcorr(as.matrix(df),type = "spearman") ##Spearman

#Matrix de Correlacion y Pvalor en forma de tabla "PEARSON"
flattenCorrMatrix(mtcorPearson$r, mtcorPearson$P)

#Matriz de Correlacion y Pvalor  en forma de tabla "Spearman"
flattenCorrMatrix(mtcorSpearman$r,mtcorSpearman$P)


##Valido la Normalidad  PESO de las Variables par decidir si me quedo con Pearson o Spearman
shapiro.test(df$Peso)


##Valido la Normalidad  ALTURA de las Variables par decidir si me quedo con Pearson o Spearman
shapiro.test(df$Altura)



#####
##  en esta seccion procedo a buscar si hay asociacion
##  Entre diff_odi y NHD.
##  Al ser una comparacion entre una medida y un variable ordina
##  debo usar el coeficiente de spearman (no parametrico)
####

setwd("~/Analiza tus Datos/Etapa 2/Bloque 5 Analiza tus Datos/3. Relaciona Medidas de Correlacion")
df <- read_excel("espalda.xlsx")
df$diff_oddi <-  df$`ODI Mes0` - df$`ODI Mes1` #calculo diff odi
df$logdiff_oddi <- log(df$diff_oddi) ## Uso Logaritmo para acercar las escalas

df$NHD <-  factor(df$NHD) ##convierto a factor la variabla NHD Variable ordinal

##Diagrama de Error diff odi sin logaratimo
dfdiffodi <-  df[c(8,12)]
ggline(data=dfdiffodi,x=names(dfdiffodi)[1],y=names(dfdiffodi)[2],
       add = c("mean_ci","jitter"),
       palette = "jco") +
       ggtitle("Diagrama Error DiffOddi vs NHD")


##Diagrama de Error diff odi con logaratimo
dfdiffodi <-  df[c(8,13)]
ggline(data=dfdiffodi,x=names(dfdiffodi)[1],y=names(dfdiffodi)[2],
       add = c("mean_ci","jitter"),
       palette = "jco") +
  ggtitle("Diagrama Error LOG DiffOddi vs NHD")

###Valido la Normalidad de diff_oddi
shapiro.test(df$diff_oddi)


###Procedo a calcular el coefeciente de correlacion de spearman
###diffoddi vs NHD (ORDINAL)
dfdiffodi <-  df[c(8,13)]
mtcorSpearman <- rcorr(as.matrix(dfdiffodi),type = "spearman") ##Spearman

#Matriz de Correlacion y Pvalor  en forma de tabla "Spearman"
flattenCorrMatrix(mtcorSpearman$r,mtcorSpearman$P)


