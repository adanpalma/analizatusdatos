#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
#********************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("gplots","corrplot" ,"foreign","Hmisc","lme4","lmerTest", "FSA","car","plotly","ggplot2", "plotly", "xlsx","scales","stringr","readr","dplyr","psych","readxl","ggpubr","PerformanceAnalytics")
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
setwd("~/Analiza tus Datos/Etapa 2/Bloque 5 Analiza tus Datos/4. Comparar Proporciones")
####
# Cargo la data en formato SPSS y la conveirto a Data Frame\
# SPSS
####
library(foreign)
df <- read.spss(file.choose(),to.data.frame = TRUE)

## Crear la tabla de contingencias para lf_mes0 vs grupo

tab_cont_lfc1 <- table(df[,c(14,11)])
dftabcontlfc1 <- data.frame(tab_cont_lfc1)
colnames(dftabcontlfc1) <- c("Nivel","Grupo","Frec")
View(dftabcontlfc1)

## Se pasa a Excel para calcular los valores esperados que sale de 
# VESPERADO F1.C1 = suma (toda la fila 1) * suma(toda la columna1) / suma(toda la tabla)
# VESPERADO F2.C2 = suma (toda la fila 1) * suma(toda la columna2) / suma(toda la tabla)
# ....
# VESPERADO FN,CN = SUMA(TODA LA FILA N) * SUMA (TODA LA COLUMNA N) / SUMA (TODA LA TABLA)


# HISTOGRAMA PARA MOSTRAR EL RECUENTO DE VALORES POR TIPO DE TRATAMIENTO

name1 = names(df)[14]
name2 = names(df)[11]

p <- ggplot(df,mapping=aes(df[,name1],..count..)) +
    geom_bar(aes(fill=df[,name2]),position = position_stack()) +
    xlab(name1) + ylab("Frecuencia Absoluta")+
    ggtitle(paste("Diagrama de Barras ",name1))+
    guides(fill=guide_legend(title=""))


###continuar con el calculo del chiq.test para determinar via el test de 
### chi cuadrado si hay o no diferencias significativa en las proporciones
chisq <- chisq.test(x=as.vector(df[,14]),y=as.vector(df[,11]),correct = FALSE)
chisq



#Cuando los datos el chi cuadrado  da un p valor pero con un warining
#es porque posiblemente los valores esperados son muy chicos y como
# chi cuadrado espera datos normales, y no lo son genera ese waring
#en tales caso puedes confirmar con fisher.test
fisher.test(x=as.vector(df[,14]),y=as.vector(df[,11]))




# PROCEDO A VER EL P-VALOR POR CADA UNA DE LAS FILAS DE LA VARIABLE LF_MES1
prop.test(x=c(0,24),n=c(101,99)) # MINIMA
prop.test(x=c(23,64),n=c(101,99)) # MODERADA
prop.test(x=c(51,11),n=c(101,99)) # INTENSA
prop.test(x=c(25,0),n=c(101,99)) # DISCAPACIDAD
prop.test(x=c(2,0),n=c(101,99)) # MAXIMA





#####
# Comparacion de una proporcion con una PROBABILIDAD CONOCIDA,valor critico (BONDAD DE AJUSTE)
# PARA EL CASO QUEREMOS VER SI POR EJEMPLO CASO DE RATONES CON CANCER ESPONTANEOS
# VER SI MACHOS Y HEMBRAS TIENEN UN 50% DE PROBABILIDAD (BONDAD DE AJUSTE

dfratones = data.frame()

#####
dfmouses <- data.frame(sexoraton = c("MACHOS","HEMBRAS"), CANCERESPONTANEO = c(95,65))
####
# COMPARO  LOS MACHOS CON CANCER ESPONTANEO VS BONDAD DE AJUSTE DEL 50%
# prop.test(valormachos, totalmuestra, p=bondadajuste,correct=FALSE) Si p-valor < 0.05 SON DIFERENTES AL 50%
# prop.test(valormachos, totalmuestra, alternative="greater", correct=FALSE) Si p-valor < 0.05 Indica que en efecto la probabilidad de machos con cancer son > 50%
####

##Busco ver si las probabilidades de Machos  son iguales o distintas
prop.test(x=c(95),n=c(160),p=0.5,correct = FALSE)
##Busco ver si las probabilidades de Machos de cancer espontaneo son > 5%
prop.test(x=95,n=160,p=0.5,correct = FALSE,alternative="greater")
prop.test(x=95,n=160,p=0.5,correct = FALSE,alternative="less")



###
#  Comparacion de dos proporciones para ver que tanta probabilidad tiene cada una 
#  Comparo a Machos y Hembas, con cancer espontaneo y veo las probabilidades
#  prop.test(x=c(machos,hembas),n=c(totaldatos,totaldatos),correct=FALSE)  SI SALEN < 0.05 LAS PROPORCIONES SON DIFERENTES Y PROBABILIDADES SON DIFERENTES
###

#Busco ver si las probabilidades de Machos  son iguales o distintas
prop.test(x=c(95,65),n=c(160,160),correct = FALSE)  # Si sale < 0.05  entonces las probabilidades de ambos grupos son distintas
##Busco ver si las probabilidades de Machos SEA mayor que las Hembras
prop.test(x=c(95,65), n=c(160,160),correct=FALSE, alternative ="greater") # Si sale < 0.05 indica que el grupo macho tiene mas probabilidad que las hembras
##Busco ver si las probabilidades de Machos SEA MENOR que las Hembras
prop.test(x=c(95,65), n=c(160,160),correct=FALSE, alternative ="less") # Si sale > 0.05 indica que no es menor sino mayor




###
# Parte 5, Asociacion de Proporciones para grpupos de control y lfmes1 y nhd
###

#Cargo la data a utilizar..

setwd("~/Analiza tus Datos/Etapa 2/Bloque 5 Analiza tus Datos/5. Relacion de Proporciones")
#Leo el archivo en sav SPSS que tiene la data para el ejercicio

df <- read.spss(file.choose(),to.data.frame = TRUE) # Leo el archivo en formato spss

##Selecciono las columnas para trabajar separadas por grupos de tratamientos
dfgest <- df[df$Grupo == "Estándar",c("Grupo","NHD","LF_Mes1")]
dfginv <- df[df$Grupo == "Investigación",c("Grupo","NHD","LF_Mes1")]

#CREO LA TABLA DE CONTINGENCIAS PARA TRATAMIENTO ESTANDARD
tcontiest =table(dfgest$LF_Mes1,dfgest$NHD)
tcontiest

# Creo Diagrama de Barras apreciar mejor la tabla de contingencias por Grupo Tratamiento
#Grupo Estandard
name1 <- names(dfgest)[3]
name2 <- names(dfgest)[2]
p2 <- ggplot(dfgest,aes(dfgest[,name1], ..count..))+
  geom_bar(aes(fill=dfgest[,name2]),position = "dodge")+
  ggtitle("Diagrama de Barra NHD y LFMes1 Grupo Estandar")+
  xlab("Tipo de Discapcidad") + ylab("Frecuencia Absoluta")

p2


#CREO LA TABLA DE CONTINGENCIAS PARA TRATAMIENTO INVESTIGACION
tcontiinv =table(dfginv$LF_Mes1,dfginv$NHD)
tcontiinv

# Creo Diagrama de Barras apreciar mejor la tabla de contingencias por Grupo Tratamiento
#Grupo Investigacion
name1 <- names(dfginv)[3]
name2 <- names(dfginv)[2]
p2 <- ggplot(dfginv,aes(dfginv[,name1], ..count..))+
  geom_bar(aes(fill=dfginv[,name2]),position = "dodge")+
  ggtitle("Diagrama de Barra NHD y LFMes1 Grupo Investigacion")+
  xlab("Tipo de Discapcidad") + ylab("Frecuencia Absoluta")

p2



#CALCULAR EL TEST DE CHI CUADRADO PEARSON SIN CORRECION (PARA SABER SI HAY RELACION)
#grupo tratamiento estandar SE LE PIDE SIMULAR EL P-VALOR ya que hay muy pocos valores y da warnigs
chisqgest <- chisq.test(as.vector(dfgest[,3]), as.vector(dfgest[,2]), correct = FALSE,simulate.p.value = TRUE ) # Sin correcion
chisqgest

#CALCULAR EL TEST DE CHI CUADRADO PEARSON SIN CORRECION  (PARA SABER SI HAY RELACION)
#grupo tratamiento INVESTIGACION SE LE PIDE SIMULAR EL P-VALOR ya que hay muy pocos valores y da warnigs
chisqginv <- chisq.test(as.vector(dfginv[,3]), as.vector(dfginv[,2]), correct = FALSE, simulate.p.value = TRUE) # Sin correcion
chisqginv


###Antes proceo a converti la tabla de contingencias en MATRICES 
###Matrix para poder usarla 
matgest <- as.data.frame.matrix(tcontiest) #matrix grupo estandard
matginv <- as.data.frame.matrix(tcontiinv) # Matriz grupo Investigacion


##Muestro una grafica llamada ggBALLONPLOT que muestra las relaciones
## de los grupos como una tabla de contingencias pero usando graficos

ggballoonplot(matgest,main="Grupo Estandard LF1 vs NHD")
ggballoonplot(matginv,main="Grupo Investigacion LF1 vs NHD")

#VALROES OBSERVADOS
chisqgest$observed# Valores OBSERVADOS del grupo Estandard
# VALROES OBSERVADOS
chisqginv$observed # Valores OBSERVADOS del grupo Investigacion

#VALORES ESPERADOS
round(chisqgest$expected,2)#Valores Esperados Grupo Estandard

#VALORES ESPERADOS
round(chisqginv$expected,2) #Valores Esperados Grupo Investigacion

library(corrplot) # Para dibujar los resiudos y ver quienes son mas fuertes

#RESIDUOS
round(chisqgest$residuals,2) # Grupo Estandard
corrplot(chisqgest$residuals,is.cor=FALSE,main=)

#Residuos 
round(chisqginv$residuals,0) #Grupo Investigacion
corrplot(chisqginv$residuals, is.corr = FALSE, main="Residuos Grupo Investigacion")

#CALCULO DE LA CONTIBUCION DE CADA CELDA PARA LOS GRUPOS
#GRUPO ESTANDARD
contri_gesta <-  100* chisqgest$residuals^2/chisqgest$statistic
round(contri_gesta,3) 
corrplot(contri_gesta,is.corr = FALSE)

#CALCULO DE LA CONTIBUCION DE CADA CELDA PARA LOS GRUPOS
#GRUPO INVESTIGACION
contri_inv <-  100 * chisqginv$residuals^2/chisqginv$statistic
round(contri_inv,3) 
corrplot(contri_inv,is.corr = FALSE)














