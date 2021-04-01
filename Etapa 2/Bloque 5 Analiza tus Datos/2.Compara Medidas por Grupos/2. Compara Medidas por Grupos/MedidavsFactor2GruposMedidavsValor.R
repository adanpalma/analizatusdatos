#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
#********************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("FSA","car","plotly","ggplot2", "plotly", "xlsx","scales","stringr","readr","dplyr","psych","readxl","ggpubr","PerformanceAnalytics")

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
setwd("~/Analiza tus Datos/Etapa 2/Bloque 5 Analiza tus Datos/Comprobar Restricciones")

####
# Cargo la data
####

df <- espalda <- read_excel("espalda.xlsx")

str(df)

###
# Preparo datos, creo columnas, grupos etc
###
df$diffodi  <- df$`ODI Mes0` - df$`ODI Mes1` # agrego  variable con la mejoria
df$NHD[df$NHD > 0] <- 1 # si tiene mas de 1 hernia las hago todas 1 eso indica que tiene hernias

#creo factores 
df$NHD <- factor(df$NHD,labels  = c("Sin Hernia","Con Hernia")) #Hernias
df$Grupo <- factor(df$Grupo,labels = c("Convencional","Experimental")) #Tipo de tratamiento



###
# Descriptiva HISTOGRAMAS, QQPLOT, BOXPLOTS
###

# 1 DESCRIPCI?N 

#Valido que me
# boxplot, histograma y qqplot


#DESCRIPCION DE VARIABLE ODIMES 0
dfvectodi0 <- pull(df,`ODI Mes0`) #Con pull convierto el tibble en VECTOR porque si no da error
hist(dfvectodi0,breaks = 20,freq = FALSE, main = "Distribución Frecuencia ODIMES 0",xlab = "Odimes 0",ylab = "Cantidad")
boxplot(dfvectodi0, main="Variable ODI MES 0") 
library(car)
# qqplot paquete car
qqPlot(dfvectodi0,main="Variable ODI MES 0")

#DESCRIPCION DE VARIABLE ODIMES 1
dfvectodi1 <- pull(df,`ODI Mes1`) #Con pull convierto el tibble en VECTOR porque si no da error
hist(dfvectodi1,breaks = 20,freq = FALSE, main = "Distribución Frecuencia ODIMES 1",xlab = "Odimes 0",ylab = "Cantidad")
boxplot(dfvectodi1, main="Variable ODI MES 1") 
library(car)
# qqplot paquete car
qqPlot(dfvectodi1,main="Variable ODI MES 1")



# Histograma de frecuencias, box plot y qqplot con plotly ODI MES 0
name1 = names(df)[9]
p1<-plot_ly(x = dfvectodi0 , type = "histogram",name = name1,marker=list(color='#FF6666'))%>%
  layout(title = paste("Histograma de Frecuencias de",name1),yaxis = list(title = name1),bargap=0.005*(max(df[,name1])-min(df[,name1])))

# Boxplot
p2<-plot_ly(y = dfvectodi0 , type = "box",name = name1,boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1),yaxis = list(title = name1))

# QQplot

p3<-ggqqplot(df , x = name1,color = "#FF6666",add.params = list(color = "black"))+
  xlab("Distribuci?n Te?rica Normal") + ylab("Cuartiles reales") +
  theme_minimal() +
  ggtitle(paste("QQ-plot de ", name1,sep = "")) +
  theme(plot.title = element_text(hjust = 0.5))
p3 <- ggplotly(p3)
p <- subplot(p1, p2, p3)%>%
  layout(title = paste("Histograma de Frecuencias Boxplot y QQplot para la variable  ", name1,sep = ""))
p



# Histograma de frecuencias, box plot y qqplot con plotly ODI MES 1
name1 = names(df)[10]
p1<-plot_ly(x = dfvectodi1 , type = "histogram",name = name1,marker=list(color='#FF6666'))%>%
  layout(title = paste("Histograma de Frecuencias de",name1),yaxis = list(title = name1),bargap=0.005*(max(df[,name1])-min(df[,name1])))

# Boxplot
p2<-plot_ly(y = dfvectodi1 , type = "box",name = name1,boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1),yaxis = list(title = name1))

# QQplot

p3<-ggqqplot(df , x = name1,color = "#FF6666",add.params = list(color = "black"))+
  xlab("Distribuci?n Te?rica Normal") + ylab("Cuartiles reales") +
  theme_minimal() +
  ggtitle(paste("QQ-plot de ", name1,sep = "")) +
  theme(plot.title = element_text(hjust = 0.5))
p3 <- ggplotly(p3)
p <- subplot(p1, p2, p3)%>%
  layout(title = paste("Histograma de Frecuencias Boxplot y QQplot para la variable  ", name1,sep = ""))
p



####
#  Compruebo Normalidad de ODIMES 0 y ODIMES 1 usando SHAPIRO-WILK (5000 o menos observaciones)
###
x.test <- shapiro.test(dfvectodi0) #ODI MES 0
print(x.test)


x.test <- shapiro.test(dfvectodi1) #ODI MES 1
print(x.test)




#####
# Comprobar test de igualdad de varianzas por grupos de tratamiento
#####

# BoxPlot por grupo de tratamiento para la variable continua Mejoria
# Boxplot con puntos con plotly
name1 <-  names(df)[12]
name2 <-  names(df)[11]

creaboxplot(df,name2,name1,"Box Plot por Tratamiento","Mejoria","Tratamiento")
crea_errdiagram(df,name1,name2,"Box Plot por Tratamiento","Tratamiento","Mejoria")

##
# Se hace una descriptiva por Grupo de tratamiento
##
Summarize(diffodi ~ Grupo, 
          data=df)



####
# REALIZO EL TEST DE LEVENE PARA VALIDAR IGUALDAD DE VARIANZAS
###

leveneTest(df$diffodi ~ df$Grupo,Data = df)





####
# Seccion de Comparancio de Medidas
###


setwd("~/Analiza tus Datos/Etapa 2/Bloque 5 Analiza tus Datos/2. Compara Medidas por Grupos/[AD]2.1_HT_Comprarmedida2grupos1medida1valor")

#abro archivo
df <- read_excel("espalda.xlsx")

df$diff_odi <-  df$`ODI Mes0` - df$`ODI Mes1` #calculo diff odi

df$ftratamiento <-  factor(df$Grupo,labels = c("Convencional","Innovador")) # Factor del grupo de tratamiento

# Grafico boxplot, histogramas para ver distribucion de los datos

#DESCRIPCION DE VARIABLE ODIMES 0
hist(df$`ODI Mes0`,breaks = 20,freq = FALSE, main = "Distribución Frecuencia ODIMES 0",xlab = "Odimes 0",ylab = "Cantidad")
boxplot(df$`ODI Mes0` ,main="Variable ODI MES 0") 
library(car)
# qqplot paquete car
qqPlot(df$`ODI Mes0`,main="Variable ODI MES 0")

#DESCRIPCION DE VARIABLE ODIMES 1
hist(df$`ODI Mes1`,breaks = 20,freq = FALSE, main = "Distribución Frecuencia ODIMES 1",xlab = "Odimes 0",ylab = "Cantidad")
boxplot(df$`ODI Mes1`, main="Variable ODI MES 1") 
library(car)
# qqplot paquete car
qqPlot(df$`ODI Mes1`,main="Variable ODI MES 1")


# Histograma de frecuencias, box plot y qqplot con plotly ODI MES 0


dfvectodi0 <- pull(df,`ODI Mes0`) #convierto a vector ya q tibble da error en plot_ly
name1 = names(df)[9]
p1<-plot_ly(x = dfvectodi0 , type = "histogram",name = name1,marker=list(color='#FF6666'))%>%
  layout(title = paste("Histograma de Frecuencias de",name1),yaxis = list(title = name1),bargap=0.005*(max(df[,name1])-min(df[,name1])))

# Boxplot
p2<-plot_ly(y = dfvectodi0 , type = "box",name = name1,boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1),yaxis = list(title = name1))

# QQplot

p3<-ggqqplot(df , x = name1,color = "#FF6666",add.params = list(color = "black"))+
  xlab("Distribuci?n Te?rica Normal") + ylab("Cuartiles reales") +
  theme_minimal() +
  ggtitle(paste("QQ-plot de ", name1,sep = "")) +
  theme(plot.title = element_text(hjust = 0.5))
p3 <- ggplotly(p3)
p <- subplot(p1, p2, p3)%>%
  layout(title = paste("Histograma de Frecuencias Boxplot y QQplot para la variable  ", name1,sep = ""))
p



# Histograma de frecuencias, box plot y qqplot con plotly ODI MES 1
name1 = names(df)[10]
dfvectodi1 <- pull(df,`ODI Mes1`) #Transformo a vector por que el tibble da problemas en el plotly
p1<-plot_ly(x =dfvectodi1, type = "histogram",name = name1,marker=list(color='#FF6666'))%>%
  layout(title = paste("Histograma de Frecuencias de",name1),yaxis = list(title = name1),bargap=0.005*(max(df[,name1])-min(df[,name1])))

# Boxplot
p2<-plot_ly(y = dfvectodi1 , type = "box",name = name1,boxpoints = "all")%>%
  layout(title = paste("Boxplot de",name1),yaxis = list(title = name1))

# QQplot

p3<-ggqqplot(df , x = name1,color = "#FF6666",add.params = list(color = "black"))+
  xlab("Distribuci?n Te?rica Normal") + ylab("Cuartiles reales") +
  theme_minimal() +
  ggtitle(paste("QQ-plot de ", name1,sep = "")) +
  theme(plot.title = element_text(hjust = 0.5))
p3 <- ggplotly(p3)
p <- subplot(p1, p2, p3)%>%
  layout(title = paste("Histograma de Frecuencias Boxplot y QQplot para la variable  ", name1,sep = ""))
p


####
#  Ahora procedo a ver las distribuciones de diff odi por grupo de tratamiento Histograma,boxplot,qqplot
###


name1 = names(df)[11]
name2 = "Tratamiento"
dfvectdiff <- pull(df,diffodi) #Transformo a vector por que el tibble da problemas en el plotly
p1 <- ggplot(df, aes(x = dfvectdiff, colour = ftratamiento)) +
      geom_density() + geom_rug() +
      geom_histogram(aes(y = ..density.., fill = ftratamiento), alpha = 0.2, bins = 50)
p1

p2 <- plot_ly(y=dfvectdiff,x=df$ftratamiento,type = 'box',name=name1,boxpoints="all", color = df$ftratamiento) %>% 
  layout(title = paste(" Boxplot  para la variable  ", name1,"en relacion al tipo",name2,sep = ""))
p2

p3 <- crea_errdiagram(df,"diffodi","ftratamiento","Diagrama de Error Mejoria por Tratamiento", "Tratmiento","Mejoria")
p3



###
# Procedo a validar la validar NORMALIDAD E IGUALDAD DE VARIANZAS DEL ODIMES 0 Y ODI MES 1 PARA TODOS LOS TRATAMIENTOS
###

shapiro.test(df$`ODI Mes0`)
shapiro.test(df$`ODI Mes1`)


#Valido Normalidad de OdiMes 0 y odiMes 1 para grupo de tratamiento convencional

shapiro.test(df$`ODI Mes0`[df$Grupo == 0])
shapiro.test(df$`ODI Mes1`[df$Grupo == 0])



#Valido Normalidad de OdiMes 0 y odiMes 1 para grupo de tratamiento Innovador

shapiro.test(df$`ODI Mes0`[df$Grupo == 1])
shapiro.test(df$`ODI Mes1`[df$Grupo == 1])


# Valido diffodd por grupo de tratameiento
shapiro.test(df$diff_odi[df$Grupo == 0])
shapiro.test(df$diff_odi[df$Grupo == 1])

#Valido la igualdad de Varianzas
leveneTest(df$diff_odi ~ df$ftratamiento,Data=df)


#Se comprueba si el diff odi es igual por grupo de tratamiento o difieren 
# PARA GRUPOS INDEPENDIENTES, NORMALIDAD Y   VARIANZAS IGUALES
t.test(diff_odi ~ ftratamiento,data=df,var.equal = TRUE) #var.equal = TRUE varianzas iguales


#Si HUBIESEN SIDO NORMALES PERO NO HAY IGUALDAD DE VARIANZAS SIENGO GRUPOS INDEPENDIENTES
#ENTONCES SE CORRIGUE CON WELCH USANDO VAR.EQUAL = FALSE
t.test(diff_odi ~ ftratamiento,data=df,var.equal = FALSE) #var.equal = FLASE varianzas DISTINTAS


#Si NO SON NORMALES ENTONCES USO TEST NO PARAMETRICO 
#PARA GRUPOS INDEPENDIENTES PERO NO NORMALES 
wilcox.test(diff_odi ~ ftratamiento,data=df)




###
# Seccion de Comparar 1 medida con un Valor
# En este caso comparar diff odi mayor a un valor critico = 20
##

valorcritico <- 20 

# Se inicia con la vista de las distribuciones de frecuencias y boxplots de la diffodi para todos y por
#tipo de tratamiento

vdiffoddi <- df[,12]
names(vdiffoddi) <- "Mejoria"

vmejoria <- cbind(vdiffoddi,df$ftratamiento)
names(vmejoria)[2] <- "Tratamiento"


###Graficamos BoxPlot e Histgrama para 

name1 <- names(vmejoria)[1]
x1 <- vmejoria[,name1]
n <- length(vmejoria)

ggplot(vmejoria,aes(x=vmejoria[,1])) +
  geom_histogram(colour="black",fill="#FF6666") +
  geom_vline(aes(xintercept = mean(valorcritico, na.rm=T, colour="Mean")),
             color="red",linetype="dashed",size=1) +
  xlab("Rango de Clases") + 
  ylab("Frecuencia Absoluta") +
  theme_minimal()+
  ggtitle(paste("Histograma de Frencuencias \n Todos Tratamientos",name1,sep=" "))+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual("Legend title", values=c("black","red"))




###Graficamos BoxPlot e Histgrama  Trataamiento Grupo0 y Grupo 1

name1 <- names(vmejoria)[1]
df1 <- vmejoria[vmejoria[,2]=="Convencional",]
n <- length(df1)
n

ggplot(df1,aes(x=df1[,1])) +
  geom_histogram(colour="black",fill="#FF6666") +
  geom_vline(aes(xintercept = mean(valorcritico, na.rm=T, colour="Mean")),
             color="red",linetype="dashed",size=1) +
  xlab("Rango de Clases") + 
  ylab("Frecuencia Absoluta") +
  theme_minimal()+
  ggtitle(paste("Histograma de Frencuencias \n Tratamiento Convencional",name1,sep=" "))+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual("Legend title", values=c("black","red"))
  


###Graficamos BoxPlot e Histgrama  Trataamiento Grupo0 y Grupo 1

name1 <- names(vmejoria)[1]
df1 <- vmejoria[vmejoria[,2]=="Innovador",]
n <- length(df1)


ggplot(df1,aes(x=df1[,1])) +
  geom_histogram(colour="black",fill="#FF6666") +
  geom_vline(aes(xintercept = mean(valorcritico, na.rm=T, colour="Mean")),
             color="red",linetype="dashed",size=1) +
  xlab("Rango de Clases") + 
  ylab("Frecuencia Absoluta") +
  theme_minimal()+
  ggtitle(paste("Histograma de Frencuencias \n Tratamiento  Innovador",name1,sep=" "))+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual("Legend title", values=c("black","red"))

creaboxplot(df,"ftratamiento","diff_odi","Box Plot DiffOdi por Grupo","Tratamientos","Mejoria")


#Valido la Normalidad delos Datos para Tratamiento Convencional y Tratamiento Innovador

dfCon <- vmejoria[vmejoria[,2]=="Convencional",]
dfInn <- vmejoria[vmejoria[,2]=="Innovador",]

shapiro.test(dfCon$Mejoria) #Grupo Convencional
shapiro.test(dfInn$Mejoria) #Grupo Innovador


###Test de wilcoxon porque los datos no siguen una distrubucion normal
wilcox.test(dfCon$Mejoria,mu = valorcritico) #Convencional 
mean(dfCon$Mejoria) #Convencional



###Test de wilcoxon porque los datos no siguen una distrubucion normal
wilcox.test(dfInn$Mejoria,mu = valorcritico) #Innovador 
mean(dfInn$Mejoria) #Innovador


###
# Parte 3 analizando grupos Dependientes
###

#Preparo la Data
dfconvencional <- df[df$ftratamiento == "Convencional",]
dfodimes0 <- cbind(dfconvencional[,9],rep("Mes 0",length(dfconvencional[,9])))
names(dfodimes0) <- c("Odi","Mes")
dfodimes1 <- cbind(dfconvencional[,10],rep("Mes 1",length(dfconvencional[,10])))
names(dfodimes1) <- c("Odi","Mes")
df1 <- rbind(dfodimes0,dfodimes1)
df1$Mes <- as.factor(df1$Mes)

creaboxplot(df1,"Mes","Odi","Box Plot Odi Mes 0 y Mes 1 para Convencional","Mes","Odi")
crea_errdiagram(df1,"Odi","Mes","Diagrama de Error Convencional","Mes","Odi")

shapiro.test(df1$Odi[df1$Mes=="Mes 0"]) ## Normalidad Mes 0
shapiro.test(df1$Odi[df1$Mes=="Mes 1"]) ## Normalidad Mes 1
shapiro.test(df1$Odi) ## Se hace para todo el grupo de Odi para Convencional (mes 0 y Mes 1)

leveneTest(df1$Odi ~ df1$Mes, Data = df1)

wilcox.test(df1$Odi ~ df1$Mes, Data=df1, paired = TRUE) ##TEST DE WILCOXON PAREADO GRUPOS DEPENDIENTE NO NORMALES



















