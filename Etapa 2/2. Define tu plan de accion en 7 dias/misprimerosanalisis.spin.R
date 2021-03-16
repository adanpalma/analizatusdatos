#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
#********************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("plotly","ggplot2", "plotly", "xlsx","scales","stringr","readr","dplyr","psych","readxl","ggpubr")


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
# Seteo el Directorio....
##
setwd("C:/Users/apalmad/Desktop/Analiza tus Datos/Bloque 2/2. Define tu plan de accion en 7 dias/Data")

####
# Cargo la data
####
df <- read_excel("datosprestamos.xlsx",col_types = c("text", 
                                                          "text", "text", "text", "numeric", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "date", "date", "numeric", 
                                                          "text", "text", "text", "numeric", "text", 
                                                          "text", "text", "text", "text", "numeric", 
                                                          "text", "text"))
str(df)

###
# Convierto HIPOTECARIO a HipotecA y creo grupos por monto cxc
###
unique(df$Producto)
df$Producto[df$Producto == "HIPOTECARIO"] <- "HIPOTECA" 
df$Saldoprestamo[df$Pais == "CFC"] <- df$Saldoprestamo[df$Pais == "CFC"] / 3800
df$`Monto CXC`[df$Pais == "CFC"] <- df$`Monto CXC`[df$Pais == "CFC"] / 3800
df$Saldoprestamo <- round(df$Saldoprestamo,2)

df$rangocxc   <- cut(df$`Monto CXC`,breaks = c(0,500,1000,1500,2000,2500,3500,Inf), labels=c("500 o menos","500 a 1000","1000 a 1500","1500 a 2000","2000 a 2500","2500 a 3500", "3500 en adelante"))
df$rangosaldo <- cut(df$Saldoprestamo,breaks = c(0,20000,40000,65000,85000,100000,120000,Inf), labels=c("20K o menos","20K a 40K","40K a 65K","65K a 85K","85K a 100K","100K a 120K", "120K +"))

unique(df$Morosidadcredito)



# para que no muestre notacion cientifica en y axis o x axis
options(scipen=5000)

#Tile Cantidad Sector vs Riesgo

df%>%
  filter(MODIFICADO =="MOD" & Fechasistema == "202012" & !is.na(RiesgoEmp1)) %>% 
  count(RiesgoEmp1,SectorEmp1) %>% 
  ggplot(mapping = aes(x=RiesgoEmp1, y=SectorEmp1)) +
  geom_tile(mapping= aes(fill=n))+
  labs(title = "Cantidad Creditos Sector vs Riesgo - Modificados",
       x="Riesgo", y= "",size="1") +
    theme(axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain")) 


# df1 dataframe que solo contiene los Aliviados al dic 31 
df1 <-   filter(df,MODIFICADO =="MOD" & Fechasistema == "202012" & !is.na(RiesgoEmp1) &!is.na(Morosidadcredito)) 

print("**** Distribucion por Saldos por Riesgo Empleador 1 con algun tipo de Alivio****")
format(tapply(df1$Saldoprestamo, df1$RiesgoEmp1, sum),nsmall=1,big.mark = ",")


#GeomPoint de SaldosPrestamos por Nivel de Riesgo Emp1 por Pais
ggplot(filter(df, !is.na(RiesgoEmp1) & RiesgoEmp1 != "NULL")) +
  geom_point(mapping=aes(x=`Monto CXC`, y=Saldoprestamo,color=RiesgoEmp1)) +
  xlim(0,3000) +
  ylim(5000,100000) +
  coord_flip()+
  facet_grid(Producto ~ Pais)

#GeomPoint de SaldosPrestamos por Nivel de Riesgo Emp2 por Pais
ggplot(filter(df, !is.na(RiesgoEmp2) & RiesgoEmp2 != "NULL")) +
  geom_point(mapping=aes(x=`Monto CXC`, y=Saldoprestamo,color=RiesgoEmp2)) +
  xlim(0,3000) +
  facet_grid(Producto ~ Pais)


#Geom Cunt que muestra los sectores calsificados por riesgo Juicio Experto
p <-   ggplot(df1,mapping = aes(x=RiesgoEmp1, y=SectorEmp1)) +
  geom_count(alpha=0.5) +
  labs(title = "Relacion Sector vs Riesgo",
    x="Riesgo", y= "",size="1") +
  theme(axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
        legend.text  = element_text(colour="black", size = 6, face = "bold")
        ) 
  ggplotly(p)


#Box Plot que muestra la media de Saldos de PRestamos por MOrosidad  
  p <- ggplot(df1) +
    geom_boxplot(mapping = aes(x=Morosidadcredito,y=Saldoprestamo))+
    facet_wrap(. ~ Producto) +
    ylim(5000,65000)
  ggplotly(p)
  
# 
  
  
  # Geom Bar agrupado por Producto y Morosidad del credito por pais  
  p <- ggplot(filter(df,df$Fechasistema == "202012" & !is.na(Morosidadcredito) & Morosidadcredito != "NULL" &  RiesgoEmp1 %in% c("Alto","Medio","Bajo")),aes(x=RiesgoEmp1,y=(Saldoprestamo),fill=Morosidadcredito)) +
    geom_bar(position = "fill",stat="identity") +
    labs(title = "BoxPlot apiladopor Morosidad Panama",
         x="", y="Sector",size="1") +
    facet_wrap(Pais~Producto ) 
  ggplotly(p)
  
  
  
    # Geom Bar agrupado por Producto y Morosidad del credito  PANAMA
  p <- ggplot(filter(df,Pais=="PAN" & df$Fechasistema == "202012" & !is.na(Morosidadcredito) & Morosidadcredito != "NULL" ),aes(x=SectorEmp1,y=(Saldoprestamo),fill=Morosidadcredito)) +
    geom_bar(position = "fill",stat="identity") +
    #facet_wrap(~Producto ) +
    labs(title = "Saldos por Sector",
         x="", y="Sector",size="1") +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_text(color = "black", size = 6, angle = 90, hjust = 1, vjust = 45, face = "plain"),
          axis.text.y = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
          legend.text  = element_text(colour="black", size = 6, face = "bold")
    ) +
    coord_flip()
  ggplotly(p)
  
  
  # Geom Bar agrupado por Producto y Morosidad del credito COLOMBIA  
  p <- ggplot(filter(df,!is.na(SectorEmp1) &  Pais=="CFC" & df$Fechasistema == "202012" & !is.na(Morosidadcredito) & Morosidadcredito != "NULL" ),aes(x=SectorEmp1,y=(Saldoprestamo),fill=Morosidadcredito)) +
    geom_bar(position = "fill",stat="identity") +
  #  facet_wrap(~Producto ) +
    labs(title = "Saldos por Sector",
         x="", y="Sector",size="1") +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_text(color = "black", size = 6, angle = 90, hjust = 1, vjust = 45, face = "plain"),
          axis.text.y = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
          legend.text  = element_text(colour="black", size = 6, face = "bold")
    ) +
    coord_flip()
  ggplotly(p)
  
  # Geom Bar agrupado por Producto y Morosidad del credito EL SALVADOR  
  p <- ggplot(filter(df,!is.na(SectorEmp1) &  Pais=="SAL" & df$Fechasistema == "202012" & !is.na(Morosidadcredito) & Morosidadcredito != "NULL" ),aes(x=SectorEmp1,y=(Saldoprestamo),fill=Morosidadcredito)) +
    geom_bar(position = "fill",stat="identity") +
    #facet_wrap(~Producto ) +
    labs(title = "Saldos por Sector",
         x="", y="Sector",size="1") +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_text(color = "black", size = 6, angle = 90, hjust = 1, vjust = 45, face = "plain"),
          axis.text.y = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
          legend.text  = element_text(colour="black", size = 6, face = "bold")
    ) +
    coord_flip()
  ggplotly(p)
  

  
  #boxplot Cantidad Sector vs Riesgo
  df1 <-   filter(df,MODIFICADO =="MOD" & Fechasistema == "202012" & !is.na(RiesgoEmp1)) 
  
  p <-   ggplot(df1) +
         geom_boxplot(
           mapping= aes(
             x=reorder(df1$SectorEmp1,`Monto CXC`,FUN=median),
             y=`Monto CXC`)
         )+
        coord_flip()+
    labs(title = "Sectores CXC",
         x="Riesgo", y= "",size="1") +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_text(color = "black", size = 6, angle = 90, hjust = 1, vjust = 45, face = "plain"),
          axis.text.y = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
          legend.text  = element_text(colour="black", size = 6, face = "bold")
    ) +
    facet_wrap( ~ Producto)
  ggplotly(p)

#########
# Relacion Cantidad de Prestamos vs Rango CXC GEOM COUNT
#########  
  
  
  p <-   ggplot(df1,mapping = aes(x=RiesgoEmp1, y=rangocxc)) +
    geom_count(alpha=0.5) +
    labs(title = "Relacion Riesgo vs CXC \n",
         x="Riesgo", y= "",size="1") +
    theme(axis.ticks.x = element_blank(),
          axis.text.y = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"))  
    #scale_colour_manual(values = lacroix_palette("PassionFruit",n=20)) +
    #facet_wrap(Producto ~ RiesgoEmp1)
  ggplotly(p)
  
###
# Relación entre Producto vs Riesgo por Rango de Saldo GEOM COUNT
###  
  p <-   ggplot(df1,mapping = aes(x=RiesgoEmp1, y=rangosaldo)) +
    geom_count(alpha=0.5) +
    labs(title = "Relacion Sector vs Riesgo por Saldo \n",
         x="Riesgo", y= "",size="1") +
    theme(axis.text.x = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
          legend.text  = element_text(colour="black", size = 10, face = "bold"),
          legend.position = "top",) + 
    #scale_colour_manual(values = lacroix_palette("PassionFruit",n=20)) +
    facet_wrap( Producto ~ RiesgoEmp1)
  ggplotly(p)
  


  #Grafico de Barras evolucion Riesgos Feb2020 a Dic 2020
  df%>%
  filter(MODIFICADO =="MOD" & !is.na(RiesgoEmp1)) %>%
  ggplot(aes(x=Fechasistema,y=Saldoprestamo,fill=RiesgoEmp1)) +
  geom_bar(stat = "identity" ) +
  theme(axis.text.x = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
        legend.text  = element_text(colour="black", size = 6, face = "bold"),
        legend.position = "top") + 
    facet_wrap(.  ~ Producto)



options(scipen=5000)
###
# Trato de Compara Riesgo Emp1 vs Riesgo Emp2
###
df1 <-   filter(df,MODIFICADO =="MOD" & Fechasistema == "202012" & !is.na(RiesgoEmp1)) 
ggplot(df1,mapping = aes(x=Saldoprestamo, y=..density..)) +
  geom_freqpoly(mapping = aes(color=RiesgoEmp1),position = "identity",stat = "bin",binwith = 5000) +
  labs(title = "Histo Densidad Riesgo Emp1 \np",
       x="Riesgo", y= "",size="1") 
  

options(scipen=5000)

df1 <-   filter(df,MODIFICADO =="MOD" & Fechasistema == "202012" & !is.na(RiesgoEmp2)) 
ggplot(df1,mapping = aes(x=Saldoprestamo, y=..density..)) +
  geom_freqpoly(mapping = aes(color=RiesgoEmp2),binwith = 10000) +
  labs(title = "Histo Densidad Riesgo Emp2 \n",
       x="Riesgo", y= "",size="1") 

df1 <- filter(df,df$`Grupo Clientes` == "Dos Dedudores" & MODIFICADO =="MOD" & Fechasistema == "202012")
dfduplicadoxemp =rbind(df1,df1)

 
###
# Tablas de Contingencias
###
table(df1$RiesgoEmp1)
table(df1$RiesgoEmp2)
table(df1$RiesgoEmp1,df1$RiesgoEmp2)
prop.table(table(df1$RiesgoEmp1,df1$RiesgoEmp2),margin=1)*100
prop.table(table(df1$RiesgoEmp1,df1$RiesgoEmp2),margin=2)*100

###
# Test CHi Cuadrado Pearson para ver si las filas y las columnas son independientes
###
chisq.test(df1$RiesgoEmp1,df1$RiesgoEmp2)





