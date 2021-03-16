#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
#********************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("plotly","ggplot2", "plotly", "xlsx","scales","stringr","readr","dplyr","psych","readxl","ggpubr","PerformanceAnalytics")



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
    df <-   filter(Datos, (Datos[,colagruparpor] == str_trim(filtrar_por[g,1])) ) %>%
      dplyr::select(c(colagruparpor,columnas))
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
setwd("~/Analiza tus Datos/Bloque 2/2. Define tu plan de accion en 7 dias/Data")

####
# Cargo la data
####
df <- read_excel("datosprestamos.xlsx",col_types = c("text", 
                                                          "text", "text", "text", "numeric", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "date", "date", "numeric", 
                                                          "text", "text", "text", "numeric", "text", 
                                                          "numeric", "text", "text", "text", "numeric", 
                                                          "text", "text"))
str(df)

df <- filter(df, df$Tipocartera=="Cartera en Libros" | df$Tipocartera=="Cartera Administrada")

###
# Convierto HIPOTECARIO a HipotecA y creo grupos por monto cxc
###
unique(df$Producto)
df$Producto[df$Producto == "HIPOTECARIO" | df$Producto == "CONSUMO" ] <- "HIPOTECA" 
df$Saldoprestamo[df$Pais == "CFC"] <- df$Saldoprestamo[df$Pais == "CFC"] / 3800
df$`Monto CXC`[df$Pais == "CFC"] <- df$`Monto CXC`[df$Pais == "CFC"] / 3800
df$Saldoprestamo <- round(df$Saldoprestamo,2)
df$rangocxc   <- cut(df$`Monto CXC`,breaks = c(0,500,1000,1500,2000,2500,3500,Inf), labels=c("500 o menos","500 a 1000","1000 a 1500","1500 a 2000","2000 a 2500","2500 a 3500", "3500 en adelante"))
df$rangosaldo <- cut(df$Saldoprestamo,breaks = c(0,20000,40000,65000,85000,100000,120000,Inf), labels=c("20K o menos","20K a 40K","40K a 65K","65K a 85K","85K a 100K","100K a 120K", "120K +"))
df$`Monto CXC`[is.na(df$`Monto CXC`)] <- 0.00 
df$Mtototalmora[is.na(df$Mtototalmora)] <- 0.00

unique(df$Morosidadcredito)

###
# Descriptiva de Saldos Hipotecas y Personales
###

HIPO <- descriptiva(filter(df, Producto == "HIPOTECA" & !is.na(Saldoprestamo) & Fechasistema=="202012" & Saldoprestamo >= 10000),15,2)
HIPO

PERS <- descriptiva(filter(df, Producto == "PERSONAL" & !is.na(Saldoprestamo) & Fechasistema=="202012" & Saldoprestamo >= 1500),(15:15),2)
PERS

###
# Histograma de Distribución de Saldos HIPOTECARIOS
###
p <- ggplot(filter(df, Producto == "HIPOTECA" & !is.na(Saldoprestamo) & Fechasistema=="202012" & Saldoprestamo >= 10000),
      aes(x=Saldoprestamo))+
      geom_histogram() +
      facet_wrap(Pais~Producto ) +
      xlim(10000,75000)
ggplotly(p)

p <- ggplot(filter(df, Producto == "HIPOTECA" & !is.na(Saldoprestamo) & Fechasistema=="202012" & Saldoprestamo >= 10000),
            aes(y=Saldoprestamo,x=Producto))+
  geom_boxplot() +
  facet_wrap(~Pais) 
# ylim(10000,75000)
ggplotly(p)

###
# Histograma de Distribución de Saldos PERSONALES
###
p <- ggplot(filter(df, Producto == "PERSONAL" & !is.na(Saldoprestamo) & Fechasistema=="202012" & Saldoprestamo >= 1500),
            aes(x=Saldoprestamo))+
  geom_histogram() +
  facet_wrap(Pais~Producto ) +
  xlim(1500,20000)
  
ggplotly(p)

####
# Histogramas hechos con el plot_ly  YES
###

df %>% 
  filter(Producto=="HIPOTECA" & !is.na(Saldoprestamo) & Fechasistema=="202012" & Saldoprestamo >= 10000) %>% 
  group_by(Pais) %>% 
  do(p=plot_ly(., x=~Saldoprestamo, type = "histogram", name=~Pais,boxpoints="all")) %>% 
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE) %>% 
  layout(title="Distribuci?n de Saldos Hipotecarios Modificados DIC 2020")

df %>% 
  filter(Producto=="PERSONAL" & !is.na(Saldoprestamo) & Fechasistema=="202012" & Saldoprestamo >= 1500) %>% 
  group_by(Pais) %>% 
  do(p=plot_ly(., x=~Saldoprestamo, type = "histogram", name=~Pais,boxpoints="all")) %>% 
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE) %>% 
  layout(title="Distribuci?n de Saldos Personales Modificados DIC 2020")

###
# Box Plot con strip point por cada pais usando la liberaria plot_ly
###
df %>% 
  filter(Producto=="HIPOTECA" & !is.na(Saldoprestamo) & Fechasistema=="202012" & Saldoprestamo >= 10000) %>% 
  group_by(Pais) %>% 
  do(p=plot_ly(., x=~Saldoprestamo, type = "box", name=~Pais,boxpoints="all")) %>% 
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE)

df %>% 
  filter(Producto=="PERSONAL" & !is.na(Saldoprestamo) & Fechasistema=="202012" & Saldoprestamo >= 1500) %>% 
  group_by(Pais) %>% 
  do(p=plot_ly(., y=~Saldoprestamo, type = "box", name=~Pais,boxpoints="all")) %>% 
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE)  +
  coord_flip()
  

p <- ggplot(filter(df, Producto == "PERSONAL" & !is.na(Saldoprestamo) & Fechasistema=="202012" & Saldoprestamo >= 1500),
            aes(y=Saldoprestamo,x=Producto))+
  geom_boxplot() +
  facet_wrap(~Pais) 
ggplotly(p)


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


#GeomPoint de SaldosPrestamos por Nivel de Riesgo Emp1 por Pais    ***YES
ggplot(filter(df, !is.na(RiesgoEmp1) & RiesgoEmp1 != "NULL")) +
  geom_point(mapping=aes(x=`Monto CXC`, y=Saldoprestamo,color=RiesgoEmp1)) +
  xlim(0,3000) +
  ylim(5000,100000) +
  coord_flip()+
  facet_grid(Producto ~ Pais)

#GeomPoint de SaldosPrestamos por Nivel de Riesgo Emp2 por Pais ***yes
ggplot(filter(df, !is.na(RiesgoEmp2) & RiesgoEmp2 != "NULL")) +
  geom_point(mapping=aes(x=`Monto CXC`, y=Saldoprestamo,color=RiesgoEmp2)) +
  xlim(0,3000) +
  coord_flip() +
  facet_grid(Producto ~ Pais)


#Geom Cunt que muestra los sectores calsificados por riesgo Juicio Experto  ***yesd
p <-   ggplot(df1,mapping = aes(x=RiesgoEmp1, y=SectorEmp1)) +
  geom_count(alpha=0.5) +
  labs(title = "Relacion Sector vs Riesgo",
    x="Riesgo", y= "",size="1") +
  theme(axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
        legend.text  = element_text(colour="black", size = 6, face = "bold")
        ) 
  ggplotly(p)


  # Geom Bar agrupado por Producto y Morosidad del credito por pais  **yes
  p <- ggplot(filter(df,df$Fechasistema == "202012" & !is.na(Morosidadcredito) & Morosidadcredito != "NULL" &  RiesgoEmp1 %in% c("Alto","Medio","Bajo")),aes(x=RiesgoEmp1,y=(Saldoprestamo),fill=Morosidadcredito)) +
    geom_bar(position = "fill",stat="identity") +
    labs(title = "BoxPlot apiladopor Morosidad Panama",
         x="", y="Sector",size="1") +
    facet_wrap(Producto~Pais ) 
  ggplotly(p)
  
  
    # Geom Bar agrupado por Producto y Morosidad del credito  PANAMA **YES
  p <- ggplot(filter(df,Pais=="PAN" & df$Fechasistema == "202012" & !is.na(Morosidadcredito) & Morosidadcredito != "NULL" ),aes(x=SectorEmp1,y=(Saldoprestamo),fill=Morosidadcredito)) +
    geom_bar(position = "fill",stat="identity") +
    #facet_wrap(~Producto ) +
    labs(title = "Saldos por Sector PANAMA",
         x="", y="Sector",size="1") +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_text(color = "black", size = 6, angle = 90, hjust = 1, vjust = 45, face = "plain"),
          axis.text.y = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
          legend.text  = element_text(colour="black", size = 6, face = "bold")
    ) +
    coord_flip()
  ggplotly(p)
  
  
  # Geom Bar agrupado por Producto y Morosidad del credito  SAL **YES
    p <- ggplot(filter(df,Pais=="SAL" & df$Fechasistema == "202012" & !is.na(Morosidadcredito) & Morosidadcredito != "NULL" ),aes(x=SectorEmp1,y=(Saldoprestamo),fill=Morosidadcredito)) +
      geom_bar(position = "fill",stat="identity") +
      #facet_wrap(~Producto ) +
      labs(title = "Saldos por Sector El Salvador",
           x="", y="Sector",size="1") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_text(color = "black", size = 6, angle = 90, hjust = 1, vjust = 45, face = "plain"),
            axis.text.y = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
            legend.text  = element_text(colour="black", size = 6, face = "bold")
      ) +
      coord_flip()
    ggplotly(p)
    
  
  # Geom Bar agrupado por Producto y Morosidad del credito  CFC **YES
  p <- ggplot(filter(df,Pais=="CFC" & df$Fechasistema == "202012" & !is.na(Morosidadcredito) & Morosidadcredito != "NULL" ),aes(x=SectorEmp1,y=(Saldoprestamo),fill=Morosidadcredito)) +
    geom_bar(position = "fill",stat="identity") +
    #facet_wrap(~Producto ) +
    labs(title = "Saldos por Sector Colombia",
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


  # Geom Bar agrupado por Producto y Morosidad del credito  POR PAIS UN DEUDOR Y DOS DEUDORES
  p <- ggplot(filter(df, df$Fechasistema == "202012" & !is.na(Morosidadcredito) & Morosidadcredito != "NULL" ),
               aes(x=`Grupo Clientes`,y=(Saldoprestamo),fill=Morosidadcredito)) +
    geom_bar(position = "fill",stat="identity") +
    facet_wrap(~Pais ) +
    labs(title = "Morosidad Agrupada por Uno o Dos Deudores",
         x="", y="Sector",size="1") +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_text(color = "black", size = 6, angle = 90, hjust = 1, vjust = 45, face = "plain"),
          axis.text.y = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
          legend.text  = element_text(colour="black", size = 6, face = "bold")
    ) 
  ggplotly(p)
  
#########
# Relacion Cantidad de Prestamos vs Rango CXC GEOM COUNT **yes
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
    facet_grid( ~ Producto )
  ggplotly(p)
  


  #Grafico de Barras evolucion Riesgos Feb2020 a Dic 2020
  options(scipen=5000)
  df%>%
  filter(MODIFICADO =="MOD" & !is.na(RiesgoEmp1)) %>%
  ggplot(aes(x=Fechasistema,y=Saldoprestamo,fill=RiesgoEmp1)) +
  geom_bar(stat = "identity" ) +
  theme(axis.text.x = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
        legend.text  = element_text(colour="black", size = 6, face = "bold"),
        legend.position = "right") + 
    labs(title = "Crecimiento Modificados x Riesgo Feb y Dic 2020",
         x="Fechas", y= "",size="1") +
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


###
# Tablas de Frecuencias DE Riesgo de Empleador 1 y Riesgo de Empleador 2
###

install.packages('epiDisplay')
library(epiDisplay)

tbfreqEmpl1 <- tab1(df$RiesgoEmp1,cum.percent = TRUE,sort.group = "decreasing",main="Distribución Riesgo Empleador 1")
tbfreqEmpl1

tbfreqEmpl2 <- tab1(df$RiesgoEmp2,cum.percent = TRUE,sort.group = "decreasing",main="Distribución Riesgo Empleador 2")
tbfreqEmpl2

###
# Tabla de Contingencias Emp1 y Emp2}
###
install.packages("gmodels")
library(gmodels)
df <- filter(df,df$`Grupo Clientes`== "Dos Dedudores")
ct <- CrossTable(df$RiesgoEmp1, df$RiesgoEmp2, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

###
# Se realiza un Mosaic Plot para variables Cualitativas o Categoricas
###
install.packages("vcd")
library(vcd)
mosaic(ct$t, shade = TRUE)


###
# Correlación 
# Diagrama de Dispersion de dos dimensiones entre Variables SaldosMoroso vs CXC 
# agrupados por factor de riesgo grupo cliente (un deudor o dos deudores
# Facets de Pais y Producto
###
names(df)

df$Mtototalmora <-  ifelse(df$Mtototalmora > 0 ,log10(df$Mtototalmora),0)
df$`Monto CXC` <-  ifelse(df$`Monto CXC` > 0, log10(df$`Monto CXC`),0)
df$Mtototalmora[is.na(df$Mtototalmora)] <- 0.00
df$`Monto CXC`[is.na(df$`Monto CXC`)] <- 0.00
df$rangosaldo[is.na(dfcxcsmor$rangosaldo)] <- "20K o menos"
dfcxcsmor <-  df[c(2,4,8,21,25,26,29)]

filter(dfcxcsmor,!is.na(RiesgoEmp1) & RiesgoEmp1 != "NULL") %>%  
ggplot() +
  geom_point(mapping = aes(x=Mtototalmora,y=`Monto CXC`,color=RiesgoEmp1))+
  facet_wrap(Producto~Pais)


##
# Scatter Plot 2D agrupado por Grupo deudor y facet producto
##
df2 <-  filter(dfcxcsmor,!is.na(RiesgoEmp1) & RiesgoEmp1 != "NULL"  & "Monto CXC" > 1)
ggscatter(df2,x="Mtototalmora",y="Monto CXC", 
          color = "Grupo Clientes",cor.coef = TRUE, cor.method = "pearson",conf.int = TRUE,add = "reg.line") +
  facet_grid(Pais ~ Producto)

###
# Matrix Plot Iris sepal.length, sepal.width, petal.lenght, petal.width and 
# group by species
###

setwd("~/Analiza tus Datos/Bloque 2/Bloque 4 Describe tus datos/4. Comparacion de Medidas (dos Factores)")
df <- read_excel("espalda.xlsx")





panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Plot #2: same as above, but add loess smoother in lower and correlation in upper
pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="Iris Scatterplot Matrix")




# Seguna opci?n para costumizar el matrixplot solo muestra
# upperpanel del scatter plot y dentro del scatter 
# coloca el indice de correlacion
upper.panel<-function(x, y){
  points(x,y, pch=23)
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}
# crear el matrixplot
pairs(df, 
      upper.panel = upper.panel)

###
# Hoja de Trabajo de Correlaci?n
###
setwd("~/Analiza tus Datos/Bloque 2/Bloque 4 Describe tus datos/3. Describe la Correlacion")
df <- read_excel("espalda.xlsx")
df$diff_odi <- df$`ODI Mes0` - df$`ODI Mes1` #diff_odi es mejoria
df$NHD[df$NHD == 2] <- 1
df$LC[df$LC == 2] <- 1
df <- df[-1] # Quito la columna de id del paciente
cconti <- df[,c(2,3,4,5,7,10,11)]

#Diagrama de Corrleacion que coloca SCATTERPLOT, HISTOGRAMAS Y COEF CORRELACION..
chart.Correlation(cconti,histogram = TRUE,pch=19,method = "spearman")

# Plot #2: same as above, but add loess smoother in lower and correlation in upper
pairs(cconti, data=cconti,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="Matrix Plot Mixto")


cconti <- df[,c(2,5,7,10,11)]

## Matrix Plot Agrupado por Factor de Hernias Discales LumboCiatica
cconti$LC <- as.factor(cconti$LC)
pairs(cconti, data=cconti,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="Matrix Plot por Lumbociatica", col=c("green","red")[cconti$LC])
legend("topright",as.vector(unique(cconti$LC)),fill=c("green","red"))

## Matrix Plot Agrupado por Factor de Hernias Discales NHD
cconti$NHD <- as.factor(cconti$NHD)
pairs(cconti, data=cconti,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="Matrix Plot por Num de Hernias", col=c("green","red")[cconti$NHD])
legend("topright",as.vector(unique(cconti$NHD)),fill=c("green","red"))












#Matrix de Correlacion
rel <-  cor(cconti)
rel <- round(rel,2)
rel


##Matriz de Correlaci?n usando la liberaria Hmisc
install.packages("Hmisc")
library("Hmisc")
res2 <- rcorr(as.matrix(cconti))

resmatrix <-  as.matrix(res2$r)

pvalues <- as.matrix(res2$P)

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

fpanel <- flattenCorrMatrix(resmatrix, pvalues)



##
# Seccion de Comparar Medidas por (dos factores)
##
setwd("~/Analiza tus Datos/Bloque 2/Bloque 4 Describe tus datos/4. Comparacion de Medidas (dos Factores)")
df <- read_excel("espalda.xlsx")

df$LC2 <- df$LC
df$LC2[df$LC == 2] <- 1 ## Convierto 2 a 1 
df$diff_odi <- 0
df$diff_odi <- df$`ODI Mes0` - df$`ODI Mes1` #diff_odi es mejoria

# Resumen Numerico de diff_odi por grupo y tratamiento
res_numLC2 <- as.matrix(descriptiva(df,13,12)) #Columna 12 es LC2
res_numGrupo <- as.matrix(descriptiva(df,13,11)) # Columna 11 es Grupo
res_numNHD <- as.matrix(descriptiva(df,13,8))
 
#BOX PLOTS
df$fNHD <- as.factor(df$NHD) # Creo un Factor por Numero de Hernias para  LOS BOX PLOTS
df$LC2 <-  as.factor(df$LC2) # Factor por si grupo LC
df$Grupo <- as.factor(df$Grupo)


creaboxplot(df,'fNHD','diff_odi',"Mejoria por  cantidad Hernias Discales","Hernias","Mejoria")
creaboxplot(df,'Grupo','diff_odi',"Mejoria por Grupo","Grupo","Mejoria")
creaboxplot(df,'LC2','diff_odi',"Mejoria por LC","LC","Mejoria")

crea_errdiagram(df,'diff_odi','fNHD',"Diagrama de Medias o Errr x NoHernias",xlabel = "No Hernias",ylabel = "Mejoria")
crea_errdiagram(df,'diff_odi','Grupo',"Diagrama de Medias o Errr x Tipo tratamiento",xlabel = "Tipo Tratamiento",ylabel = "Mejoria")
crea_errdiagram(df,'diff_odi','LC2',"Diagrama de Medias o Errr x LC",xlabel = "LC",ylabel = "Mejoria")

##
# Histograma de Densidad por factores
##
df = mtcars[c(1,10)]
df[,2] = as.factor(df[,2])
names1 = names(df)[1]
names2 = names(df)[2]
ggplot(df,aes(df[,1], fill = df[,2] )) +
  geom_density(alpha=0.2) +
  xlab("Titulo x") + ylab("Titulo y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  guides(fill=guide_legend(title=""))


##
# Box Plot de dos factores
##
df = mtcars[c(1,8,9)]
str(df)
df[,2] = as.factor(df[,2])
df[,3] = as.factor(df[,3])
str(df)

ggplot(df, mapping=aes(x=df[,2],y=df[,1],fill=df[,3])) +
geom_boxplot() +
ggtitle("Titulo del Grafico")+
xlab(paste("Factor " , names(df)[2])) +
ylab(paste("Factor ", names(df)[1])) +
labs(fill = paste("Factor",names(df)[3]))



df <- read_excel("espalda.xlsx")

df$LC2 <- df$LC

df$LC2[df$LC == 2] <- 1 ## Convierto 2 a 1 
df$diff_odi <- 0
df$diff_odi <- df$`ODI Mes0` - df$`ODI Mes1` #diff_odi es mejoria

#Convierto a Factor para que se puedan hacer la comparaciones entre grupos
df$NHD <- as.factor(df$NHD)
df$Grupo <- as.factor(df$Grupo)
df$LC2 <-  as.factor(df$LC2)

# Box Plot por los dos factores Grupo y NHD
df <- df[c(8,11,12,13)] # Se seleccionan 3 factores y una var cuantitativa continua
ggplot(df,mapping = aes(x=Grupo,y=diff_odi,fill=NHD))+
  geom_boxplot()+
  ggtitle("Mejoria Grupo Convencional y Experimental \n vs Hernias")+
  xlab(names(df)[2]) +
  ylab(names(df)[4]) +
  labs(fill="Cant Hernias")

#Diagrama de Error por los dos factores Grupo y NHD

#ggpubr Library
#ggerrorplot
#Parametros
#df, x, y	
#desc_stat:descriptive statistics to be used for visualizing errors. 
#Default value is "mean_se". Allowed values are one of , "mean", "mean_se", "mean_sd", "mean_ci", 
#"mean_range", "median", "median_iqr", "median_hilow", "median_q1q3", "median_mad", "median_range";
#error.plot ="Puede colocar los limites de la barrita de error
#add=c("violin","media) Añade la distribucion y la media a la grafica


ggpubr::ggerrorplot(df,x = 'Grupo',y='diff_odi',color='NHD',
                    desc_stat = "mean_sd",
                    error.plot="errorbar",
                    add=c("violin","mean"))+
  ggtitle("Diagrama Error Mejoria por Grupo y No de Hernias")



# Box Plot por los dos factores Grupo y LHC 2
 # Se seleccionan 3 factores y una var cuantitativa continua

df <- read_excel("espalda.xlsx")
df$LC2 <- df$LC
df$diff_odi <- 0
df$diff_odi <- df$`ODI Mes0` - df$`ODI Mes1` #diff_odi es mejoria

#Convierto a Factor para que se puedan hacer la comparaciones entre grupos
df$NHD <- as.factor(df$NHD)
df$Grupo <- as.factor(df$Grupo)
df$LC2 <-  as.factor(df$LC2)


df <- df[c(8,11,12,13)] 

ggplot(df,mapping = aes(x=Grupo,y=diff_odi,fill=LC2))+
  geom_boxplot()+
  ggtitle("Mejoria Grupo Convencional y Experimental \n vs Tipo Lumbociatica")+
  xlab(names(df)[2]) +
  ylab(names(df)[4]) +
  labs(fill="Tipo Lumbociatica") +
  scale_fill_manual(name="LumboCiatica",
                    breaks=c(0,1, 2),
                    labels=c("Sin LC", "LC Izq", "LC Der"),
                    values=c("grey", "blue", "yellow") )


#Diagrama de Error por los dos factores Grupo y NHD

#ggpubr Library
#ggerrorplot
#Parametros
#df, x, y	
#desc_stat:descriptive statistics to be used for visualizing errors. 
#Default value is "mean_se". Allowed values are one of , "mean", "mean_se", "mean_sd", "mean_ci", 
#"mean_range", "median", "median_iqr", "median_hilow", "median_q1q3", "median_mad", "median_range";
#error.plot ="Puede colocar los limites de la barrita de error
#add=c("violin","media) Añade la distribucion y la media a la grafica

ggpubr::ggerrorplot(df,x = 'Grupo',y='diff_odi',color='LC2',
                    desc_stat = "mean_sd",
                    error.plot="errorbar",
                    add=c("violin","mean"))+
  ggtitle("Diagrama Error Mejoria por Grupo y Tipo Lumbociatica")



