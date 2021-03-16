

# importar datos - leer datos
datos <- iris
df <- datos[5]

# tabla de frecuencias de un factor
tfrec <- as.data.frame(table(df))
colnames(tfrec) <- c("Species","Frec")


# La entrada es un data frame con la tabla de frecuencias de 1 factor
df <- tfrec


# diagrama de sectores sencillo
name1 <- names(df)[1]
name2 <- names(df)[2]
slices <- df[,name2]
lbls <- df[,name1]
pie(slices, labels = lbls, main="Título del diagrama de sectores")


# diagrama de sectores con porcentajes
name1 <- names(df)[1]
name2 <- names(df)[2]
slices <- df[,name2]
lbls <- df[,name1]
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls,
    main="Título del diagrama de sectores")



library(plotrix)

# diagrma de sectores 3D
name1 <- names(df)[1]
name2 <- names(df)[2]
slices <- df[,name2]
lbls <- df[,name1]
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of Countries ")

