

# importar datos - leer datos - ejemplo 1
datos <- iris
df <- datos[5]

# tabla de frecuencias de un factor
aux = table(df)
tfrec <- as.data.frame(table(df))
colnames(tfrec) <- c("Species","Frec")





# importar datos - leer datos - ejemplo 2
datos <- HairEyeColor # son datos tipo tabla (no es usual leer los datos así)
df_bruto <- as.data.frame(HairEyeColor) # los convertimos a data frame

# preproceso de datos
hair_eye_col <- df_bruto[rep(row.names(df_bruto), df_bruto$Freq), 1:3]
rownames(hair_eye_col) <- 1:nrow(hair_eye_col)
head(hair_eye_col) # tabla de frecuencias de 3 variables cualitativas

df <- hair_eye_col # este el data frame que deberías tener (por ejemplo seleccionando 3 variables cualitativas)

# tabla de frecuencias de un factor - variable Hair
tfrec <- table(df$Hair)
tfrec <- as.data.frame(tfrec)
colnames(tfrec) <- c("Hair","Frec")


# tabla de frecuencias de un factor - variable Eye
tfrec <- table(df$Eye)
tfrec <- as.data.frame(tfrec)
colnames(tfrec) <- c("Eye","Frec")