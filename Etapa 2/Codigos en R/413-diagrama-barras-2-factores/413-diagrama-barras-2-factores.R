

# importar datos - leer datos
datos <- HairEyeColor # son datos tipo tabla (no es usual leer los datos así)
df_bruto <- as.data.frame(HairEyeColor) # los convertimos a data frame

# preproceso de datos
hair_eye_col <- df_bruto[rep(row.names(df_bruto), df_bruto$Freq), 1:3]
rownames(hair_eye_col) <- 1:nrow(hair_eye_col)
head(hair_eye_col) # tabla de frecuencias de 3 variables cualitativas

df <- hair_eye_col # este el data frame que deberías tener (por ejemplo seleccionando 3 variables cualitativas)




# tabla de contingencias de dos factor
tfrec <- table(df$Hair,df$Eye)
tfrec <- as.data.frame(tfrec)
colnames(tfrec) <- c("Hair","Eye","Frec")


# La entrada es un data frame con la tabla de contingencias - df
df <- tfrec


library(ggpubr)

# diagrama de barras de 2 factores apiladas
name1 = names(df)[1]
name2 = names(df)[2]
ggbarplot(df, x = name1, y = "Frec",
          color = name2)

#diagrama de barras de 2 factores no apiladas
ggbarplot(df, x = name1, y = "Frec",
          color = name2, position = position_dodge())



# la entrada es un data frame con las dos variables cualitativas

df <- hair_eye_col[c(1,2)] # este el data frame que deberías tener (por ejemplo seleccionando 3 variables cualitativas)


library(ggplot2)

# Diagrama de barras de dos variables categóricas apiladas
name1 <- names(df)[1]
name2 <- names(df)[2]
p<- ggplot(df, aes(df[,name1], ..count..)) +
           geom_bar(aes(fill = df[,name2]), position = "stack") +
           xlab(name1) + ylab("Frecuencia Absoluta") +
           theme_minimal() +
           ggtitle(paste("Diagrama de Barras de la variable ",name1)) +
           theme(plot.title = element_text(hjust = 0.5)) +
           guides(fill=guide_legend(title=""))

p


library(plotly)
ggplotly(p)


library(ggplot2)

# Diagrama de barras de dos variables categóricas separadas
name1 <- names(df)[1]
name2 <- names(df)[2]
p2 <- ggplot(df, aes(df[,name1], ..count..)) +
  geom_bar(aes(fill = df[,name2]), position = "dodge") +
  xlab(name1) + ylab("Frecuencia Absoluta") +
  theme_minimal() +
  ggtitle(paste("Diagrama de Barras de la variable ",name1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=""))

p2





library(plotly)

# Diagrama de barras de dos variables categóricas

ggplotly(p2)