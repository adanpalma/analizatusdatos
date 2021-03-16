# leer el archivo de csv
library(readr)
datos <- read_csv("EuStockMarkets.csv", 
                  col_types = cols(CAC = col_double(), 
                                   DAX = col_double(), FTSE = col_double(), 
                                   SMI = col_double(), time = col_double()))

# Escalar variables - Estandarizar
scaleDAX <- scale(datos$DAX) # no tiene unidades 
mean(scaleDAX) # media 0
var(scaleDAX) # dispersión 1

# Escalar variables - Quitar media
scaleDAX <- scale(datos$DAX,scale = FALSE) # tenemos unidades
mean(scaleDAX) # media 0
var(scaleDAX) # dispersión NO ES 1

# Escalar variables - Normalizar (desv Estandar 1)
scaleDAX <- scale(datos$DAX,scale = TRUE,center = FALSE) # NO tenemos unidades
mean(scaleDAX) # media ES DIFERENTE DE 0
var(scaleDAX) # dispersión ES MÁS PEQUEÑA QUE LA ORIGINAL