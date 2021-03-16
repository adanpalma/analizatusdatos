# Leer archivo EuStockMarkets.xlsx
datos <- read_csv("EuStockMarkets.csv", 
                  col_types = cols(CAC = col_double(), 
                                   DAX = col_double(), FTSE = col_double(), 
                                   SMI = col_double(), time = col_double()))
# borrar primera columna
datos <- datos[-1]

# calcular una variables que sea el log(DAX)
varNum <- datos$DAX
logDAX <- log(varNum)
# ejemplo de gráficos
plot(datos$DAX,type="l")
plot(logDAX,type="l")

# poner la variable log(DAX) en el data frame
datos <- cbind(datos,logDAX)

# calcular el producto de dos variables: DAX*SMI
DAX_SMI <- datos$DAX*datos$SMI
datos[1,"DAX"]*datos[1,"SMI"]

# poner la variables DAX_SMI en el data frame
datos <- cbind(datos,DAX_SMI)

# Diferencia entre dos indices borsatiles
diff_DAX_SMI <- datos$DAX-datos$SMI
plot(diff_DAX_SMI,type="l")

# poner la variable Diff en el data frame
datos <- cbind(datos,diff_DAX_SMI)
