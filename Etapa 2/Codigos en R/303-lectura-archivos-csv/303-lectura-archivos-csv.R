# leer un archivo csv
# install.packages("readr")
library(readr)
datos1 <- read_csv("EuStockMarkets.csv")

# Quitando la primera variable del dataframe
df <- datos1[-1]

# leer otro archivo csv
library(readr)
datos2 <- read_csv(file.choose()) # apunta el archivo mtcars.xls
# Quitando la primera variable del dataframe
df2 <- datos2[-1]
# Identificar el nombre de las filas
row.names(df2) <- unlist(datos2[,1])