# LEER ARCHIVOS ASCII
# TIPO 1: solo datos
tusDatos <- read.table(file.choose(), skip = 0, header = FALSE, sep =',')

# TIPO 2: encabezado una linea + datos
tusDatos <- read.table(file.choose(), skip = 0, header = TRUE, sep =',')

# TIPO 3: encabezado raro + datos
tusDatos <- read.table(file.choose(), skip = 8, header = TRUE, sep =',')


# LEER ARCHIVOS EXCEL
# OPCIÓN 1. COPY PASTE TÍPICO
read.table(file = "clipboard", sep = "\t", header=TRUE)

# OPCIÓN 2. DIRECTAMENTE DE EXCEL
# Instalar el paquete 'readxls' el paquete más práctico para leer ficheros Excel
install.packages("readr")
# Leer el paquete
library("readxl")

# Puedes leer la hoja poniendo el nombre
misDatos <- read_excel(file.choose(), sheet = "el nombre de la Hoja que quieres leer")
# Puedes leer la hoja número 1. La primera de todas
misDatos <- read_excel(file.choose(), sheet = 1)
