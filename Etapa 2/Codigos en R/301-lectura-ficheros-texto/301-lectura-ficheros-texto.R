# Lectura de ficheros de texto
# lectura sin header
datos1 <- read.table("ejemplo-datos.txt",header = FALSE,sep = ",")

# lectura con header
data2 <- read.table(file.choose(),header = TRUE,sep = ",")

# lectura con header en una ubicación determinada
data3 <- read.table("C:/Users/Anna i Jordi/Desktop/carpeta trabajo/ejemplo-datos-header.txt",header = TRUE,sep = ",")

# lectura con header skip de las primera filas
data4 <- read.table("ejemplo-datos-header-skip.txt",header = TRUE, skip = 8,sep = ",")

# lectura sin header con separador de punto y coma
data5 <- read.table(file.choose(),header = FALSE, sep = ";")