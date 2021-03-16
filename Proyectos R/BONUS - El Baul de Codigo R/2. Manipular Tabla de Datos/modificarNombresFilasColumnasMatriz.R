#*******************************************************************************

# MODIFICAR NOMBRES DE LAS FILAS y LAS COLUMNAS DE UNA MATRIZ

#*******************************************************************************
#-------------------------------------------------------------------------------


# Construimos la tabla de contingencias:
tablaContingencias <- matrix(c(490, 400,10,100),2,2)
colnames(tablaContingencias) <- c("Fumadores","No Fumadores")
rownames(tablaContingencias) <- c("Cancer","No Cancer")