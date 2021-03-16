#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
#********************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("ggplot2", "plotly", "readxl")

# Instala los paquetes sinó los tienes instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])


#********************************************************************
# 1.2 CARGAR PAQUETES O CREAR FUNCIONES
#********************************************************************
# Carga los paquetes sinó los tienes cargados
lapply(.packages, require, character.only=TRUE)

# LEER LOS DATOS EN FORMATO DATAFRAME de UN excel
misDatos <- read_excel(file.choose(), sheet = 1)

#####################################################
# GRÁFICOS BASE CON PLOT()
#####################################################
x <- misDatos$F.
y <- misDatos$G.

plot(x = x, y = y,
     pch = 16, frame = FALSE, main = "Partidos Ganados vs Goles a Favor",
     xlab = "Goles metidos", ylab = "Ganados", col = "#2E9FDF")

# OPCIÓN 1 - GRABAR EN PDF

# Abrir archivo pdf
pdf("ejemploPlot.pdf") 
# Crear el plot
plot(x = x, y = y,
     pch = 16, frame = FALSE, main = "Partidos Ganados vs Goles a Favor",
     xlab = "Goles a Favor", ylab = "Partidos Ganados", col = "#2E9FDF")
# Cerrar el archivo pdf
dev.off() 

# OPCIÓN 2 - GRABAR EN JPG

# Abrir archivo jpg
jpeg("ejemploPlot.jpg", width = 780,height = 450)
# Crear el plot
plot(x = x, y = y,
     pch = 16, frame = FALSE, main = "Partidos Ganados vs Goles a Favor",
     xlab = "Goles a Favor", ylab = "Partidos Ganados", col = "#2E9FDF")
# Cerrar el archivo
dev.off()

#####################################################
# GRÁFICOS CON GGPLOT2
#####################################################
scatter2D <- ggplot(misDatos,aes(x = F., y = G.)) + 
  geom_point(shape=16,size = 6,colour = "#2E9FDF") + 
  ggtitle("Partidos Ganados vs Goles a Favor") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goles a Favor", y = "Partidos Ganados")

scatter2D

# OPCIÓN 1 - GRABAR EN PDF

# Abrir archivo pdf
pdf("ejemploPlot.pdf") 
# Crear el plot
scatter2D
# Cerrar el archivo pdf
dev.off() 

# OPCIÓN 2 - GRABAR EN JPG

# Abrir archivo jpg
jpeg("ejemploPlot.jpg", width = 780,height = 450)
# Crear el plot
scatter2D
# Cerrar el archivo
dev.off()

#####################################################
# GRÁFICOS CON PLOTLY
#####################################################
p <- plot_ly(data = misDatos, x = ~F., y = ~G.,
             type = "scatter", mode = "markers",
             marker = list(size = 10,
                           color = 'rgba(255, 182, 193, .9)',
                           line = list(color = 'rgba(152, 0, 0, .8)',
                                       width = 2)))%>%
  layout(title = 'Partidos Ganados vs Goles a Favor',
         yaxis = list(zeroline = FALSE, title = "Partidos Ganados"),
         xaxis = list(zeroline = FALSE,title = "Goles a Favor"))

p
