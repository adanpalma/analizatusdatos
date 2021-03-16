#********************************************************************
# 1.1 INSTALAR PAQUETES DE FUNCIONES
#********************************************************************
# Lista de paquetes de funciones a instalar
.packages = c("xlsx")

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

# SELECCIONAMOS VARIABLES NUMÉRICAS
df <- misDatos[c(3,4,5,6,7,8,9)]

# OPERACIONES PARA LOS DATAFRAMES
summary(misDatos)
summary(df)
sapply(df, mean)
sapply(df, max)

estadisticaDescriptiva <- do.call(data.frame, 
               list(mean = apply(df, 2, mean),
                    std = apply(df, 2, sd),
                    median = apply(df, 2, median),
                    min = apply(df, 2, min),
                    max = apply(df, 2, max),
                    quantile_1 = apply(df,2,quantile,probs = c(0.25)),
                    quantile_3 = apply(df,2,quantile,probs = c(0.75))
               ))

