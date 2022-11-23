#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

# ZZ final que necesita de UNDERSAMPLING

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

###############################################################
#################### Parametros del script ####################
###############################################################

PARAM <- list()
PARAM$base_dir <- "C:\\uba\\repos\\labo\\src\\Competencias\\Competencia 04\\Entrega Final"
PARAM$semillerios_dir <- "ZZAVG"
PARAM$avg_semillerios_files <- c("promedio.semillas.modelo1.csv", "promedio.semillas.modelo2.csv", "promedio.semillas.modelo3.csv")
PARAM$envios_dir <- "envios"
PARAM$base_file_name <- "ensamble_semillerio_modelos_1_2_3"

PARAM$semilla <- 763369

###################################################
#################### Funciones ####################
###################################################

promediar.prob <- function(semillas) {
  dt.resultados <- NULL
  
  # Columnas de dataset leído
  # numero_de_cliente
  # foto_mes
  # prob
  # rank
  for (semilla in semillas) {
    file.name <- paste0(base_input_dir, PARAM$exp_input, "_", semilla, "_resultados.csv")
    dt.res.sem <- fread(file.name)
    
    # me aseguro de que esté ordenado
    dt.res.sem <- dt.res.sem[order(rank(numero_de_cliente))]
    
    if (is.null(dt.resultados)) {
      dt.resultados <- data.table(numero_de_cliente = dt.res.sem[, numero_de_cliente],
                                  foto_mes = dt.res.sem[, foto_mes])
      
      dt.resultados <- dt.resultados[order(rank(numero_de_cliente))]
    }
    
    dt.resultados[, paste0("prob_", semilla) := dt.res.sem[, prob]]
  }
  
  
  dt.final.semillerio.promedio <- data.table(
    numero_de_cliente = dt.resultados[, numero_de_cliente],
    prediccion = rowMeans(dt.resultados[, c(-1, -2)]) # excluye el numero_de_cliente y foto_mes del cálculo de la media
  )
  
  return(dt.final.semillerio.promedio)
}

###################################################################
#################### Aquí comienza el programa ####################
###################################################################

# base_dir <- "~/buckets/b1/"
# base_input_dir <- paste0( base_dir, "exp/", PARAM$exp_input, "/")
# 
# #creo la carpeta donde va el experimento
# dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )

# Cambio el working directory
setwd(PARAM$base_dir)
# Creo la carpeta de envíos, si NO existe
dir.create(PARAM$envios_dir, showWarnings = FALSE )

dt.resultados <- NULL

for (semillerio.file in PARAM$avg_semillerios_files) {
  # Leo el archivo que contiene los semillerios promediados
  dt.sem.avg <- fread(paste(PARAM$semillerios_dir, semillerio.file, sep = "\\"))
  
  # me aseguro de que esté ordenado
  dt.sem.avg <- dt.sem.avg[order(rank(numero_de_cliente))]
  
  if (is.null(dt.resultados)) {
    dt.resultados <- data.table(numero_de_cliente = dt.sem.avg[, numero_de_cliente])
    
    # me aseguro de que esté ordenado
    dt.resultados <- dt.resultados[order(rank(numero_de_cliente))]
  }
  
  # agrego la predicción del file como columna
  dt.resultados[, (semillerio.file) := dt.sem.avg[, prediccion]]
}

# dataset final. Tiene el promedio de todas las predicciones
dt.final <- data.table(
  numero_de_cliente = dt.resultados[, numero_de_cliente],
  prediccion = rowMeans(dt.resultados[, c(-1)]) # excluye el numero_de_cliente
)

# Ordeno por predicción, de forma descendente
setorder( dt.final, -prediccion )

#genero los archivos para Kaggle
cortes  <- seq( from=  7000,
                to=   20000,
                by=     500 )

for( corte in cortes ) {
  dt.final[  , Predicted := 0L ]
  dt.final[ 1:corte, Predicted := 1L ]

  nom_submit  <- paste0( PARAM$envios_dir,
                         "/",
                         PARAM$base_file_name,
                         "_",
                         sprintf( "%05d", corte ),
                         ".csv" )

  fwrite(  dt.final[ , list( numero_de_cliente, Predicted ) ],
           file= nom_submit,
           sep= "," )

}