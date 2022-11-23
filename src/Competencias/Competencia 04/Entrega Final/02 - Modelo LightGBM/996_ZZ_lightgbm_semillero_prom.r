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
PARAM$experimento <- "ZZAVG_9410_semillerio_promedio_modelo06"
PARAM$exp_input <- "ZZ9410_semillerio_compFinal_modelo06"

PARAM$archivo_semillas <- "ksemillas.csv"
PARAM$indice_inicio_semilla <- 1
PARAM$indice_fin_semilla <- 74

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

base_dir <- "~/buckets/b1/"
base_input_dir <- paste0( base_dir, "exp/", PARAM$exp_input, "/")

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))

# Leo las n semillas con las que trabajé anteriormente
dt.semillas <- fread(paste0(base_input_dir, PARAM$archivo_semillas))
semillas <- dt.semillas$x[PARAM$indice_inicio_semilla:PARAM$indice_fin_semilla]

archivo.semillerio.promedio <- "promedio.semillas.csv"

dt.prom <- NULL

if (!file.exists(archivo.semillerio.promedio)) {
  dt.prom <- promediar.prob(semillas)
  
  fwrite(  dt.prom,
           file= archivo.semillerio.promedio,
           sep= "," )
} else {
  dt.prom <- fread(archivo.semillerio.promedio)
}

#genero los archivos para Kaggle
cortes  <- seq( from=  7000,
                to=   20000,
                by=     500 )


setorder( dt.prom, -prediccion )

for( corte in cortes )
{
  dt.prom[  , Predicted := 0L ]
  dt.prom[ 1:corte, Predicted := 1L ]
  
  nom_submit  <- paste0( PARAM$experimento,
                         "_",
                         sprintf( "%05d", corte ),
                         ".csv" )
  
  fwrite(  dt.prom[ , list( numero_de_cliente, Predicted ) ],
           file= nom_submit,
           sep= "," )
  
}