#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

# ZZ final que necesita de UNDERSAMPLING

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

require("lightgbm")
require("primes")

#Parametros del script

PARAM <- list()
PARAM$experimento <- "ZZ9410_public_vs_private_modelo_05"
PARAM$exp_input <- "HT9420_compFinal_modelo05_public_vs_private"

PARAM$semilla_particion <- 763369
PARAM$semilla <- 763381
PARAM$modelo <- 1 # se usa el mejor de la OB, pero a futuro podria variar esto

# FIN Parametros del script


#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )
  
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  
  
  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
        by= agrupa ]
}

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
setorder( tb_log, -ganancia )

#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn=FALSE )

#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_train_final.csv.gz" )
dataset  <- fread( arch_dataset )

#leo el dataset donde voy a aplicar el modelo final
arch_future  <- paste0( base_dir, "exp/", TS, "/dataset_future.csv.gz" )
dfuture <- fread( arch_future )


#defino la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]

dfuture[, ganancia :=  ifelse( clase_ternaria == "BAJA+2", 78000, -2000 )]

particionar( dfuture, 
             division= c(1,99),   #particion 50% / 50%
             agrupa= "clase_ternaria",
             seed= PARAM$semilla_particion )


campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria", "clase01", "ganancia", "train", "fold") )

parametros <- as.list(copy(tb_log[PARAM$modelo]))

dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with=FALSE] ),
                        label=   dataset[ , clase01],
                        weight=  dataset[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                        free_raw_data= FALSE
)

#elimino los parametros que no son de lightgbm
parametros$experimento  <- NULL
parametros$cols         <- NULL
parametros$rows         <- NULL
parametros$fecha        <- NULL
parametros$prob_corte   <- NULL
parametros$estimulos    <- NULL
parametros$ganancia     <- NULL
parametros$iteracion_bayesiana  <- NULL

if( ! ("leaf_size_log" %in% names(parametros) ) )  stop( "El Hyperparameter Tuning debe tener en BO_log.txt  el pseudo hiperparametro  lead_size_log.\n" )
if( ! ("coverage" %in% names(parametros) ) ) stop( "El Hyperparameter Tuning debe tener en BO_log.txt  el pseudo hiperparametro  coverage.\n" )

#Primero defino el tamaño de las hojas
parametros$min_data_in_leaf  <- pmax( 1,  round( nrow(dtrain) / ( 2.0 ^ parametros$leaf_size_log ))  )
#Luego la cantidad de hojas en funcion del valor anterior, el coverage, y la cantidad de registros
parametros$num_leaves  <-  pmin( 131072, pmax( 2,  round( parametros$coverage * nrow( dtrain ) / parametros$min_data_in_leaf ) ) )
cat( "min_data_in_leaf:", parametros$min_data_in_leaf,  ",  num_leaves:", parametros$num_leaves, "\n" )

#ya no me hacen falta
parametros$leaf_size_log  <- NULL
parametros$coverage  <- NULL

#Utilizo la semilla definida en este script
parametros$seed  <- PARAM$semilla

#genero el modelo entrenando en los datos finales
set.seed( parametros$seed )

modelo_final  <- lightgbm( data= dtrain,
                           param=  parametros,
                           verbose= -100 )

#genero la prediccion, Scoring
prediccion  <- predict( modelo_final,
                        data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )

tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes, fold, ganancia ) ]
tb_prediccion[ , prob := prediccion ]

#ordeno por probabilidad descendente
setorder( tb_prediccion, -prob )

tb_prediccion[ , x := .I ]
tb_prediccion[ , gan_acum := cumsum( ganancia ) ]
tb_prediccion[ fold==1,  gan_public  :=  2*cumsum( ganancia ) ]
tb_prediccion[ fold==2,  gan_private :=  2*cumsum( ganancia ) ]

#guardo los resultados de la predicción, por cada registro su probabilidad y ranking
fwrite( tb_prediccion,
        file= "tb_prediccion.txt",
        sep= "\t" )