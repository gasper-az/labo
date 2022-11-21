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

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "ZZ9420_compFinal_modelo01_bis" # TODO: atenti con este nombre. Ya debe existir esta carpeta. Se mejorará en futuras versiones del script.
PARAM$exp_input  <- "HT9420_compFinal_modelo01_bis" # TODO: atenti con este nombre.
PARAM$model_file_name <- "modelo_01_056.model"   # TODO: acá debe ir el nombre del modelo creado (debe estar en la misma carpeta de PARAM$experimento. Se mejorará en futuras versiones del script)
# FIN Parametros del script

ksemilla  <- 763369

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa (es igual al file 992_ZZ_lightgbm_under)

base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#leo la salida de la optimizaciob bayesiana
# arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
# tb_log  <- fread( arch_log )
# setorder( tb_log, -ganancia )

#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn=FALSE )

# #leo el dataset donde voy a entrenar el modelo final
# arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_train_final.csv.gz" )
# dataset  <- fread( arch_dataset )

#leo el dataset donde voy a aplicar el modelo final
# arch_future  <- paste0( base_dir, "exp/", TS, "/dataset_future.csv.gz" )
# dfuture <- fread( arch_future )

###########################################################################################
#################### Hasta acá, es igual al file 992_ZZ_lightgbm_under ####################
###########################################################################################

# leo el dataset donde voy a testear el modelo final
arch_training  <- paste0( base_dir, "exp/", TS, "/dataset_training.csv.gz" )
dtraining <- fread( arch_training )

# NOTE: Las columnas ("azar_under", "azar_sampling") se agregan al final del file 992_ZZ_lightgbm_under. Importante sacarlas, ya que cuando se hizo el modelo, el archivo de TRAIN NO las tenía
campos_buenos  <- setdiff( copy(colnames( dtraining )), c( "clase01", "clase_ternaria", "fold_train", "fold_validate", "fold_test", "azar_under", "azar_sampling" ) )

dtest <- dtraining[ fold_test== 1 ]

# acá cargo un modelo ya entrenado (como, por ejemplo, el que crea 992_ZZ_lightgbm_under)
modelo_final <- lgb.load(filename = PARAM$model_file_name)

# calculo la predicción sobre el dataset de testing
prediccion_test  <- predict( modelo_final,
                             data.matrix( dtest[ , campos_buenos, with=FALSE ] ) )

tb_prediccion_test  <- dtest[  , list( numero_de_cliente, foto_mes) ]
tb_prediccion_test[ , prob := prediccion_test ]

# nombre muy ingenioso para la predicción
nom_pred_test <- "tb_prediccion_test.csv"

# guardo la predicción
fwrite( tb_prediccion_test,
        file= nom_pred_test,
        sep= "\t" )