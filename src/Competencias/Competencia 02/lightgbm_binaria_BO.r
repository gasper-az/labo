# source( "~/labo/src/lightgbm/z723_lightgbm_binaria_BO.r" )
# Este script esta pensado para correr en Google Cloud
#   8 vCPU
#  32 GB memoria RAM
# 256 GB espacio en disco

# se entrena con POS =  { BAJA+1, BAJA+2 }
# Optimizacion Bayesiana de hiperparametros de  lightgbm, con el metodo TRADICIONAL de los hiperparametros originales de lightgbm
# 5-fold cross validation
# la probabilidad de corte es un hiperparametro

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
         makeNumericParam("learning_rate",    lower=    0.005, upper=    0.3),  # modificado
         makeNumericParam("feature_fraction", lower=    0.001, upper=    1.0),  # modificado
         makeIntegerParam("min_data_in_leaf", lower=    20L  , upper=  8000L),  # modificado
         makeIntegerParam("num_leaves",       lower=   16L   , upper=  1024L),  # modificado
         makeIntegerParam("envios",           lower= 5000L   , upper= 15000L),
         # makeIntegerParam("max_bin",          lower= 2L      , upper= 31L),   # modificado
         makeNumericParam("bagging_fraction", lower= 0.0001  , upper= 0.9999),  # modificado. Debe estar entre 0 y 1
         makeIntegerParam("bagging_freq",     lower= 1       , upper= 999),     # modificado. Buscamos entre 1 (mínimo y necesario) y 999 (max iteracions, ver más abajo)
         makeNumericParam("lambda_l1",        lower= 0.0001  , upper= 100),     # modificado: more info: https://towardsdatascience.com/kagglers-guide-to-lightgbm-hyperparameter-tuning-with-optuna-in-2021-ed048d9838b5
         makeNumericParam("lambda_l2",        lower= 0.0001  , upper= 100),     # modificado: more info: https://towardsdatascience.com/kagglers-guide-to-lightgbm-hyperparameter-tuning-with-optuna-in-2021-ed048d9838b5
         makeNumericParam("min_gain_to_split",lower= 0.0001  , upper= 15),      # modificado: more info: https://towardsdatascience.com/kagglers-guide-to-lightgbm-hyperparameter-tuning-with-optuna-in-2021-ed048d9838b5
         makeIntegerParam("max_depth ",       lower= 6       , upper= 20)       # modificado: busco entre 1 y 30. Después veré que otros valores pueden ser de interés
        )

#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM  <- list()

PARAM$experimento  <- "HT7235"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )

PARAM$trainingstrategy$undersampling  <-  1.0   # un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$semilla_azar   <- 763369  #Aqui poner la propia semilla

PARAM$hyperparametertuning$iteraciones <- 100
PARAM$hyperparametertuning$xval_folds  <- 5
PARAM$hyperparametertuning$POS_ganancia  <- 78000
PARAM$hyperparametertuning$NEG_ganancia  <- -2000

PARAM$hyperparametertuning$semilla_azar  <- 763381  #Aqui poner la propia semilla, PUEDE ser distinta a la de trainingstrategy

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./exp/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#esta funcion calcula internamente la ganancia de la prediccion probs

fganancia_logistic_lightgbm  <- function( probs, datos) 
{
  vpesos   <- get_field(datos, "weight")

  #vector de ganancias
  vgan  <- ifelse( vpesos == 1.0000002, PARAM$hyperparametertuning$POS_ganancia, 
                   ifelse( vpesos == 1.0000001, PARAM$hyperparametertuning$NEG_ganancia, 
                           PARAM$hyperparametertuning$NEG_ganancia / PARAM$trainingstrategy$undersampling ) )

  tbl  <- as.data.table( list( "vprobs" = probs, "vgan" = vgan ) )
  setorder( tbl,  -vprobs )
  ganancia <- tbl[ 1:GLOBAL_envios, sum( vgan ) ]

  return( list( "name"= "ganancia", 
                "value"=  ganancia,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  gc()  #libero memoria

  #llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  #para usar en fganancia_logistic_lightgbm 
  GLOBAL_envios <<- as.integer(x$envios/PARAM$hyperparametertuning$xval_folds)   #asigno la variable global

  kfolds  <- PARAM$hyperparametertuning$xval_folds   # cantidad de folds para cross validation

  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          # max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          # min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          # lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          # lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE,   #para que los alumnos no se atemoricen con tantos warning
                          seed= PARAM$hyperparametertuning$semilla_azar
                        )

  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )

  param_completo  <- c( param_basicos, param_variable, x )

  set.seed( PARAM$hyperparametertuning$semilla_azar )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_logistic_lightgbm,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
                      )

  #obtengo la ganancia
  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]

  ganancia_normalizada  <-  ganancia* kfolds     #normailizo la ganancia

  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL     #elimino de la lista el componente  "early_stopping_rounds"
  
  #Voy registrando la importancia de variables
  if( ganancia_normalizada >  GLOBAL_gananciamax )
  {
    GLOBAL_gananciamax  <<- ganancia_normalizada
    modelo  <- lgb.train( data= dtrain,
                          param= param_completo,
                          verbose= -100
                         )

    tb_importancia  <- as.data.table( lgb.importance(modelo ) )
    archivo_importancia  <- paste0( "impo_", GLOBAL_iteracion,".txt")
    fwrite( tb_importancia,
            file= archivo_importancia,
            sep= "\t" )
  }


  #el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra

  #logueo 
  xx  <- param_completo
  xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear( xx, arch= klog )

  return( ganancia_normalizada )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread( PARAM$input$dataset )

#-------------------------------------------------------------#
#------------- RANKING DE VARIABLES CON DRIFTING -------------#
#-------------------------------------------------------------#

variables.con.drifting <- c(
  "mcomisiones"
  ,"mpasivos_margen"
  ,"mcuenta_corriente"
  ,"mcaja_ahorro"
  ,"mcaja_ahorro_dolares"
  ,"mcuentas_saldo"
  ,"mtarjeta_visa_consumo"
  ,"ccuenta_debitos_automaticos"
  ,"mcuenta_debitos_automaticos"
  ,"ccomisiones_otras"
  ,"mcomisiones_otras"
  ,"chomebanking_transacciones"
  ,"ccajas_otras"
  ,"Master_mfinanciacion_limite"
  ,"Master_Finiciomora"
  ,"Master_fultimo_cierre"
  ,"Visa_Finiciomora"
  ,"Visa_msaldopesos"
  ,"Visa_mconsumospesos"
  ,"Visa_madelantodolares"
  ,"Visa_fultimo_cierre"
  ,"Visa_mpagado"
  ,"Visa_mpagosdolares"
  ,"Visa_mconsumototal"
)

rank.prefix <- "ranked_"
for (var in variables.con.drifting) {
  dataset[, paste0(rank.prefix, var) := frankv(dataset, cols = var, na.last = TRUE, ties.method = "dense")]
  dataset[, (var) := NULL]
}

#-------------------------------------------------------------#
#----------- FIN RANKING DE VARIABLES CON DRIFTING -----------#
#-------------------------------------------------------------#

#-------------------------------------------------------------#
#-------------------- FEATURE ENGINEERING --------------------#
#-------------------------------------------------------------#

dataset[, master_fe_suma_all :=
          Master_msaldototal + Master_msaldopesos + Master_msaldodolares +
          Master_mconsumospesos + Master_mconsumosdolares + Master_mlimitecompra +
          Master_madelantopesos + Master_madelantodolares + Master_mpagado +
          Master_mpagospesos + Master_mpagosdolares + Master_mconsumototal +
          Master_mpagominimo
]

dataset[, visa_fe_suma_all :=
          Visa_mfinanciacion_limite + Visa_msaldototal + Visa_msaldodolares + 
          Visa_mconsumosdolares + Visa_mlimitecompra + Visa_madelantopesos +
          Visa_mpagospesos + Visa_mpagominimo
]

dataset[, tarjetas_fe_suma_all := master_fe_suma_all + visa_fe_suma_all]

dataset[, pesos_fe_suma_menos_tarjetas :=
          mrentabilidad + mrentabilidad_annual + mactivos_margen +
          mcuenta_corriente_adicional + mcaja_ahorro_adicional + mautoservicio + 
          mtarjeta_master_consumo + mprestamos_personales + mprestamos_prendarios +
          mprestamos_hipotecarios + mplazo_fijo_dolares + mplazo_fijo_pesos +
          minversion1_pesos + minversion1_dolares + minversion2 +
          mpayroll + mpayroll2 + mttarjeta_master_debitos_automaticos +
          mpagodeservicios + mpagomiscuentas + mcajeros_propios_descuentos +
          mtarjeta_visa_descuentos + mtarjeta_master_descuentos + mcomisiones_mantenimiento +
          mforex_buy + mforex_sell + mtransferencias_recibidas +
          mtransferencias_emitidas + mextraccion_autoservicio + mcheques_depositados +
          mcheques_emitidos + mcheques_depositados_rechazados + mcheques_emitidos_rechazados +
          matm + matm_other
]

dataset[, cociente_fe_01 := ctrx_quarter/mcuentas_saldo]
dataset[, cociente_fe_02 := pesos_fe_suma_menos_tarjetas/mrentabilidad_annual]
dataset[, cociente_fe_03 := pesos_fe_suma_menos_tarjetas/ranked_mcaja_ahorro]
dataset[, cociente_fe_04 := pesos_fe_suma_menos_tarjetas/Master_Fvencimiento]
dataset[, cociente_fe_05 := pesos_fe_suma_menos_tarjetas/cociente_fe_01]
dataset[, cociente_fe_06 := pesos_fe_suma_menos_tarjetas/ranked_mcuentas_saldo]
dataset[, cociente_fe_07 := mrentabilidad_annual/ranked_mcaja_ahorro]
dataset[, cociente_fe_08 := mrentabilidad_annual/Master_Fvencimiento]
dataset[, cociente_fe_09 := mrentabilidad_annual/cociente_fe_01]
dataset[, cociente_fe_10 := mrentabilidad_annual/ranked_mcuentas_saldo]
dataset[, cociente_fe_11 := ranked_mcaja_ahorro/Master_Fvencimiento]
dataset[, cociente_fe_12 := ranked_mcaja_ahorro/cociente_fe_01]
dataset[, cociente_fe_13 := ranked_mcaja_ahorro/ranked_mcuentas_saldo]
dataset[, cociente_fe_14 := Master_Fvencimiento/cociente_fe_01]
dataset[, cociente_fe_15 := Master_Fvencimiento/ranked_mcuentas_saldo]
dataset[, cociente_fe_16 := cociente_fe_01/ranked_mcuentas_saldo]
dataset[, suma_fe_01 := pesos_fe_suma_menos_tarjetas + mrentabilidad_annual]
dataset[, vabs_fe_01 := abs(pesos_fe_suma_menos_tarjetas - mrentabilidad_annual)]
dataset[, prod_fe_01 := pesos_fe_suma_menos_tarjetas * mrentabilidad_annual]

#-------------------------------------------------------------#
#------------------ FIN FEATURE ENGINEERING ------------------#
#-------------------------------------------------------------#

#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd( paste0( "./exp/", PARAM$experimento, "/") )   #Establezco el Working Directory DEL EXPERIMENTO

#en estos archivos quedan los resultados
kbayesiana  <- paste0( PARAM$experimento, ".RDATA" )
klog        <- paste0( PARAM$experimento, ".txt" )


GLOBAL_iteracion  <- 0   #inicializo la variable global
GLOBAL_gananciamax <- -1 #inicializo la variable global

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
  GLOBAL_gananciamax  <- tabla_log[ , max( ganancia ) ]
}



#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ foto_mes %in% PARAM$input$training, clase01 := ifelse( clase_ternaria=="CONTINUA", 0L, 1L) ]


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "azar", "training" ) )

set.seed( PARAM$trainingstrategy$semilla_azar )
dataset[  , azar := runif( nrow( dataset ) ) ]
dataset[  , training := 0L ]
dataset[ foto_mes %in% PARAM$input$training & 
          ( azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c( "BAJA+1", "BAJA+2" ) ),
         training := 1L ]

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ training == 1L, campos_buenos, with=FALSE]),
                        label= dataset[ training == 1L, clase01 ],
                        weight=  dataset[ training == 1L, ifelse( clase_ternaria=="BAJA+2", 1.0000002, ifelse( clase_ternaria=="BAJA+1",  1.0000001, 1.0) )],
                        free_raw_data= FALSE  )



#Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,     #definido al comienzo del programa
              has.simple.signature = FALSE   #paso los parametros en una lista
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= PARAM$hyperparametertuning$iteraciones )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( kbayesiana ) ) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}


quit( save="no" )

