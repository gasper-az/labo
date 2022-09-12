# Este script esta pensado para corren en Google Cloud
# si se lo desea correr en Windows debera
#  * cambiar el setwd()  y las rutas
#  * cuando llame a la funcion mcmapply  poner  mc.cores=1
#  * armarse de mucha paciencia porque va a demorar muchas horas en Windows

#Optimizacion Bayesiana de hiperparametros de  rpart
# Hace  1-Repeated  5-Fold Cross Validation


# NO utiliza Feature Engineering  ( el Fiscal General se enoja ... )


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("rpart")
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

#aqui deben ir SUS semillas, se usan para  1-Repeated  (5-Fold Cross Validation)
ksemilla_azar  <- c(763369)


#Defino la  Optimizacion Bayesiana

kBO_iter  <- 100   #cantidad de iteraciones de la Optimizacion Bayesiana

hs  <- makeParamSet(
          makeNumericParam("cp"       , lower=  -1.0, upper=    0.1),
          makeNumericParam("minsplit" , lower=   1,   upper= 5000 ),
          makeNumericParam("minbucket", lower=   1,   upper= 1000 ),
          makeIntegerParam("maxdepth" , lower=   3L,  upper=   20L),  #la letra L al final significa ENTERO
          forbidden = quote( minbucket > 0.5*minsplit ) )             # minbuket NO PUEDE ser mayor que la mitad de minsplit


#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0( folder, substitute( reg), ext )

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
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 
# particionar( data=dataset, division=c(1,1,1,1,1), agrupa=clase_ternaria, seed=semilla)   divide el dataset en 5 particiones

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
           by= agrupa ]
}
#------------------------------------------------------------------------------
#fold_test  tiene el numero de fold que voy a usar para testear, entreno en el resto de los folds
#param tiene los hiperparametros del arbol

ArbolSimple  <- function( fold_test, data, param )
{
  param2 <- param
  #param2$minsplit   <- as.integer( round( 2^param$minsplit ) )
  #param2$minbucket  <- as.integer( round( 2^param$minbucket ) )
  
  
  #---------------------------------------------------------------------------------------------#
  #-------------------- Marco variables en la que identifiqué Data Drifting --------------------#
  #---------------------------------------------------------------------------------------------#

  variables.drifting <- c(
    "Master_mlimitecompra",
    "mcomisiones_mantenimiento",
    "mextraccion_autoservicio",
    "mforex_sell",
    "Visa_mlimitecompra",
    "Visa_mpagado"
  )
  
  #--------------------------------------------------------------------------------------------#
  #-------------------- Defino variables a quitar de la fórmula del modelo --------------------#
  #--------------------------------------------------------------------------------------------#
  
  variables.sacar <- c(
    "clase_ternaria",
    variables.drifting
  )
  
  #--------------------------------------------------------------------------------------------#
  #--------------------- Quito variables que performan PEOR que canarios ----------------------#
  #--------------------------------------------------------------------------------------------#
  
  variables.sacar <- c(
    variables.sacar
    # ,
    # "cliente_edad",
    # "numero_de_cliente",
    # "Master_mfinanciacion_limite",
    # "mpasivos_margen",
    # "ccajas_extracciones",
    # "Visa_msaldototal",
    # "mcuenta_corriente",
    # "ctarjeta_master_debitos_automaticos",
    # "ccallcenter_transacciones",
    # "ctarjeta_master_transacciones",
    # "mttarjeta_visa_debitos_automaticos",
    # "mactivos_margen",
    # "Master_fechaalta",
    # "Master_Fvencimiento",
    # "mcaja_ahorro_dolares",
    # "mrentabilidad",
    # "cliente_antiguedad",
    # "mcaja_ahorro",
    # "Visa_status",
    # "Visa_Fvencimiento",
    # "Visa_fechaalta",
    # "mrentabilidad_annual",
    # "mcomisiones_mantenimiento"
  )
  
  #-----------------------------------------------------------------#
  #-------------------- Creo fórmula del modelo --------------------#
  #-----------------------------------------------------------------#
  
  formula.modelo <- "clase_binaria ~ ."
  
  for (variable in variables.sacar) {
    formula.modelo <- paste(formula.modelo, variable, sep = " -")
  }
  
  #genero el modelo
  modelo  <- rpart(formula  = formula.modelo,
                   data     = data[ fold != fold_test, ],  #entreno en todo MENOS el fold_test que uso para testing
                   xval     = 0,
                   control  = param2 )

  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo, 
                          data[ fold==fold_test, ],  #aplico el modelo sobre los datos de testing
                          type= "prob")   #quiero que me devuelva probabilidades

  #En el 1er cuatrimestre del Tercer Año de la Maestria se explicaran las siguientes 12 lineas
  dtest <- copy( data[ fold==fold_test , list( clase_ternaria )] )
  dtest[ , pred := prediccion[ ,"SI"] ]
  dtest[ , azar := runif( nrow( dtest ) ) ]
  setorder(  dtest, -pred, azar )

  dtest[ , gan :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
  dtest[ , gan_acum := cumsum( gan ) ]

  #calculo la ganancia
  dtest2   <- dtest[ (1:100)*100,  ]
  idx_max  <- which.max( dtest2$gan_acum ) 
  ganancia_testing  <- dtest2[ (idx_max-1):(idx_max+1),  mean(gan_acum) ]


  rm( dtest )
  rm( dtest2 )

  return( ganancia_testing )  #esta es la ganancia sobre el fold de testing, NO esta normalizada
}
#------------------------------------------------------------------------------

ArbolesCrossValidation  <- function( semilla, data, param, qfolds, pagrupa )
{
  divi  <- rep( 1, qfolds )  # generalmente  c(1, 1, 1, 1, 1 )  cinco unos

  particionar( data, divi, seed=semilla, agrupa=pagrupa )  #particiono en dataset en folds

  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 5 )   #debe ir 1 si es Windows

  data[ , fold := NULL ]

  #devuelvo la primer ganancia y el promedio
  ganancia_promedio  <- mean( unlist( ganancias ) )   #promedio las ganancias
  ganancia_promedio_normalizada  <- ganancia_promedio * qfolds  #aqui normalizo la ganancia

  gc()

  return( ganancia_promedio_normalizada )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros, lamentablemente se pasan como variables globales

EstimarGanancia  <- function( x )
{
   GLOBAL_iteracion  <<-  GLOBAL_iteracion + 1

   xval_folds  <- 5
   vganancias <- mcmapply( ArbolesCrossValidation,
                           ksemilla_azar,
                           MoreArgs= list ( dtrain, param=x, qfolds= xval_folds, pagrupa= "clase_ternaria" ),
                           SIMPLIFY= FALSE,
                           mc.cores = 5 )  #debe ir 1 si es Windows


   ganancia_promedio  <- mean( unlist( vganancias ) )
   #logueo 
   xx  <- x
   xx$xval_folds  <-  xval_folds
   xx$ganancia  <- ganancia_promedio
   xx$iteracion <- GLOBAL_iteracion
   loguear( xx,  arch= archivo_log )

   return( xx$ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd( "~/buckets/b1/" )

#cargo el dataset, aqui debe poner  SU RUTA
dataset  <- fread("./datasets/competencia1_2022.csv")   #donde entreno

#creo la clase_binaria  SI= {BAJA+1, BAJA+2}  NO={CONTINUA}
dataset[ foto_mes==202101, clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

#-------------------------------------------------------------#
#-------------------- FEATURE ENGINEERING --------------------#
#-------------------------------------------------------------#

# Feature Engineering del tipo AX + BY, aplicado a columnas asociadas a la
# tarjeta del cliente (Master)
# dataset[, master_fe_suma_all := 
#           # Master_mfinanciacion_limite +
#           Master_msaldototal +
#           Master_msaldopesos + Master_msaldodolares +
#           Master_mconsumospesos + Master_mconsumosdolares +
#           Master_mlimitecompra +
#           Master_madelantopesos +
#           Master_madelantodolares + 
#           Master_mpagado +
#           Master_mpagospesos +
#           Master_mpagosdolares + Master_mconsumototal + Master_mpagominimo
# ]

# Feature Engineering del tipo AX + BY, aplicado a columnas asociadas a la
# tarjeta del cliente (Visa)
# dataset[, visa_fe_suma_all := Visa_mfinanciacion_limite +
#           # Visa_msaldototal +
#           Visa_msaldopesos +
#           Visa_msaldodolares + Visa_mconsumospesos +
#           Visa_mconsumosdolares +
#           Visa_mlimitecompra +
#           Visa_madelantopesos +
#           Visa_madelantodolares +
#           Visa_mpagado +
#           Visa_mpagospesos +
#           Visa_mpagosdolares + Visa_mconsumototal
#         + Visa_mpagominimo
#         ]

# Feature Engineering del tipo AX + BY, aplicado a columnas asociadas a las
# tarjetas del cliente (Master + Visa)
# dataset[, tarjetas_fe_suma_all := master_fe_suma_all + visa_fe_suma_all]


# Feature Engineering del tipo AX + BY, aplicado a todas las columnas en pesos
# salvo las tarjetas
dataset[, pesos_fe_suma_menos_tarjetas := 
          mrentabilidad +
          mrentabilidad_annual +
          mcomisiones +
          mactivos_margen +
          mpasivos_margen +
          mcuenta_corriente_adicional + 
          mcuenta_corriente +
          mcaja_ahorro +
          mcaja_ahorro_adicional +
          mcaja_ahorro_dolares +
          mcuentas_saldo +
          mautoservicio + mtarjeta_visa_consumo +
          mtarjeta_master_consumo +
          mprestamos_personales + mprestamos_prendarios +
          mprestamos_hipotecarios + mplazo_fijo_dolares + mplazo_fijo_pesos +
          minversion1_pesos +
          minversion1_dolares + minversion2 + 
          mpayroll +
          mpayroll2 + 
          mcuenta_debitos_automaticos +
          mttarjeta_master_debitos_automaticos + mpagodeservicios +
          mpagomiscuentas +
          mcajeros_propios_descuentos +
          mtarjeta_visa_descuentos + mtarjeta_master_descuentos +
          mcomisiones_mantenimiento +
          mcomisiones_otras +
          mforex_buy +
          mforex_sell +
          mtransferencias_recibidas +
          mtransferencias_emitidas +
          mextraccion_autoservicio +
          mcheques_depositados + mcheques_emitidos +
          mcheques_depositados_rechazados + mcheques_emitidos_rechazados +
          matm + matm_other
]

# Feature Engineering del tipo AX + BY, aplicado a todas las columnas en pesos
# dataset[, pesos_fe_suma_all :=
#           pesos_fe_suma_menos_tarjetas +
#           tarjetas_fe_suma_all
# ]

# Feature Engineering del tipo A/B, aplicado a variables más importantes para el
# modelo, pero que SI estén en el gráfico, y performen mejor que canarios
# dataset[, cociente_fe_01 := ctrx_quarter/mcuentas_saldo]
dataset[, cociente_fe_02 := ctrx_quarter/mcomisiones]
# dataset[, cociente_fe_03 := mcuentas_saldo/mcomisiones]

#-------------------------------------------------------------------#
#-------------------- Divido en train y testing --------------------#
#-------------------------------------------------------------------#

#defino los datos donde entreno
dtrain  <- dataset[ foto_mes==202101, ]


#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/HT0909/", showWarnings = FALSE )
setwd("./exp/HT0909/")   #Establezco el Working Directory DEL EXPERIMENTO

#defino los archivos donde guardo los resultados de la Bayesian Optimization
archivo_log  <- "HT0909.txt"
archivo_BO   <- "HT0909.RDATA"

#leo si ya existe el log, para retomar en caso que se se corte el programa
GLOBAL_iteracion  <- 0

if( file.exists(archivo_log) )
{
 tabla_log  <- fread( archivo_log )
 GLOBAL_iteracion  <- nrow( tabla_log )
}



#Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar  <- EstimarGanancia

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar,
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,
              has.simple.signature = FALSE   #espia Tomas Delvechio, dejar este parametro asi
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= archivo_BO)
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( archivo_BO ) ) {

  run  <- mbo( fun=     obj.fun, 
               learner= surr.km,
               control= ctrl)

} else  run  <- mboContinue( archivo_BO )   #retomo en caso que ya exista

