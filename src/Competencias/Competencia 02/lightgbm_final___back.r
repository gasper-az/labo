# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "KA7246"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

# fecha	objective	metric	first_metric_only	boost_from_average	feature_pre_filter	verbosity	max_bin	num_iterations	force_row_wise	seed	learning_rate	feature_fraction	min_data_in_leaf	num_leaves	envios	bagging_fraction	bagging_freq	lambda_l1	lambda_l2	min_gain_to_split	max_depth 	ganancia	iteracion
# 20220927 022734	binary	custom	TRUE	TRUE	FALSE	-100	31	97	TRUE	763381	0.089556394	0.445734265	1153	436	8476	0.665748464	29	0.01010077	16.26989805	0.078369826	3	26970000	89

PARAM$finalmodel$max_bin           <- 31
PARAM$finalmodel$learning_rate     <- 0.0450579481474516
PARAM$finalmodel$num_iterations    <- 144
PARAM$finalmodel$num_leaves        <- 21
PARAM$finalmodel$min_data_in_leaf  <- 5094
PARAM$finalmodel$feature_fraction  <- 0.347009781655412
PARAM$finalmodel$semilla           <- 763369

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
setwd( "C:\\uba\\dmeyf" )

#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)


#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#-------------------------------------------------------------#
#------------- RANKING DE VARIABLES CON DRIFTING -------------#
#-------------------------------------------------------------#

variables.drifting.ranking <- c(
  "mpasivos_margen"
  ,"mcuenta_corriente"
  ,"mcaja_ahorro_dolares"
  ,"mtarjeta_visa_consumo"
  ,"mcuenta_debitos_automaticos"
  ,"chomebanking_transacciones"
  ,"ccajas_otras"
  ,"Visa_mconsumospesos"
  ,"Visa_madelantodolares"
  ,"Visa_mpagosdolares"
  ,"Visa_mconsumototal"
)

variables.drifting.ranking.pos.neg.cero <- c(
  "mcomisiones"
  ,"mcaja_ahorro"
  ,"mcuentas_saldo"
  ,"ccuenta_debitos_automaticos"
  ,"ccomisiones_otras"
  ,"mcomisiones_otras"
  ,"Master_mfinanciacion_limite"
  ,"Master_Finiciomora"
  ,"Master_fultimo_cierre"
  ,"Visa_Finiciomora"
  ,"Visa_msaldopesos"
  ,"Visa_fultimo_cierre"
  ,"Visa_mpagado"
  ,"Visa_msaldototal"
  ,"mrentabilidad"
  ,"mrentabilidad_annual"
  ,"mactivos_margen"
)

rank.prefix <- "r_"

for (var in variables.drifting.ranking) {
  new.var.name <- paste0(rank.prefix, var)
  dataset[, (new.var.name) := (frankv(dataset, cols = var, na.last = TRUE, ties.method = "dense") - 1) / (.N - 1)]
  dataset[, (var) := NULL]
}

for (var in variables.drifting.ranking.pos.neg.cero) {
  new.var.name <- paste0(rank.prefix, var)
  dataset[, (new.var.name) := ifelse(var >= 0,
                                     (ifelse(var > 0,
                                             (frankv(dataset, cols = var, na.last = TRUE, ties.method = "dense") - 1) / (.N - 1), ## mayores a cero
                                             0)), # cero
                                     -(frankv(dataset, cols = var, na.last = TRUE, ties.method = "dense") - 1) / (.N - 1) ## menores a cero
  )
  ]
  dataset[, (var) := NULL]
}

#-------------------------------------------------------------#
#----------- FIN RANKING DE VARIABLES CON DRIFTING -----------#
#-------------------------------------------------------------#

#-------------------------------------------------------------#
#-------------------- FEATURE ENGINEERING --------------------#
#-------------------------------------------------------------#

dataset[, master_fe_suma_all := 
          r_Master_mfinanciacion_limite +
          Master_msaldototal + Master_msaldopesos + Master_msaldodolares +
          Master_mconsumospesos + Master_mconsumosdolares + Master_mlimitecompra +
          Master_madelantopesos + Master_madelantodolares + Master_mpagado +
          Master_mpagospesos + Master_mpagosdolares + Master_mconsumototal +
          Master_mpagominimo
]

dataset[, visa_fe_suma_all := Visa_mfinanciacion_limite +
          r_Visa_msaldototal +
          r_Visa_msaldopesos + Visa_msaldodolares + r_Visa_mconsumospesos +
          Visa_mconsumosdolares + Visa_mlimitecompra + Visa_madelantopesos +
          r_Visa_madelantodolares + r_Visa_mpagado + Visa_mpagospesos +
          r_Visa_mpagosdolares + r_Visa_mconsumototal + Visa_mpagominimo
]

dataset[, tarjetas_fe_suma_all := master_fe_suma_all + visa_fe_suma_all]

dataset[, pesos_fe_suma_menos_tarjetas := 
          r_mrentabilidad + r_mrentabilidad_annual + r_mactivos_margen +
          mcomisiones_mantenimiento + r_mpasivos_margen + r_mcuenta_corriente +
          r_mcaja_ahorro + r_mcaja_ahorro_dolares + r_mcomisiones + mcuenta_corriente_adicional + mcaja_ahorro_adicional +          
          r_mcuentas_saldo + mautoservicio + r_mtarjeta_visa_consumo +
          mtarjeta_master_consumo + mprestamos_personales + mprestamos_prendarios +
          mprestamos_hipotecarios + mplazo_fijo_dolares + mplazo_fijo_pesos +
          minversion1_pesos + minversion1_dolares + minversion2 + 
          mpayroll + mpayroll2 + r_mcuenta_debitos_automaticos +
          mttarjeta_master_debitos_automaticos + mpagodeservicios + mpagomiscuentas +
          mcajeros_propios_descuentos + mtarjeta_visa_descuentos + mtarjeta_master_descuentos +
          r_mcomisiones_otras + mforex_buy + mforex_sell +
          mtransferencias_recibidas + mtransferencias_emitidas + mextraccion_autoservicio +
          mcheques_depositados + mcheques_emitidos + mcheques_depositados_rechazados +
          mcheques_emitidos_rechazados + matm + matm_other
]

dataset[, pesos_fe_suma_all :=
          pesos_fe_suma_menos_tarjetas +
          tarjetas_fe_suma_all
]

dataset[, cociente_fe_01 := ctrx_quarter/r_mcuentas_saldo]
dataset[, cociente_fe_02 := ctrx_quarter/r_mcomisiones]
dataset[, cociente_fe_03 := r_mcuentas_saldo/r_mcomisiones]

#-------------------------------------------------------------#
#------------------ FIN FEATURE ENGINEERING ------------------#
#-------------------------------------------------------------#

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO



#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=            PARAM$finalmodel$max_bin,
                                   learning_rate=      PARAM$finalmodel$learning_rate,
                                   num_iterations=     PARAM$finalmodel$num_iterations,
                                   num_leaves=         PARAM$finalmodel$num_leaves,
                                   min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                   feature_fraction=   PARAM$finalmodel$feature_fraction,
                                   seed=               PARAM$finalmodel$semilla
                      )
)

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "impo.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------


#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== PARAM$input$future ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix(dapply[, campos_buenos, with=FALSE ]))

#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 5000, 12000, by=500 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]
  
  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  PARAM$experimento, "_", envios, ".csv" ),
          sep= "," )
}

#--------------------------------------

quit( save= "no" )