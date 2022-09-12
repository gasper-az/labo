rm(list=ls())
gc()

require("data.table")
require("rpart")
require("rpart.plot")

#--------------------------------------------------------#
#-------------------- PATH y SEMILLA --------------------#
#--------------------------------------------------------#

working.directory <- "C:\\uba\\dmeyf"
semilla <- 763369

setwd(working.directory)

#----------------------------------------------------------#
#-------------------- Carga de dataset --------------------#
#----------------------------------------------------------#

dataset  <- fread("./datasets/competencia1_2022.csv" )

#------------------------------------------------------------#
#-------------------- Creo clase binaria --------------------#
#------------------------------------------------------------#

dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

#-------------------------------------------------------------#
#-------------------- FEATURE ENGINEERING --------------------#
#-------------------------------------------------------------#

# Feature Engineering del tipo AX + BY, aplicado a columnas asociadas a la
# tarjeta del cliente (Master)
dataset[, master_fe_suma_all := 
          # Master_mfinanciacion_limite +
          Master_msaldototal +
          Master_msaldopesos + Master_msaldodolares +
          Master_mconsumospesos + Master_mconsumosdolares +
          Master_mlimitecompra +
          Master_madelantopesos +
          Master_madelantodolares + 
          Master_mpagado +
          Master_mpagospesos +
          Master_mpagosdolares + Master_mconsumototal + Master_mpagominimo
]

# Feature Engineering del tipo AX + BY, aplicado a columnas asociadas a la
# tarjeta del cliente (Visa)
dataset[, visa_fe_suma_all := Visa_mfinanciacion_limite +
          # Visa_msaldototal +
          Visa_msaldopesos +
          Visa_msaldodolares + Visa_mconsumospesos +
          Visa_mconsumosdolares +
          Visa_mlimitecompra +
          Visa_madelantopesos +
          Visa_madelantodolares +
          Visa_mpagado +
          Visa_mpagospesos +
          Visa_mpagosdolares + Visa_mconsumototal
        + Visa_mpagominimo
        ]

# Feature Engineering del tipo AX + BY, aplicado a columnas asociadas a las
# tarjetas del cliente (Master + Visa)
dataset[, tarjetas_fe_suma_all := master_fe_suma_all + visa_fe_suma_all]


# Feature Engineering del tipo AX + BY, aplicado a todas las columnas en pesos
# salvo las tarjetas
dataset[, pesos_fe_suma_menos_tarjetas := 
          # mrentabilidad +
          # mrentabilidad_annual +
          mcomisiones +
          # mactivos_margen +
          # mpasivos_margen +
          mcuenta_corriente_adicional + 
          # mcuenta_corriente +
          # mcaja_ahorro +
          mcaja_ahorro_adicional +
          # mcaja_ahorro_dolares + 
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
          # mcomisiones_mantenimiento +
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
dataset[, pesos_fe_suma_all :=
          pesos_fe_suma_menos_tarjetas +
          tarjetas_fe_suma_all
]

# Feature Engineering del tipo A/B, aplicado a variables más importantes para el
# modelo, pero que SI estén en el gráfico, y performen mejor que canarios
dataset[, cociente_fe_01 := ctrx_quarter/mcuentas_saldo]
dataset[, cociente_fe_02 := ctrx_quarter/mcomisiones]
dataset[, cociente_fe_03 := mcuentas_saldo/mcomisiones]

#-------------------------------------------------------------------#
#-------------------- Divido en train y testing --------------------#
#-------------------------------------------------------------------#

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#---------------------------------------------------------------------------------------------#
#-------------------- Marco variables en la que identifiqué Data Drifting --------------------#
#---------------------------------------------------------------------------------------------#

## Fiscal, vea el archivo "003_analisis_data_drifting.R"
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
  variables.sacar,
  "cliente_edad",
  "numero_de_cliente",
  "Master_mfinanciacion_limite",
  "mpasivos_margen",
  "ccajas_extracciones",
  "Visa_msaldototal",
  "mcuenta_corriente",
  "ctarjeta_master_debitos_automaticos",
  "ccallcenter_transacciones",
  "ctarjeta_master_transacciones",
  "mttarjeta_visa_debitos_automaticos",
  "mactivos_margen",
  "Master_fechaalta",
  "Master_Fvencimiento",
  "mcaja_ahorro_dolares",
  "mrentabilidad",
  "cliente_antiguedad",
  "mcaja_ahorro",
  "Visa_status",
  "Visa_Fvencimiento",
  "Visa_fechaalta",
  "mrentabilidad_annual",
  "mcomisiones_mantenimiento"
)

#-----------------------------------------------------------------#
#-------------------- Creo fórmula del modelo --------------------#
#-----------------------------------------------------------------#

formula.modelo <- "clase_binaria ~ ."

for (variable in variables.sacar) {
  formula.modelo <- paste(formula.modelo, variable, sep = " -")
}

#--------------------------------------------------------#
#-------------------- Creo el modelo --------------------#
#--------------------------------------------------------#

modelo  <- rpart(
  formula  = formula.modelo,
  data     = dtrain,
  xval     = 0,
  cp       = -0.56159657,
  minsplit = 1485,
  minbucket= 129,
  maxdepth = 7
)

#-------------------------------------------------------------------------------#
#-------------------- Aplico modelo a datos nuevos (202103) --------------------#
#-------------------------------------------------------------------------------#

prediccion  <- predict( object=  modelo,
                        newdata= dapply,
                        type = "prob")

#---------------------------------------------------------------------------------#
#-------------------- Creo columna con probabilidad de BAJA+2 --------------------#
#---------------------------------------------------------------------------------#

#prediccion es una matriz con DOS columnas, llamadas "NO", "SI"
#cada columna es el vector de probabilidades 
#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dfinal  <- copy(dapply[, list(numero_de_cliente)])
dfinal[, prob_SI := prediccion[, "SI"]]
set.seed(semilla)
dfinal[, azar := runif(nrow(dapply))]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder(dfinal, -prob_SI, azar)

#-------------------------------------------------------------------------------------------------------------------------#
#-------------------- Grabo salida para kaggle, en función de los cortes y la probabilidad de prob_si --------------------#
#-------------------------------------------------------------------------------------------------------------------------#

dir.create( "./exp/" )
dir.create( "./exp/HT0909" )
dir.create( "./exp/HT0909/v1.0.5" )

for(corte in c(7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000)) {
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]
  
  fwrite(
    dfinal[, list(numero_de_cliente, Predicted)], #solo los campos para Kaggle
    file= paste0("./exp/HT0909/v1.0.5/KA4120_005_", corte, ".csv"),
    sep=  ","
  )
}

#-------------------------------------------------------------------#
#-------------------- Guardo el modelo como PDF --------------------#
#-------------------------------------------------------------------#

pdf(file = "./exp/HT0909/v1.0.5/rpart.pdf", width=28, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()