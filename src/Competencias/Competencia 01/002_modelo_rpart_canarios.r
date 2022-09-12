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
#------------------- AGREGO N VARIABLES CANARIOS -------------------#
#-------------------------------------------------------------------#

cantidad.canarios <- 30

for(i in 1:cantidad.canarios) {
  dataset[, paste0("canarito", i) :=  runif(nrow(dataset))]
}

#-------------------------------------------------------------------#
#-------------------- Divido en train y testing --------------------#
#-------------------------------------------------------------------#

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

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
  "clase_ternaria"
  ,
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

# modelo.original  <- rpart(
#   formula  = formula.modelo,
#   data     = dtrain,
#   xval     = 0,
#   cp       = -1,
#   minsplit = 2,
#   minbucket= 1,
#   maxdepth = 10
# )

# modelo.original  <- rpart(
#   formula  = formula.modelo,
#   data     = dtrain,
#   xval     = 0,
#   cp       = -0.56159657,
#   minsplit = 1485,
#   minbucket= 129,
#   maxdepth = 10
# )

# modelo.original  <- rpart(
#   formula  = formula.modelo,
#   data     = dtrain,
#   xval     = 0,
#   cp       = -0.56159657,
#   minsplit = 1485,
#   minbucket= 129,
#   maxdepth = 8
# )

# modelo.original  <- rpart(
#   formula  = formula.modelo,
#   data     = dtrain,
#   xval     = 0,
#   cp       = -0.816404835,
#   minsplit = 1629,
#   minbucket= 814,
#   maxdepth = 30
# )

#---------------------------------------------------------------------------#
#-------------------- Hago Prunning del modelo original --------------------#
#---------------------------------------------------------------------------#

modelo.original$frame[modelo.original$frame$var %like% "canarito", "complexity"] <- -666
modelo.pruned <- prune(modelo.original, -666)

#-----------------------------------------------------------------------#
#-------------------- Guardo ambos árboles como PDF --------------------#
#-----------------------------------------------------------------------#

entrega.directory <- "C:/uba/repos/labo/src/rpart/Entrega/Salida BO/HT0909/"
setwd(entrega.directory)

dir.create("./v1.0.7")
dir.create("./v1.0.7/Canarios")

pdf(file = "./v1.0.7/Canarios/canaritos_unprunned.pdf", width=28, height=4)
prp(modelo.original, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

pdf(file = "./v1.0.7/Canarios/canaritos_prunned.pdf", width=28, height=4)
prp(modelo.pruned, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
