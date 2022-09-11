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
dataset[, master_fe_suma_all := Master_mfinanciacion_limite +
          Master_msaldototal + Master_msaldopesos + Master_msaldodolares +
          Master_mconsumospesos + Master_mconsumosdolares +
          Master_mlimitecompra + Master_madelantopesos +
          Master_madelantodolares + 
          # Master_mpagado + 
          Master_mpagospesos +
          Master_mpagosdolares + Master_mconsumototal + Master_mpagominimo
]

# Feature Engineering del tipo AX + BY, aplicado a columnas asociadas a la
# tarjeta del cliente (Visa)
# dataset[, visa_fe_suma_all := Visa_mfinanciacion_limite + 
#           # Visa_msaldototal +
#           # Visa_msaldopesos +
#           Visa_msaldodolares + Visa_mconsumospesos +
#           Visa_mconsumosdolares + Visa_mlimitecompra + Visa_madelantopesos +
#           Visa_madelantodolares + Visa_mpagado + 
#           # Visa_mpagospesos +
#           Visa_mpagosdolares + Visa_mconsumototal 
#         # + Visa_mpagominimo
#         ]

# Feature Engineering del tipo AX + BY, aplicado a columnas asociadas a las
# tarjetas del cliente (Master + Visa)
# dataset[, tarjetas_fe_suma_all := master_fe_suma_all + visa_fe_suma_all]


# Feature Engineering del tipo AX + BY, aplicado a todas las columnas en pesos
# salvo las tarjetas
dataset[, pesos_fe_suma_menos_tarjetas := mrentabilidad + 
          mrentabilidad_annual +
          # mcomisiones +
          mactivos_margen + 
          # mpasivos_margen +
          mcuenta_corriente_adicional + 
          # mcuenta_corriente + 
          mcaja_ahorro +
          # mcaja_ahorro_adicional + 
          mcaja_ahorro_dolares + 
          # mcuentas_saldo +
          mautoservicio + mtarjeta_visa_consumo +
          mtarjeta_master_consumo +
          mprestamos_personales + mprestamos_prendarios +
          mprestamos_hipotecarios + mplazo_fijo_dolares + mplazo_fijo_pesos +
          # minversion1_pesos + 
          minversion1_dolares + minversion2 + 
          # mpayroll +
          mpayroll2 + 
          # mcuenta_debitos_automaticos +
          mttarjeta_master_debitos_automaticos + mpagodeservicios +
          mpagomiscuentas + mcajeros_propios_descuentos +
          mtarjeta_visa_descuentos + mtarjeta_master_descuentos +
          mcomisiones_mantenimiento + 
          # mcomisiones_otras + 
          mforex_buy +
          mforex_sell + 
          # mtransferencias_recibidas + 
          # mtransferencias_emitidas +
          mextraccion_autoservicio + mcheques_depositados + mcheques_emitidos +
          mcheques_depositados_rechazados + mcheques_emitidos_rechazados +
          matm + matm_other
]

# Feature Engineering del tipo AX + BY, aplicado a todas las columnas en pesos
dataset[, pesos_fe_suma_all :=
          pesos_fe_suma_menos_tarjetas +
          master_fe_suma_all
]

# Feature Engineering del tipo A/B
# Aplico sobre 10 Var + importantes (de modelo anterior - v1.0), pero
# sacando aquellas que considero que performan peor que canarios
dataset[, cociente_fe_01 := pesos_fe_suma_menos_tarjetas/mtarjeta_visa_consumo]
dataset[, cociente_fe_02 := pesos_fe_suma_menos_tarjetas/ctarjeta_visa_transacciones]
dataset[, cociente_fe_03 := pesos_fe_suma_menos_tarjetas/mactivos_margen]
dataset[, cociente_fe_04 := pesos_fe_suma_menos_tarjetas/cdescubierto_preacordado]
dataset[, cociente_fe_05 := pesos_fe_suma_menos_tarjetas/ctrx_quarter]
dataset[, cociente_fe_06 := mtarjeta_visa_consumo/ctarjeta_visa_transacciones]
dataset[, cociente_fe_07 := mtarjeta_visa_consumo/mactivos_margen]
dataset[, cociente_fe_08 := mtarjeta_visa_consumo/cdescubierto_preacordado]
dataset[, cociente_fe_09 := mtarjeta_visa_consumo/ctrx_quarter]
dataset[, cociente_fe_10 := ctarjeta_visa_transacciones/mactivos_margen]
dataset[, cociente_fe_11 := ctarjeta_visa_transacciones/cdescubierto_preacordado]
dataset[, cociente_fe_12 := ctarjeta_visa_transacciones/ctrx_quarter]
dataset[, cociente_fe_13 := mactivos_margen/cdescubierto_preacordado]
dataset[, cociente_fe_14 := mactivos_margen/ctrx_quarter]
dataset[, cociente_fe_15 := cdescubierto_preacordado/ctrx_quarter]

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
  "Visa_mpagado",
  "mcomisiones_mantenimiento"
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
  "cproductos",
  "mpayroll",
  "cliente_edad",
  "mcuenta_debitos_automaticos",
  "ctransferencias_emitidas",
  "Master_mpagado",
  "cpagomiscuentas",
  "mcomisiones",					
  "numero_de_cliente",
  "Visa_msaldototal",			
  "mcuenta_corriente",
  "minversion1_pesos",
  "ctarjeta_master_debitos_automaticos",
  "mtransferencias_recibidas",
  "mcaja_ahorro_adicional",
  "Master_fechaalta",
  "mpasivos_margen",					
  "mcuentas_saldo",					
  "mtransferencias_emitidas",
  "Visa_mpagospesos",
  
  "Visa_msaldopesos",   ## APARECÍAN COMO IMPORTANTES, PERO NO APARECÍAN EN EL GRÁFICO DEL ÁRBOL
  "mcomisiones_otras",
  "active_quarter",
  "ccomisiones_mantenimiento",
  "tcuentas",
  "Visa_mpagominimo"
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
#   maxdepth = 30
# )

# modelo.original  <- rpart(
#   formula  = formula.modelo,
#   data     = dtrain,
#   xval     = 0,
#   cp       = -0.7422262,
#   minsplit = 1205,
#   minbucket= 314,
#   maxdepth = 8  # MAX DEPTH ORIGINAL (la que salió del HO del exp anterior)
# )

modelo.original  <- rpart(
  formula  = formula.modelo,
  data     = dtrain,
  xval     = 0,
  cp       = -0.7422262,
  minsplit = 1205,
  minbucket= 314,
  maxdepth = 30
)

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

dir.create("./v1.0.3")
dir.create("./v1.0.3/Canarios")

pdf(file = "./v1.0.3/Canarios/canaritos_unprunned.pdf", width=28, height=4)
prp(modelo.original, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

pdf(file = "./v1.0.3/Canarios/canaritos_prunned.pdf", width=28, height=4)
prp(modelo.pruned, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
