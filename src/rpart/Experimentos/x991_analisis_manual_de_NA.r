rm(list = ls())
gc()

require("data.table")
base.path <- "C:\\uba\\dmeyf\\"
dataset.path <- "./datasets/competencia1_2022.csv"

setwd(base.path)
dataset <- fread(dataset.path)

variables.interes <- c(
  "ctrx_quarter",
  "mpayroll",
  "mtarjeta_visa_consumo",
  "ccaja_ahorro",
  "numero_de_cliente",
  "mautoservicio",
  "mtransferencias_recibidas",
  "mcuentas_saldo",
  "Visa_msaldopesos",
  "mcaja_ahorro",
  "tcallcenter",
  "Visa_msaldopesos",
  "cliente_edad",
  "cpayroll_trx",
  "mcheques_depositados",
  "mtarjeta_master_consumo",
  "mpasivos_margen",
  "Visa_msaldototal",
  "mrentabilidad_annual",
  "Visa_mpagominimo",
  "Visa_msaldodolares",
  "Visa_mconsumospesos",
  "mcomisiones",
  "ctarjeta_visa_transacciones",
  "ctarjeta_visa",
  "Visa_mpagospesos",
  "chomebanking_transacciones",
  "Visa_mfinanciacion_limite",
  "mttarjeta_visa_debitos_automaticos",
  "cproductos",
  "ctarjeta_master"
)


for (variable.interes in unique(variables.interes)) {
  n <- dataset[is.na(variable.interes), .N]
  
  cat(variable.interes, " ---> ", n, "\n", sep = "")
}
