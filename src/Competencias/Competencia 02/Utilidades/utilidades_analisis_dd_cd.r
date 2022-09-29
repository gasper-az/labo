#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

# `%notin%` <- Negate(`%in%`)
# if ("modeest" %notin% installed.packages()) {
#   install.packages("modeest")
# }
# 
# library(modeest)

analizar.variable <- function(dataset, var) {
  count.pos   <- dataset[ get(var) > 0, .N]
  count.neg   <- dataset[ get(var) < 0, .N]
  count.cero  <- dataset[ get(var) == 0, .N]
  count.na    <- dataset[ is.na(get(var)) == T, .N]
  count.na.03 <- dataset[ is.na(get(var)) == T & foto_mes == 202103, .N]
  count.na.05 <- dataset[ is.na(get(var)) == T & foto_mes == 202105, .N]
  
  cat("========================================", 
      "\n", var,
      "\n\tPositivos:\t", count.pos,
      "\n\tNegativos:\t", count.neg,
      "\n\tCeros:\t", count.cero,
      "\n\tNAs total:\t", count.na,
      "\n\tNAs Marzo:\t", count.na.03,
      "\n\tNAs: Mayo\t", count.na.05, "\n")
}


#Aqui comienza el programa
setwd("C:\\uba\\dmeyf")

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/competencia2_2022.csv.gz")

variables.a.analizar <- c(
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

# for (var in variables.a.analizar) {
#   analizar.variable(dataset, var)
# }

rank.prefix <- "ranked_"
for (var in variables.a.analizar) {
  dataset[, paste0(rank.prefix, var) := frankv(dataset, cols = var, na.last = TRUE, ties.method = "dense")]
  dataset[, (var) := NULL]
}
