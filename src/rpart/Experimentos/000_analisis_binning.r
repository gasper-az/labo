#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

library(data.table)
library(binr)
library(lsr)

# TODO: cambiar path
setwd("C:\\uba\\dmeyf\\")

# cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")

dataset.enero <- dataset[ foto_mes == 202101 ]
dataset.marzo <- dataset[ foto_mes == 202103 ]

# unique.enero <- dataset.enero[, lapply(.SD, uniqueN), by = .SD]
# rownames(unique.enero) <- c("uniques")
# 
# range.enero <- dataset.enero[, lapply(.SD, range, na.rm = T), by = .SD]
# rownames(range.enero) <- c("min", "max")
# 
# transpose.range.enero <- transpose(range.enero)
# colnames(transpose.range.enero) <- rownames(range.enero)
# transpose.range.enero$colnames <- colnames(range.enero)
# 
# transpose.unique.enero <- transpose(unique.enero)
# colnames(transpose.unique.enero) <- rownames(unique.enero)
# transpose.unique.enero$colnames <- colnames(unique.enero)
# 
# merge.enero <- merge(transpose.unique.enero, transpose.range.enero, all = T)
# ordered.enero <- merge.enero[order(-rank(uniques))]
# 
# cols.interes.enero <- ordered.enero[uniques >= 15000, ]
# cols.interes.enero


# Creo función para aplicar binning al dataset (subset, es decir, 202101 o 202103)
agregar.bins <- function(subset, col, probs = NULL) {
  if (is.null(probs)) {
    probs <- seq(0, 1, by = 0.25)
  }
  
  labels.bins <- 1:(length(probs) - 1)
  var.quantiles <- subset[, quantile(get(col), probs = probs, na.rm = T)]
  new.col <- paste(col, "bin", sep = "__")
  
  subset[, (new.col) := cut(
    get(col),
    var.quantiles,
    include.lowest = T,
    labels.bins
  )]
}

# agregar.bins(dataset.enero, "mrentabilidad_annual", seq(0,1, by=0.1))

# Variables más importantes, corresponden al modelo de salida del scrip 412_rpart_binaria_aplicar
variables.analizar <- c(
  "ctrx_quarter",
  "mcuentas_saldo",
  "cdescubierto_preacordado",
  "active_quarter",
  "mcuenta_corriente",
  "cprestamos_personales",
  "mprestamos_personales",
  "mcaja_ahorro",
  "mactivos_margen",
  "ccomisiones_otras",
  "mpasivos_margen",
  "mtarjeta_visa_consumo",
  "cliente_antiguedad",
  "mcomisiones",
  "mcomisiones_otras",
  "Visa_msaldototal",
  "Visa_fechaalta",
  "Visa_msaldopesos",
  "ctarjeta_visa_transacciones",
  "Master_fechaalta"    
)

for (variable.interes in variables.analizar) {
  unique.enero <- dataset.enero[, uniqueN(get(variable.interes))]
  range.enero <- dataset.enero[, range(get(variable.interes), na.rm = T)]
  length.enero <- dataset.enero[, length(get(variable.interes))]
  
  unique.marzo <- dataset.marzo[, uniqueN(get(variable.interes))]
  range.marzo <- dataset.marzo[, range(get(variable.interes), na.rm = T)]
  length.marzo <- dataset.marzo[, length(get(variable.interes))]
  
  unique.all <- dataset[, uniqueN(get(variable.interes))]
  range.all <- dataset[, range(get(variable.interes), na.rm = T)]
  length.all <- dataset[, length(get(variable.interes))]
  
  to.print <- paste(
    "====================================================\n",
    variable.interes, ": \n",
    "\tUniques en enero:\t", unique.enero, "\n",
    # "\tlength en enero:\t", length.enero, "\n",
    "\tRange en enero:\n\t\t", "min: ", range.enero[1], "\n\t\tmax: ", range.enero[2], "\n",
    
    "\tUniques en marzo:\t", unique.marzo, "\n",
    # "\tlength en marzo:\t", length.marzo, "\n",
    "\tRange en marzo:\n\t\t", "min: ", range.marzo[1], "\n\t\tmax: ", range.marzo[2], "\n",
    
    "\tUniques en total:\t", unique.all, "\n",
    # "\tlength en all:\t", length.all, "\n",
    "\tRange en total:\n\t\t", "min: ", range.all[1], "\n\t\tmax: ", range.all[2], "\n",
    "====================================================\n",
    sep = ""
  )
  
  cat(to.print)
}


variables.para.binning <- c(
  "mcuentas_saldo",
  "mcuenta_corriente",
  "mprestamos_personales",
  "mcaja_ahorro",
  "mactivos_margen",   ## ???
  "mpasivos_margen",
  "mtarjeta_visa_consumo",
  "mcomisiones",       ## ???
  "mcomisiones_otras", ## ???
  "Visa_msaldototal",
  "Visa_msaldopesos"
)