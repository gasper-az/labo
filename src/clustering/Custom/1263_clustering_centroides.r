#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
library(ggplot2)
library(hrbrthemes)
library(dplyr)

graficar.barplot <- function(dataset, cluster.var, var.to.plot, values) {
  gg <- ggplot(dataset, aes(x = factor(get(cluster.var))  , y = get(var.to.plot), fill = factor(get(cluster.var)))) +
    xlab("Número de Cluster") +
    ylab(paste0("Variable: ", var.to.plot)) +
    scale_fill_manual("Cluster", values = values) +
    geom_bar(stat = "identity")
    # geom_text(aes(label=(get(var))), vjust=1.6, color="black", size=3.5)
  
  return(gg)
}
###################################################################
#################### Aquí comienza el programa ####################
###################################################################

#TODO: setear el working directory
base.dir <- "C:/uba/dmeyf/exp/"
#TODO: setear el nombre de la carpeta de salida de experimentos de clustering 1261 o 1262
base.folder <- "CLU1261_10K"

setwd(paste(base.dir,base.folder, sep = "/"))

#TODO: setear la semilla
set.seed(763369)

file <- "cluster_de_bajas.txt"  #TODO: setear el nombre del archivo de salida de experimentos de clustering 1261 o 1262

dataset  <- fread(file, stringsAsFactors= TRUE)

cluster.colors <- c( "1" = "#42687C", "2" = "#236E96", "3" = "#84A5B8", "4" = "#15B2D3", "5" = "#FFD700", "6" = "#F3872F", "7" = "#FF598F")

campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_transacciones",
                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
                     "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos",
                     "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
                     "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
                     "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
                     "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
                     "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
                     "mpagomiscuentas")


dt.centroides <- data.table(variable = character(), mean = double(), sd = double(), count.2.sd.mean = integer())

for (var in campos_buenos) {
  data <- dataset[, mean(get(var)), cluster2]
  mean.v1 <- mean(data$V1)
  sd.v1 <- sd(data$V1)
  upper.mean.sd <- mean.v1 + (2 * sd.v1)
  lower.mean.sd <- mean.v1 - (2 * sd.v1)
  
  count.upper.lower <- data[ V1 < lower.mean.sd | V1 > upper.mean.sd, .N]
  
  dt.centroides <- rbind(dt.centroides, list(var, mean.v1, sd.v1, count.upper.lower))
}

dt.centroides <- dt.centroides[order(-rank(count.2.sd.mean))]

var.interes <- dt.centroides[count.2.sd.mean > 0, variable]
var.interes <- c(var.interes, "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mcuentas_saldo")


# pdf("var_interes.pdf")

for (var in var.interes) {
  cat(var, "\n")
  data <- dataset[, mean(get(var)), cluster2]
  data <- data[order(-rank(V1))]
  print(data)
  
  # dataset.reducido <- dataset[, max(get(var)), by = cluster2]
  # dataset.reducido <- dataset.reducido[, (var) := V1]
  # dataset.reducido <- dataset.reducido[, V1 := NULL]
  
  # print(graficar.barplot(dataset = dataset, cluster.var = "cluster2", var.to.plot = var, values = cluster.colors))
}

# dev.off()


library(fmsb)

cant.clusters <- length(unique(dataset[, cluster2]))
cant.var.interes <- length(var.interes)

data.radar <- as.data.frame(matrix(rep(0, cant.clusters * cant.var.interes), ncol = cant.var.interes))
colnames(data.radar) <- var.interes
rownames(data.radar) <- paste("Cluster", seq(1, cant.clusters, by = 1), sep = " ")

for (var in var.interes) {
  data <- dataset[, mean(get(var)), cluster2]
  data.radar[, (var)] <- data$V1
}

max.data.radar <- sapply(data.radar, max, na.rm = T)
min.data.radar <- sapply(data.radar, min, na.rm = T)

data.radar <- rbind(max.data.radar, min.data.radar, data.radar)
