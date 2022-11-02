#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


library(ggplot2)
library(hrbrthemes)
# library(dplyr)
# library(viridis)

# install.packages("wesanderson")
# library(wesanderson)


graficar.barplot <- function(dataset, cluster.var, var.to.plot, values) {
  gg <- ggplot(dataset,aes(x = factor(get(cluster.var))  , y =get(var.to.plot), fill = factor(get(cluster.var)))) +
    xlab("Número de Cluster") +
    ylab(paste0("Variable: ", var.to.plot)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual("Cluster", values = values)
  
  return(gg)
}

setwd("C:/uba/dmeyf/exp/")

file.folder <- "CLU1261"
file <- "cluster_de_bajas.txt"

file.path <- paste(file.folder,file, sep = "/")

dataset  <- fread(file.path, stringsAsFactors= TRUE)

#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

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

# dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
# dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
# dataset[  , mean(mcuentas_saldo),  cluster2 ]
# dataset[  , mean(chomebanking_transacciones),  cluster2 ]

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

for (var in var.interes) {
  cat(var, "\n")
  data <- dataset[, mean(get(var)), cluster2]
  data <- data[order(-rank(V1))]
  print(data)
  print(graficar.barplot(dataset = dataset, cluster.var = "cluster2", var.to.plot = var, values = cluster.colors))

  # gg <- ggplot(dataset,aes(x = cluster2  , y =get(var), fill = factor(cluster2))) +
  #   xlab("Número de Cluster") +
  #   ylab(paste0("Variable: ", var)) +
  #   geom_bar(stat = "identity", position = "dodge")  +
  #   scale_fill_manual("Cluster", values = cluster.colors)
  # 
  # print(gg)
}



# for (var.name in dt.centroides[ count.2.sd.mean>0, variable ]) {
#   data <- dataset[, mean(get(var.name)), cluster2]
#   cat(var.name, "\n")
#   print(data)
# }
# 
# var.interes <- dt.centroides[count.2.sd.mean > 0, variable]
# var.interes.and.cluster2 <- c(var.interes, "cluster2")
# dataset.var.interes.and.cluster2 <- dataset[, ..var.interes.and.cluster2]



# ggplot(dataset,aes(x = cluster2  , y =mtarjeta_visa_consumo, fill = cluster2)) +
#   xlab("Número de Cluster") +
#   ylab(paste0("Variable: ", "mtarjeta_visa_consumo")) +
#   geom_bar(stat = "identity", position = "dodge")
















# library(cluster)
# plot(silhouette(cutree(mydata.hclust,3), distance))

# p <- ggplot(data=dataset.var.interes.and.cluster2, aes(x=Visa_msaldototal, group=cluster2, fill=cluster2)) +
#   geom_density(adjust=1.5, position="fill") +
#   theme_ipsum()
# 
# print(p)


# p1 <- ggplot(data=dataset, aes(x=mtransferencias_emitidas      , group=cluster2, fill=cluster2)) +
#   geom_density(adjust=1.5) +
#   theme_ipsum() +
#   scale_x_continuous(limits = c(-5000, 5000))
# 
# print(p1)

