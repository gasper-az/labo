#limpio la memoria
cat("\014") # limpio la consola
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
library(ggplot2)
library(hrbrthemes)
library(dplyr)

library(GGally)
library(fmsb)
library(RColorBrewer)
library(scales)

graficar.barplot <- function(dataset, cluster.var, var.to.plot, values, flip = F) {
  gg <- ggplot(dataset, aes(x = factor(get(cluster.var))  , y = get(var.to.plot), fill = factor(get(cluster.var)))) +
    xlab("Número de Cluster") +
    ylab(paste0("Variable: ", var.to.plot)) +
    scale_fill_manual("Cluster", values = values) +
    geom_bar(stat = "identity")
    # geom_text(aes(label=(get(var))), vjust=1.6, color="black", size=3.5)
  
  if (flip == T) {
    gg <- gg + coord_flip()
  }
  
  return(gg)
}

graficar.radar.chart <- function(data.radar, colors_border, colors_in, row.names) {
  radarchart( data.radar, axistype=1 , 
              #custom polygon
              pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
              #custom labels
              vlcex=0.8 
  )
  
  # Add a legend
  legend(x=1, y=1, legend = row.names, bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
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

# cluster.colors <- c( "1" = "#42687C", "2" = "#236E96", "3" = "#84A5B8", "4" = "#15B2D3", "5" = "#FFD700", "6" = "#F3872F", "7" = "#FF598F")
cluster.colors <- c( "1" = "#F6EFF7", "2" = "#D0D1E6", "3" = "#A6BDDB", "4" = "#67A9CF", "5" = "#3690C0", "6" = "#02818A", "7" = "#016450")
cluster.nuevos.colors <- c( "Cluster A" = "#F6EFF7", "Cluster B" = "#D0D1E6", "Cluster C" = "#A6BDDB", "Cluster D" = "#67A9CF")

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

dt.centroides <- dt.centroides[order(-rank(count.2.sd.mean), -rank(sd))]

var.interes <- dt.centroides[count.2.sd.mean > 0, variable]
var.interes <- c(var.interes, "ctrx_quarter", "cpayroll_trx", "mcuentas_saldo",
                 # "mprestamos_personales", "mactivos_margen", "cliente_edad", "cliente_antiguedad", "mrentabilidad", "mcuenta_corriente", "mcomisiones_mantenimiento", "mcomisiones")
                 "mtransferencias_recibidas", "mtransferencias_emitidas", "mcaja_ahorro", 
                 # "mcuenta_corriente", 
                 "mprestamos_personales", "mpagomiscuentas", 
                 # "mrentabilidad", "mcomisiones", "mcomisiones_mantenimiento", 
                 "cliente_antiguedad", "cliente_edad",
                 "cproductos", "mcomisiones", "mcomisiones_mantenimiento", "mcomisiones_otras")

# me aseguro que no haya repetidos
var.interes <- unique(var.interes)

# TODO: descomentar para grabar gráficos en pdf
# pdf("var_interes.pdf")

for (var in var.interes) {
  cat(var, "\n")
  data <- dataset[, mean(get(var)), cluster2]
  data <- data[order(-rank(V1))]
  print(data)
  
  # dataset.reducido <- dataset[, max(get(var)), by = cluster2]
  # dataset.reducido <- dataset.reducido[, (var) := V1]
  # dataset.reducido <- dataset.reducido[, V1 := NULL]
  
  # TODO: descomentar para grabar gráficos en pdf
  # print(graficar.barplot(dataset = dataset, cluster.var = "cluster2", var.to.plot = var, values = cluster.colors))
}

# TODO: descomentar para grabar gráficos en pdf
# dev.off()

# variables de interés que me gustan a mi. Son un subconjunto de var.interes, sacados de un análisis de bar plot
var.interes <- c("mtarjeta_visa_consumo"
                 ,"ctarjeta_visa_transacciones"
                 ,"Visa_msaldototal"
                 # ,"Visa_mpagospesos"
                 ,"Visa_mconsumospesos"
                 ,"Master_mfinanciacion_limite"
                 ,"mcuenta_debitos_automaticos"
                 ,"Visa_cconsumos"
                 ,"ctrx_quarter"
                 ,"mcuentas_saldo"
                 ,"mtransferencias_recibidas", "mtransferencias_emitidas", "mcaja_ahorro", "mprestamos_personales", "mpagomiscuentas"
                 ,"cliente_antiguedad", "cliente_edad"
                 ,"cproductos", "mcomisiones", "mcomisiones_mantenimiento", "mcomisiones_otras")


cant.clusters <- length(unique(dataset[, cluster2]))
cant.var.interes <- length(var.interes)

data.radar <- as.data.frame(matrix(rep(0, cant.clusters * cant.var.interes), ncol = cant.var.interes))
colnames(data.radar) <- var.interes
rownames(data.radar) <- paste("Tipo", seq(1, cant.clusters, by = 1), sep = " ")

# TODO: descomentar para grabar gráficos en pdf
# pdf("var_interes_redux.pdf")

for (var in var.interes) {
  # forma 1
  # data <- dataset[, mean(get(var)), cluster2]
  # data.radar[, (var)] <- scale(data$V1)
  
  # forma 2
  dataset <- dataset[, paste0((var), "_scale") := scale(get(var))]
  data <- dataset[, mean(get(paste0((var), "_scale"))), cluster2]
  data.radar[, (var)] <- data$V1

  # TODO: descomentar para grabar gráficos en pdf  
  # print(graficar.barplot(dataset = dataset, cluster.var = "cluster2", var.to.plot = var, values = cluster.colors))
}

# TODO: descomentar para grabar gráficos en pdf
# dev.off()

# necesarios para el radar chart
max.data.radar <- sapply(data.radar, max, na.rm = T)
min.data.radar <- sapply(data.radar, min, na.rm = T)
data.radar <- rbind(max.data.radar, min.data.radar, data.radar)

## TODO:
## src: https://r-graph-gallery.com/143-spider-chart-with-saveral-individuals.html

# # forma 1: colores hardcodeados
# colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
# colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

# # forma 2: paleta de colores via library
coul <- brewer.pal(3, "PuBuGn")
colors_border <- coul
colors_in <- alpha(coul,0.3)

# Para un primer análisis
radar.min.max <- c(1:2)
radar.cluster.1.2 <- c(3:4)
radar.cluster.3.4 <- c(5:6)
radar.cluster.5.6.7 <- c(7:9)

radar.cluster.1.2.min.max <- c(radar.min.max, radar.cluster.1.2)
radar.cluster.3.4.min.max <- c(radar.min.max, radar.cluster.3.4)
radar.cluster.5.6.7.min.max <- c(radar.min.max, radar.cluster.5.6.7)

# Para un análisis comparando clústeres "similares"

radar.cluster.2.4.7 <- c(4, 6, 9)
radar.cluster.2.4.7.min.max <- c(radar.min.max, radar.cluster.2.4.7)

radar.cluster.1.3.5 <- c(3, 5, 7)
radar.cluster.1.3.5.min.max <- c(radar.min.max, radar.cluster.1.3.5)

radar.cluster.3.5 <- c(5, 7)
radar.cluster.3.5.min.max <- c(radar.min.max, radar.cluster.3.5)

radar.cluster.1.6 <- c(3, 8)
radar.cluster.1.6.min.max <- c(radar.min.max, radar.cluster.1.6)

# # Clústers 1 y 2
# graficar.radar.chart(data.radar = data.radar[radar.cluster.1.2.min.max, ], colors_border = colors_border[1:2], colors_in = colors_in[1:2], row.names = rownames(data.radar[radar.cluster.1.2, ]))
# 
# # Clústers 3 y 4
# graficar.radar.chart(data.radar = data.radar[radar.cluster.3.4.min.max, ], colors_border = colors_border[1:2], colors_in = colors_in[1:2], row.names = rownames(data.radar[radar.cluster.3.4, ]))
# 
# # Clústers 5, 6 y 7
# graficar.radar.chart(data.radar = data.radar[radar.cluster.5.6.7.min.max, ], colors_border = colors_border, colors_in = colors_in, row.names = rownames(data.radar[radar.cluster.5.6.7, ]))


# Clústeres 2, 4 y 7
graficar.radar.chart(data.radar = data.radar[radar.cluster.2.4.7.min.max, ], colors_border = colors_border, colors_in = colors_in, row.names = rownames(data.radar[radar.cluster.2.4.7, ]))

# # Clústeres 1, 3 y 5
# graficar.radar.chart(data.radar = data.radar[radar.cluster.1.3.5.min.max, ], colors_border = colors_border, colors_in = colors_in, row.names = rownames(data.radar[radar.cluster.1.3.5, ]))

# Clústeres 3 y 5
graficar.radar.chart(data.radar = data.radar[radar.cluster.3.5.min.max, ], colors_border = colors_border[1:2], colors_in = colors_in[1:2], row.names = rownames(data.radar[radar.cluster.3.5, ]))

# # Clústeres 1 y 6
# graficar.radar.chart(data.radar = data.radar[radar.cluster.1.6.min.max, ], colors_border = colors_border[1:2], colors_in = colors_in[1:2], row.names = rownames(data.radar[radar.cluster.1.6, ]))

# ggcorr(dataset[, ..var.interes], method = c("everything", "pearson"))

###################################################################
#################### Redefinimos los clústeres ####################
###################################################################

dataset[cluster2 %in% c(6), cluster_nuevo := "Cluster A"]
dataset[cluster2 %in% c(1), cluster_nuevo := "Cluster B"]
dataset[cluster2 %in% c(3,5), cluster_nuevo := "Cluster C"]
dataset[cluster2 %in% c(2,4,7), cluster_nuevo := "Cluster D"]

cant.clusters.nuevo <- length(unique(dataset[, cluster_nuevo]))
cant.var.interes <- length(var.interes)

data.radar.nuevo <- as.data.frame(matrix(rep(0, cant.clusters.nuevo * cant.var.interes), ncol = cant.var.interes))
colnames(data.radar.nuevo) <- var.interes
rownames(data.radar.nuevo) <- unique(dataset[, cluster_nuevo])

# Muestra MAX, MIN, y AVG por cada grupo nuevo, por cada variable
for (var in var.interes) {
  data.max <- dataset[, max(get(var)), cluster_nuevo]
  data.min <- dataset[, min(get(var)), cluster_nuevo]
  data.mean <- dataset[, mean(get(var)), cluster_nuevo]
  
  data.fin <- cbind(data.max, data.min$V1, data.mean$V1)
  data.fin <- data.fin[, paste0((var),"_max") := V1]
  data.fin <- data.fin[, V1 := NULL]
  data.fin <- data.fin[, paste0((var),"_min") := V2]
  data.fin <- data.fin[, V2 := NULL]
  data.fin <- data.fin[, paste0((var),"_mean") := V3]
  data.fin <- data.fin[, V3 := NULL]
  
  cat(var, "\n")
  print(data.fin[order(rank(cluster_nuevo))])
}

# Radar plot con nuevas clases

for (var in var.interes) {
  # forma 2
  dataset <- dataset[, paste0((var), "_scale") := scale(get(var))]
  data <- dataset[, mean(get(paste0((var), "_scale"))), cluster_nuevo]
  data.radar.nuevo[, (var)] <- data$V1
}

# necesarios para el radar chart
max.data.radar.nuevo <- sapply(data.radar.nuevo, max, na.rm = T)
min.data.radar.nuevo <- sapply(data.radar.nuevo, min, na.rm = T)
data.radar.nuevo <- rbind(max.data.radar.nuevo, min.data.radar.nuevo, data.radar.nuevo)

colors_border_nuevo <- brewer.pal(4, "PuBuGn")
colors_in_nuevo <- alpha(colors_border_nuevo,0.3)
radar.min.max <- c(1:2)
radar.cluster.nuevo.all <- c(3:6)
colors_border_nuevo.min.max <- c(radar.min.max, colors_border_nuevo)


graficar.radar.chart(data.radar = data.radar.nuevo, colors_border = colors_border_nuevo, colors_in = colors_in_nuevo, row.names = rownames(data.radar.nuevo[radar.cluster.nuevo.all, ]))

# # TODO: descomentar para grabar gráficos en pdf
# # pdf("var_interes_clusters_nuevos.pdf")
# 
# for (var in var.interes) {
#   # forma 1
#   # data <- dataset[, mean(get(var)), cluster2]
#   # data.radar[, (var)] <- scale(data$V1)
#   
#   # forma 2
#   dataset <- dataset[, paste0((var), "_scale") := scale(get(var))]
#   data <- dataset[, mean(get(paste0((var), "_scale"))), cluster_nuevo]
#   data.radar.nuevo[, (var)] <- data$V1
#   
#   # TODO: descomentar para grabar gráficos en pdf  
#   # print(graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = var, values = cluster.nuevos.colors))
#   
#   # gg <- ggplot(dataset[order(-rank(cluster_nuevo))], aes(cluster2, get(var), fill = cluster_nuevo)) + 
#   #   geom_bar(position = "dodge", width = 0.5, stat = "identity")
#   # print(gg)
# }
# 
# # TODO: descomentar para grabar gráficos en pdf  
# # dev.off()

# TODO: graficar var.interes por cada clúster
# Cluster A
# Cluster B
# Cluster C
# Cluster D

# # TODO: descomentar para grabar gráficos en pdf
# pdf("cluster.nuevo.vars.pdf")

for (var in var.interes) {
  # TODO: descomentar para grabar gráficos en pdf
  print(graficar.barplot(dataset = dataset[order(rank(cluster_nuevo))], cluster.var = "cluster_nuevo", var.to.plot = var, values = colors_border_nuevo, flip = T))
}

# # TODO: descomentar para grabar gráficos en pdf
# dev.off()