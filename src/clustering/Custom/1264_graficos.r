#limpio la memoria
cat("\014") # limpio la consola
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr) # para unnest

library(stringr)

library(GGally)
library(fmsb)
library(RColorBrewer)
library(scales)

library(gganimate)
library(gifski)

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

cluster.colors <- c( "1" = "#F6EFF7", "2" = "#D0D1E6", "3" = "#A6BDDB", "4" = "#67A9CF", "5" = "#3690C0", "6" = "#02818A", "7" = "#016450")
cluster.nuevos.colors <- c( "Cluster A" = "#F6EFF7", "Cluster B" = "#D0D1E6", "Cluster C" = "#A6BDDB", "Cluster D" = "#67A9CF")


# var.interes <- c("mtarjeta_visa_consumo"
#                  ,"ctarjeta_visa_transacciones"
#                  ,"Visa_msaldototal"
#                  # ,"Visa_mpagospesos"
#                  ,"Visa_mconsumospesos"
#                  ,"Master_mfinanciacion_limite"
#                  ,"mcuenta_debitos_automaticos"
#                  ,"Visa_cconsumos"
#                  ,"ctrx_quarter"
#                  ,"mcuentas_saldo"
#                  ,"mtransferencias_recibidas", "mtransferencias_emitidas", "mcaja_ahorro", "mprestamos_personales", "mpagomiscuentas"
#                  ,"cliente_antiguedad", "cliente_edad"
#                  ,"cproductos", "mcomisiones", "mcomisiones_mantenimiento", "mcomisiones_otras")

var.interes <- c(
  "ctrx_quarter"
  ,"mcuentas_saldo"
  ,"mtransferencias_recibidas"
  ,"mtransferencias_emitidas"
  ,"cproductos"
  ,"mtarjeta_visa_consumo"
  ,"ctarjeta_visa_transacciones"
)

###################################################################
#################### Redefinimos los clústeres ####################
###################################################################

dataset[cluster2 %in% c(6), cluster_nuevo := "Cluster A"]
dataset[cluster2 %in% c(1), cluster_nuevo := "Cluster B"]
dataset[cluster2 %in% c(3,5), cluster_nuevo := "Cluster C"]
dataset[cluster2 %in% c(2,4,7), cluster_nuevo := "Cluster D"]

###################################################################
#################### Definimos variables ##########################
###################################################################

cant.clusters.nuevo <- length(unique(dataset[, cluster_nuevo]))
cant.var.interes <- length(var.interes)

temp.vars <- c("cluster_nuevo", c(var.interes))
dataset.temp <- dataset[, ..temp.vars]

#############################################################
######################### Intento 1 #########################
#############################################################

seq.percentile <- seq(.25, 1, by = .25)

procesar.dataset.graficos.movim <- function(dataset, var.clusters, var.graficar, percentiles) {
  clusters.names <- sort(unique(dataset[, get(var.clusters)]))

  for (clust.name in clusters.names) {
    clust.percentile <- dataset.temp[cluster_nuevo == clust.name, quantile(get(var.graficar), percentiles)]
    perc.numb <- 1
    for (i in seq(1, length(percentiles), by = 1)) {
      max <- clust.percentile[[i]]
      min <- ifelse(i > 1, clust.percentile[[i-1]], -Inf)
      dataset.temp[cluster_nuevo == clust.name & get(var.graficar) > min & get(var.graficar) <= max, paste("perc.grp", var.graficar, sep = "_") := perc.numb]
      perc.numb <- perc.numb + 1
    }
  }
}

for (variable in var.interes) {
  procesar.dataset.graficos.movim(dataset.temp, "cluster_nuevo", (variable), seq.percentile) 
}

#############################################################
######################### PLOT ##############################
#############################################################

# check: https://stackoverflow.com/questions/58024703/gganimate-how-to-make-stacked-bar-chart-grow-smoothly-upwards-from-x-axis
myPlot <- ggplot(dataset.temp, aes(x=cluster_nuevo, y=mcuentas_saldo, fill=cluster_nuevo)) + 
  geom_bar(stat='identity', position = "dodge") +
  # theme_bw() +
  scale_fill_manual("Cluster", values = cluster.nuevos.colors) + 
  coord_flip() +
  # gganimate specific bits:
  transition_states(
    perc.grp_mcuentas_saldo,
    wrap = FALSE,
  ) +
  shadow_mark()
  # ease_aes('sine-in-out')

# myPlot2 <- ggplot(dataset.temp, aes(x=cluster_nuevo, y=mcuentas_saldo, fill=cluster_nuevo)) + 
#   geom_bar(stat='identity') +
#   theme_bw() +
#   scale_fill_manual("Cluster", values = cluster.nuevos.colors) + 
#   coord_flip() +
#   transition_layers(layer_length = 1, transition_length = 2) +
#   ease_aes('linear')

animate(myPlot, duration = 2, fps = 5, width = 800, height = 800, renderer = gifski_renderer(loop = FALSE))

# anim_save("output.gif")

############################################################
####################### Otros tests ########################
############################################################

dataset.orig  <- fread( "C:/uba/dmeyf/datasets/competencia3_2022.csv.gz", stringsAsFactors= TRUE)

embellecer.anio.mes <- function(anio.mes) {
  len <- nchar(anio.mes)
  parte.mes <- substring(anio.mes, len-1, len) # toma los últimos 2 caracteres, correspondientes al mes
  parte.anio <- substring(anio.mes, 1, len-2) # toma la parte del año
  
  return(paste(parte.anio, parte.mes, sep = " - "))
}


dataset.orig.means <- dataset.orig[clase_ternaria == "BAJA+2", .N, foto_mes]
dataset.orig.means[, foto_mes := embellecer.anio.mes(as.character(foto_mes))]

colores <- brewer.pal(9, "PuBuGn")
colores <- colores[4:length(colores)]
backgorund.color <- "#f7f4fa"

l.meses <- length(unique(dataset.orig$foto_mes))
l.colores <- length(colores)

repeticiones <- ceiling(l.meses/l.colores)

colores.rep <- rep(colores, repeticiones)

myPlot <- ggplot(dataset.orig.means, aes(x=foto_mes, y=N, fill=foto_mes)) + 
  geom_bar(stat='identity', position = "dodge") +
  scale_fill_manual("Año - Mes", values = colores.rep) +
  scale_x_discrete(limits = dataset.orig.means$foto_mes) +
  transition_states(
    foto_mes,
    wrap = FALSE,
  ) +
  shadow_mark() + # para mantener los valores de meses anteriores al hacer transiciones
  xlab("Año - Mes") +
  ylab("Cantidad de bajas de clientes Premium") +
  ggtitle("Cantidad de bajas de clientes Premium por Año - Mes") +
  theme(axis.text=element_text(size=20, angle = 90),
        axis.title=element_text(size=20),
        title = element_text(size=20),
        legend.position = "none",
        rect = element_rect(fill = backgorund.color),
        plot.title = element_text(hjust = 0.5))
# ease_aes('sine-in-out')

animate(myPlot, duration = 10, fps = 10, width = 1200, height = 800, renderer = gifski_renderer(loop = FALSE))
anim_save("baja.clientes.gif")
