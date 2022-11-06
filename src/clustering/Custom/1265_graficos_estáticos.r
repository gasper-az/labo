#limpio la memoria
cat("\014") # limpio la consola
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(viridis)
library(stringr)

library(GGally)
library(fmsb)
library(RColorBrewer)
library(scales)

# Para graficar gif
library(gganimate)
library(gifski)

###################################################################
#################### Funciones de utilidad ########################
###################################################################

graficar.barplot <- function(dataset, cluster.var, var.to.plot, values, flip = F, title = NULL, x.lab = NULL, y.lab = NULL, promediar = T) {
  dataset.plot <- dataset
  
  if (promediar == T) {
    dataset.plot <- dataset.plot[, mean(get(var.to.plot)), cluster.var]
    dataset.plot <- dataset.plot[, (var.to.plot) := V1] 
  }
  
  gg <- ggplot(dataset.plot, aes(x = factor(get(cluster.var))  , y = get(var.to.plot), fill = factor(get(cluster.var)))) +
    scale_fill_manual("Cluster", values = values) +
    geom_bar(stat = "identity") +
    xlab(x.lab) +
    ylab(y.lab) +
    ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=20),
          title = element_text(size=20),
          legend.text = element_text(size=15),
          rect = element_rect(fill = backgorund.color))
  
  if (flip == T) {
    gg <- gg + coord_flip()
  }
  
  return(gg)
}

# Tranforma un string con formato "aaaamm" al formato "aaaa - mm", siendo "aaaa" el año, y "mm" el mes
embellecer.anio.mes <- function(anio.mes) {
  len <- nchar(anio.mes)
  parte.mes <- substring(anio.mes, len-1, len) # toma los últimos 2 caracteres, correspondientes al mes
  parte.anio <- substring(anio.mes, 1, len-2) # toma la parte del año
  return(paste(parte.anio, parte.mes, sep = " - "))
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

cluster.colors <- c( "1" = "#F6EFF7", "2" = "#D0D1E6", "3" = "#A6BDDB", "4" = "#67A9CF", "5" = "#3690C0", "6" = "#02818A", "7" = "#016450")
cluster.nuevos.colors <- c( "Cluster A" = "#a066cb", "Cluster B" = "#D0D1E6", "Cluster C" = "#A6BDDB", "Cluster D" = "#67A9CF")
backgorund.color <- "#f7f4fa"

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
  ,"Master_mfinanciacion_limite"
  ,"Visa_mfinanciacion_limite"
  
  ,"mprestamos_personales"
  
  ,"Master_mlimitecompra"
  ,"Visa_mlimitecompra"
  
  ,"mtarjeta_visa_descuentos"
  ,"mtarjeta_master_descuentos"
  
  # ,"Master_status"
  # ,"Visa_status"
  # ,"mcuenta_debitos_automaticos"
  # ,"mrentabilidad"
  # ,"Visa_mpagominimo"
  # ,"Master_mpagominimo"
  # ,"thomebanking"
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

###################################################################
#################### Graficamos y guardamos #######################
###################################################################

gg.pl <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "mcuenta_corriente", values = cluster.nuevos.colors, flip = T, title = "Monto en pesos en cuenta corriente según Clúster", y.lab = "Monto en cuenta corriente (en pesos)")
print(gg.pl)

gg.pl <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "mcaja_ahorro", values = cluster.nuevos.colors, flip = T, title = "Monto en pesos en caja de ahorro Clúster", y.lab = "Monto en pesos en caja de ahorro")
print(gg.pl)

gg.pl <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "mcaja_ahorro_dolares", values = cluster.nuevos.colors, flip = T, title = "Monto en dolares en caja de ahorro (pesificado) según Clúster", y.lab = "Monto en dolares en caja de ahorro (pesificado)")
print(gg.pl)





gg.pl <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "Master_mconsumototal", values = cluster.nuevos.colors, flip = T, title = "Monto de consumo total (pesos + dólares) tarjeta Master según Clúster", y.lab = "Monto de consumo total (pesos + dólares) tarjeta Master (en pesos)")
print(gg.pl)

gg.pl <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "Visa_mconsumototal", values = cluster.nuevos.colors, flip = T, title = "Monto de consumo total (pesos + dólares) tarjeta Visa según Clúster", y.lab = "Monto de consumo total (pesos + dólares) tarjeta Visa (en pesos)")
print(gg.pl)

gg.pl <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "minversion1_pesos", values = cluster.nuevos.colors, flip = T, title = "Monto de inversión en pesos según Clúster", y.lab = "Monto de inversión en pesos")
print(gg.pl)

gg.pl <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "minversion1_dolares", values = cluster.nuevos.colors, flip = T, title = "Monto de inversión en dólares (pesificado) según Clúster", y.lab = "Monto de inversión en dólares (pesificado)")
print(gg.pl)






gg.ctrx.quarter <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "ctrx_quarter", values = cluster.nuevos.colors, flip = T, title = "Cantidad de movimientos voluntarios según Clúster", y.lab = "Cantidad de movimientos voluntarios")
print(gg.ctrx.quarter)

gg.mtransf.recibidas <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "mtransferencias_recibidas", values = cluster.nuevos.colors, flip = T, title = "Monto total acumulado de transferencias recibidas según Clúster", y.lab = "Monto de transferencias recibidas (en pesos)")
print(gg.mtransf.recibidas)
# ggsave("mtransferencias_recibidas.png", plot = gg.mtransf.recibidas)

gg.mtransf.emitidas <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "mtransferencias_emitidas", values = cluster.nuevos.colors, flip = T, title = "Monto total acumulado de transferencias emitidas según Clúster", y.lab = "Monto de transferencias emitidas (en pesos)")
print(gg.mtransf.emitidas)
# ggsave("mtransferencias_emitidas.png", plot = gg.mtransf.emitidas)





gg.financiacion.master <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "Master_mfinanciacion_limite", values = cluster.nuevos.colors, flip = T, title = "Monto financiación de tarjeta Master según Clúster", y.lab = "Monto financiación de tarjeta Master (en pesos)")
print(gg.financiacion.master)
# ggsave("Master_mfinanciacion_limite.png", plot = gg.financiacion.master)

gg.financiacion.visa <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "Visa_mfinanciacion_limite", values = cluster.nuevos.colors, flip = T, title = "Monto financiación de tarjeta Visa según Clúster", y.lab = "Monto financiación de tarjeta Visa (en pesos)")
print(gg.financiacion.visa)
# ggsave("Visa_mfinanciacion_limite.png", plot = gg.financiacion.visa)





gg.transacciones.visa <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "ctarjeta_visa_transacciones", values = cluster.nuevos.colors, flip = T, title = "Cantidad de transacciones de tarjetas Visa según Clúster", y.lab = "Cantidad de transacciones de tarjetas Visa")
print(gg.transacciones.visa)

gg.monto.transacciones.visa <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "mtarjeta_visa_consumo", values = cluster.nuevos.colors, flip = T, title = "Monto de transacciones de tarjetas Visa según Clúster", y.lab = "Monto de transacciones de tarjetas Visa (en pesos)")
print(gg.monto.transacciones.visa)

gg.transacciones.master <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "ctarjeta_master_transacciones", values = cluster.nuevos.colors, flip = T, title = "Cantidad de transacciones de tarjetas Master según Clúster", y.lab = "Cantidad de transacciones de tarjetas Master")
print(gg.transacciones.master)

gg.monto.transacciones.master <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "mtarjeta_master_consumo", values = cluster.nuevos.colors, flip = T, title = "Monto de transacciones de tarjetas Master según Clúster", y.lab = "Monto de transacciones de tarjetas Master (en pesos)")
print(gg.monto.transacciones.master)





gg.mcuentas.saldo <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "mcuentas_saldo", values = cluster.nuevos.colors, flip = T, title = "Saldo en cuenta según Clúster", y.lab = "Saldo en cuenta (en pesos)")
print(gg.mcuentas.saldo)
# ggsave("mcuentas_saldo.png", plot = gg.mcuentas.saldo)





gg.mcomisiones <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "mcomisiones", values = cluster.nuevos.colors, flip = T, title = "Monto de comisiones ganadas según Clúster", y.lab = "Monto de comisiones ganadas (en pesos)")
print(gg.mcomisiones)

gg.mcomisiones.mantenimiento <- graficar.barplot(dataset = dataset, cluster.var = "cluster_nuevo", var.to.plot = "mcomisiones_mantenimiento", values = cluster.nuevos.colors, flip = T, title = "Monto de comisiones por mantenimiento según Clúster", y.lab = "Monto de comisiones por mantenimiento (en pesos)")
print(gg.mcomisiones.mantenimiento)

############################################################
####################### Otros gráficos #####################
############################################################

dataset.orig  <- fread( "C:/uba/dmeyf/datasets/competencia3_2022.csv.gz", stringsAsFactors= TRUE)

# tomamos las BAJA+2 por mes de todo el dataset
dataset.orig.means <- dataset.orig[clase_ternaria == "BAJA+2", .N, foto_mes]
dataset.orig.means[, foto_mes := embellecer.anio.mes(as.character(foto_mes))]

# colores para el gráfico final
colores <- brewer.pal(9, "PuBuGn")
colores <- colores[4:length(colores)]
backgorund.color <- "#f7f4fa"

l.meses <- length(unique(dataset.orig$foto_mes))
l.colores <- length(colores)
repeticiones <- ceiling(l.meses/l.colores)
colores.rep <- rep(colores, repeticiones)

########## Graficamos (formato .gif) ##########

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

# Hacemos un plot del .gif
animate(myPlot, duration = 10, fps = 10, width = 1200, height = 800, renderer = gifski_renderer(loop = FALSE))

# Grabamos el .gif
anim_save("baja.clientes.gif")

############################################################
####################### Otros gráficos #####################
############################################################

dataset.clust.meses <- dataset[, .N, .(cluster_nuevo, foto_mes)]
dataset.clust.meses[, foto_mes := embellecer.anio.mes(as.character(foto_mes))]

gg.bajas.anio.mes.por.cluster <- ggplot(dataset.clust.meses, aes(x=foto_mes, y = N, fill=factor(cluster_nuevo))) +
  geom_bar(stat='identity', position = "dodge") +
  scale_fill_manual("Cluster", values = cluster.nuevos.colors) +
  xlab("Año - mes") +
  ylab("Cantidad de bajas de clientes Premium") +
  ggtitle("Cantidad de bajas de clientes Premium por Año - Mes según Clúster") +
  theme(axis.text=element_text(size=20, angle = 90),
        axis.title=element_text(size=20),
        title = element_text(size=20),
        legend.text = element_text(size=15),
        rect = element_rect(fill = backgorund.color),
        plot.title = element_text(hjust = 0.5))

print(gg.bajas.anio.mes.por.cluster)



# dataset.clust.meses <- dataset[, mean(mcaja_ahorro_dolares), .(cluster_nuevo, foto_mes)]
# dataset.clust.meses[, foto_mes := embellecer.anio.mes(as.character(foto_mes))]
# 
# gg.bajas.anio.mes.por.cluster <- ggplot(dataset.clust.meses, aes(x=foto_mes, y = V1, fill=factor(cluster_nuevo))) +
#   geom_bar(stat='identity', position = "dodge") +
#   scale_fill_manual("Cluster", values = cluster.nuevos.colors) +
#   xlab("Año - mes") +
#   ylab("Cantidad de bajas de clientes Premium") +
#   ggtitle("Cantidad de bajas de clientes Premium por Año - Mes según Clúster") +
#   theme(axis.text=element_text(size=20, angle = 90),
#         axis.title=element_text(size=20),
#         title = element_text(size=20),
#         legend.text = element_text(size=15),
#         rect = element_rect(fill = backgorund.color),
#         plot.title = element_text(hjust = 0.5))
# 
# print(gg.bajas.anio.mes.por.cluster)
