#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("ggplot2")
require("hrbrthemes")
#-------------------------------------------------------------#
#-------------------------------------------------------------#
#-------------------------------------------------------------#

graficar.analisis.distrib <- function(dataset, variable) {
  my.plot <- ggplot(dataset, aes(x=get(variable), fill=as.factor(foto_mes))) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    theme_ipsum() +
    labs(fill="") +
    ggtitle(paste0("Grafico de Marzo vs Mayo de: ", variable))
  
  return(my.plot)
}

graficar_campo  <- function( campo, ini, fin, modo = "") {
  qA  <- quantile(  dataset[ foto_mes==ini , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qB  <- quantile(  dataset[ foto_mes==fin , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  
  xxmin  <- pmin( qA[[1]], qB[[1]] )
  xxmax  <- pmax( qA[[2]], qB[[2]] )
  
  
  densidad_A  <- density( dataset[ foto_mes==ini, get(campo) ],
                            kernel="gaussian", na.rm=TRUE )
    
  densidad_B  <- density( dataset[ foto_mes==fin, get(campo) ],
                            kernel="gaussian", na.rm=TRUE )
  
  
  plot( densidad_A,
        col="blue",
        xlim= c( xxmin, xxmax ),
        ylim= c( 0, pmax( max(densidad_A$y), max(densidad_B$y) ) ),
        main= paste(campo, " - modo: ", modo)
  )
  
  lines(densidad_B, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("202103", "202105"),
           col=c("blue", "red"), lty=c(1,2))
  
}

rank.pos.neg <- function(dataset, var, new.var.name, mes, ties.method) {
  dataset[foto_mes==mes, (new.var.name) := ifelse(var >= 0,
                                                  (ifelse(var > 0,
                                                          (frankv(dataset[foto_mes==mes], cols = var, na.last = TRUE, ties.method = ties.method) - 1) / (.N - 1), ## mayores a cero
                                                          0)), # cero
                                                  -(frankv(dataset[foto_mes==mes], cols = var, na.last = TRUE, ties.method = ties.method) - 1) / (.N - 1) ## menores a cero
  )
  ]
}

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#-------------------------------------------------------------#

setwd("C:\\uba\\dmeyf")
set.seed(763369)
dataset  <- fread("./datasets/competencia2_2022.csv.gz")

mes.ini <- 202103
mes.fin <- 202105

#-------------------------------------------------------------#
#------------- RANKING DE VARIABLES CON DRIFTING -------------#
#-------------------------------------------------------------#

variables.drifting.ranking <- c(
  "mpasivos_margen"
  ,"mcaja_ahorro_dolares"
  ,"mtarjeta_visa_consumo"
  ,"mcuenta_debitos_automaticos"
  ,"chomebanking_transacciones"
  ,"ccajas_otras"
  ,"Visa_mconsumospesos"
  ,"Visa_mconsumototal"
  ,"mcuentas_saldo"
  ,"Visa_msaldototal"
)

var.drifting.pos.neg.dense <- c(
  "ccuenta_debitos_automaticos"
  ,"Master_mfinanciacion_limite"
  ,"Master_fultimo_cierre"
  ,"Visa_fultimo_cierre"
  ,"mcuenta_corriente"
  ,"Visa_madelantodolares"
  ,"Visa_mpagosdolares"
  
)

var.drifting.pos.neg.last <- c(
  "Master_Finiciomora"
  ,"Visa_Finiciomora"
)

var.drifting.pos.neg.first <- c(
  "Visa_mpagado"
)

variables.drifting.random <- c(
  "mcaja_ahorro"
  ,"mcomisiones"
  ,"ccomisiones_otras"
  ,"mcomisiones_otras"
  ,"Visa_msaldopesos"
  ,"mrentabilidad"
  ,"mrentabilidad_annual"
  ,"mactivos_margen"
)

# c("average", "first", "last", "random", "max", "min", "dense")


ranked.drifting <- c()

rank.prefix <- "r_"

for (var in variables.drifting.ranking) {
  new.var.name <- paste0(rank.prefix, var)
  ranked.drifting <- c(ranked.drifting, new.var.name)
  dataset[foto_mes==mes.ini, (new.var.name) := (frankv(dataset[foto_mes==mes.ini], cols = var, na.last = TRUE, ties.method = "dense") - 1) / (.N - 1)]
  dataset[foto_mes==mes.fin, (new.var.name) := (frankv(dataset[foto_mes==mes.fin], cols = var, na.last = TRUE, ties.method = "dense") - 1) / (.N - 1)]
}

ranked.drifting.random <- c()

for (var in variables.drifting.random) {
  new.var.name <- paste0(rank.prefix, var)
  ranked.drifting.random <- c(ranked.drifting.random, new.var.name)
  dataset[foto_mes==mes.ini, (new.var.name) := (frankv(dataset[foto_mes==mes.ini], cols = var, na.last = TRUE, ties.method = "random") - 1) / (.N - 1)]
  dataset[foto_mes==mes.fin, (new.var.name) := (frankv(dataset[foto_mes==mes.fin], cols = var, na.last = TRUE, ties.method = "random") - 1) / (.N - 1)]
}

ranked.pos.neg <- c()
rank.pos.neg.prefix <- "rpn_"

ties.method <- "dense"
for (var in var.drifting.pos.neg.dense) {
  new.var.name <- paste0(rank.pos.neg.prefix, var)
  ranked.pos.neg <- c(ranked.pos.neg, new.var.name)
  
  rank.pos.neg(dataset, var, new.var.name, mes.ini, ties.method)
  rank.pos.neg(dataset, var, new.var.name, mes.fin, ties.method)
}


ranked.pos.neg.last <- c()

ties.method <- "last"
for (var in var.drifting.pos.neg.last) {
  new.var.name <- paste0(rank.pos.neg.prefix, var)
  ranked.pos.neg.last <- c(ranked.pos.neg.last, new.var.name)
  
  rank.pos.neg(dataset, var, new.var.name, mes.ini, ties.method)
  rank.pos.neg(dataset, var, new.var.name, mes.fin, ties.method)
}

ranked.pos.neg.first <- c()

ties.method <- "first"
for (var in var.drifting.pos.neg.first) {
  new.var.name <- paste0(rank.pos.neg.prefix, var)
  ranked.pos.neg.first <- c(ranked.pos.neg.first, new.var.name)

  rank.pos.neg(dataset, var, new.var.name, mes.ini, ties.method)
  rank.pos.neg(dataset, var, new.var.name, mes.fin, ties.method)
}



#-------------------------------------------------------------#
#----------- FIN RANKING DE VARIABLES CON DRIFTING -----------#
#-------------------------------------------------------------#

setwd("C:\\uba\\repos\\labo\\src\\Competencias\\Competencia 02")

pdf("fix_data_drifting_competencia_02.pdf")

#---------------------------------------------------------------#
#-------------------- Gráfico de variables rankeadas -----------#
#---------------------------------------------------------------#

for(campo in  ranked.drifting) {
  graficar_campo( campo, mes.ini, mes.fin, "frank - dense")
}

for(campo in  ranked.drifting.random) {
  graficar_campo( campo, mes.ini, mes.fin, "frank - random")
}

#-----------------------------------------------------------------------#
#-------------------- Gráfico de variables rankeadas Pos Neg -----------#
#-----------------------------------------------------------------------#


for(campo in  ranked.pos.neg) {
  graficar_campo( campo, mes.ini, mes.fin, "frank - pos.neg - dense")
}

for(campo in  ranked.pos.neg.last) {
  graficar_campo( campo, mes.ini, mes.fin, "frank - pos.neg - last")
}

for(campo in  ranked.pos.neg.first) {
  graficar_campo( campo, mes.ini, mes.fin, "frank - pos.neg - first")
}

dev.off()