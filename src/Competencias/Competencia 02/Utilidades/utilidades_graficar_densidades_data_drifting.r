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

graficar_campo  <- function( campo, ini, fin) {
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
        main= campo
  )
  
  lines(densidad_B, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("202103", "202105"),
           col=c("blue", "red"), lty=c(1,2))
  
}

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#-------------------------------------------------------------#

setwd("C:\\uba\\dmeyf")
dataset  <- fread("./datasets/competencia2_2022.csv.gz")

mes.ini <- 202103
mes.fin <- 202105

#-------------------------------------------------------------#
#------------- RANKING DE VARIABLES CON DRIFTING -------------#
#-------------------------------------------------------------#

variables.drifting.ranking <- c(
  "mpasivos_margen"
  ,"mcuenta_corriente"
  ,"mcaja_ahorro_dolares"
  ,"mtarjeta_visa_consumo"
  ,"mcuenta_debitos_automaticos"
  ,"chomebanking_transacciones"
  ,"ccajas_otras"
  ,"Visa_mconsumospesos"
  ,"Visa_madelantodolares"
  ,"Visa_mpagosdolares"
  ,"Visa_mconsumototal"
)

variables.drifting.ranking.pos.neg.cero <- c(
  "mcomisiones"
  ,"mcaja_ahorro"
  ,"mcuentas_saldo"
  ,"ccuenta_debitos_automaticos"
  ,"ccomisiones_otras"
  ,"mcomisiones_otras"
  ,"Master_mfinanciacion_limite"
  ,"Master_Finiciomora"
  ,"Master_fultimo_cierre"
  ,"Visa_Finiciomora"
  ,"Visa_msaldopesos"
  ,"Visa_fultimo_cierre"
  ,"Visa_mpagado"
)

variables.con.drifting <- c(
  variables.drifting.ranking,
  variables.drifting.ranking.pos.neg.cero
)

ranked.drifting <- c()

rank.prefix <- "ranked_"
for (var in variables.con.drifting) {
  new.var.name <- paste0(rank.prefix, var)
  ranked.drifting <- c(ranked.drifting, new.var.name)
  dataset[, (new.var.name) := (frankv(dataset, cols = var, na.last = TRUE, ties.method = "dense") - 1) / (.N - 1)]
}

#-------------------------------------------------------------#
#----------- FIN RANKING DE VARIABLES CON DRIFTING -----------#
#-------------------------------------------------------------#

#---------------------------------------------------------------#
#-------------------- Gráfico de variables con drifting --------#
#---------------------------------------------------------------#

# for(campo in  variables.con.drifting) {
#   graficar_campo( campo, mes.ini, mes.fin)
# }

#---------------------------------------------------------------#
#-------------------- Gráfico de variables rankeadas -----------#
#---------------------------------------------------------------#


for(campo in  ranked.drifting) {
  graficar_campo( campo, mes.ini, mes.fin)
}
