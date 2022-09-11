rm(list=ls())
gc()

require("data.table")
require("rpart")
require("rpart.plot")

`%notin%` <- Negate(`%in%`)

if ("sm" %notin% installed.packages()) {
  install.packages("sm")
}

if ("hrbrthemes" %notin% installed.packages()) {
  install.packages("hrbrthemes")
}

require("sm")
require("hrbrthemes")

#--------------------------------------------------------#
#-------------------- PATH y SEMILLA --------------------#
#--------------------------------------------------------#

working.directory <- "C:\\uba\\dmeyf"
semilla <- 763369

setwd(working.directory)

#----------------------------------------------------------#
#-------------------- Carga de dataset --------------------#
#----------------------------------------------------------#

dataset  <- fread("./datasets/competencia1_2022.csv" )

#----------------------------------------------------------#
#-------------------- Divido por meses --------------------#
#----------------------------------------------------------#

denero  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dmarzo  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#----------------------------------------------------------#
#-------------------- Función para graficar ---------------#
#----------------------------------------------------------#

graficar.analisis.distrib <- function(dataset, variable) {
  my.plot <- ggplot(dataset, aes(x=get(variable), fill=as.factor(foto_mes))) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    theme_ipsum() +
    labs(fill="") +
    ggtitle(paste0("Grafico de Enero vs Marzo de: ", variable))
  
  return(my.plot)
}

#----------------------------------------------------------#
#-------------------- Variables (pesos) a analizar --------#
#----------------------------------------------------------#

var.analizar <- c(
  "Master_mfinanciacion_limite"
  , "Master_msaldototal"
  , "Master_msaldopesos"
  , "Master_msaldodolares"
  , "Master_mconsumospesos"
  , "Master_mconsumosdolares"
  , "Master_mlimitecompra"
  , "Master_madelantopesos"
  , "Master_madelantodolares"
  , "Master_mpagado"
  , "Master_mpagospesos"
  , "Master_mpagosdolares"
  , "Master_mconsumototal"
  , "Master_mpagominimo"
  , "Visa_mfinanciacion_limite"
  , "Visa_msaldototal"
  , "Visa_msaldopesos"
  , "Visa_msaldodolares"
  , "Visa_mconsumospesos"
  , "Visa_mconsumosdolares"
  , "Visa_mlimitecompra"
  , "Visa_madelantopesos"
  , "Visa_madelantodolares"
  , "Visa_mpagado"
  , "Visa_mpagospesos"
  , "Visa_mpagosdolares"
  , "Visa_mconsumototal"
  , "Visa_mpagominimo"
  , "mrentabilidad"
  , "mrentabilidad_annual"
  , "mcomisiones"
  , "mactivos_margen"
  , "mpasivos_margen"
  , "mcuenta_corriente_adicional"
  , "mcuenta_corriente"
  , "mcaja_ahorro"
  , "mcaja_ahorro_adicional"
  , "mcaja_ahorro_dolares"
  , "mcuentas_saldo"
  , "mautoservicio"
  , "mtarjeta_visa_consumo"
  , "mtarjeta_master_consumo"
  , "mprestamos_personales"
  , "mprestamos_prendarios"
  , "mprestamos_hipotecarios"
  , "mplazo_fijo_dolares"
  , "mplazo_fijo_pesos"
  , "minversion1_pesos"
  , "minversion1_dolares"
  , "minversion2"
  , "mpayroll"
  , "mpayroll2"
  , "mcuenta_debitos_automaticos"
  , "mttarjeta_master_debitos_automaticos"
  , "mpagodeservicios"
  , "mpagomiscuentas"
  , "mcajeros_propios_descuentos"
  , "mtarjeta_visa_descuentos"
  , "mtarjeta_master_descuentos"
  , "mcomisiones_mantenimiento"
  , "mcomisiones_otras"
  , "mforex_buy"
  , "mforex_sell"
  , "mtransferencias_recibidas"
  , "mtransferencias_emitidas"
  , "mextraccion_autoservicio"
  , "mcheques_depositados"
  , "mcheques_emitidos"
  , "mcheques_depositados_rechazados"
  , "mcheques_emitidos_rechazados"
  , "matm"
  , "matm_other"
)

#-------------------------------------------------------------#
#-------------------- Gráfico de variables a analizar --------#
#-------------------------------------------------------------#

# for (variable in var.analizar) {
#   print(graficar.analisis.distrib(dataset, variable))
#   readline(prompt = "Presionar cualquier tecla para continuar...."); 
# }

#----------------------------------------------------------------#
#-------------------- Variables con drifting --------------------#
#----------------------------------------------------------------#

var.drifting <- c(
  "Visa_mpagado",
  "mcomisiones_mantenimiento",
  "mforex_sell",
  "Master_mlimitecompra",
  "Visa_mlimitecompra",
  "mextraccion_autoservicio"
)

#---------------------------------------------------------------#
#-------------------- Gráfico de variables con drifting --------#
#---------------------------------------------------------------#

for (variable in var.drifting) {
  print(graficar.analisis.distrib(dataset, variable))
  readline(prompt = "Presionar cualquier tecla para continuar....");
}