#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

library(data.table)
library(ggplot2)

# TODO: cambiar path
setwd("C:\\uba\\dmeyf\\")

# cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")


# medias_enero <- dataset[ foto_mes == 202101, lapply(.SD, mean, na.rm = T), by = .SD ]
# medias_marzo <- dataset[ foto_mes == 202103, lapply(.SD, mean, na.rm = T), by = .SD ]


variables <- c(
  quote(ctarjeta_visa_debitos_automaticos),
  quote(mttarjeta_visa_debitos_automaticos),
  quote(mttarjeta_master_debitos_automaticos),
  quote(mpagodeservicios),
  quote(ctransferencias_recibidas),
  quote(mtransferencias_recibidas), 
  quote(ctransferencias_emitidas),
  quote(mtransferencias_emitidas),
  quote(matm),
  quote(Master_fultimo_cierre),
  quote(Visa_delinquency),
  quote(Visa_Finiciomora),
  quote(Visa_msaldopesos),
  quote(Visa_msaldodolares),
  quote(Visa_fultimo_cierre),
  quote(Visa_mpagado)
)

dataset.enero <- dataset[ foto_mes == 202101 ]
dataset.marzo <- dataset[ foto_mes == 202103 ]

for (variable in variables) {
  new.variable <- paste(variable, "imputada", sep = "__")
  
  media.enero <- mean(dataset.enero[[variable]], na.rm = T)
  media.marzo <- mean(dataset.marzo[[variable]], na.rm = T)
  
  dataset.enero[, (new.variable) := ifelse(is.na(eval(variable)), media.enero, eval(variable))]
  dataset.marzo[, (new.variable) := ifelse(is.na(eval(variable)), media.marzo, eval(variable))]
}

# TODO: descomentar para graficar


# for (variable in variables) {
#   new.variable <- paste(variable, "imputada", sep = "__")
#   hist(dataset.enero[[variable]], col=rgb(0,0,1,1/8), main = variable)
#   hist(dataset.marzo[[variable]], col=rgb(1,0,0,1/8), main = variable, add = T)
# }

compara.medias <- data.table(col_name=character(), media_enero = numeric(), media_marzo = numeric(), dif_medias = numeric())

for (variable in variables) {
  new.variable <- paste(variable, "imputada", sep = "__")
  media.var.enero <- dataset.enero[, mean(eval(variable), na.rm = T)]
  media.var.marzo <- dataset.marzo[, mean(eval(variable), na.rm = T)]

  
  compara.medias <- rbind(compara.medias,
        data.table(
          col_name = new.variable,
          media_enero = media.var.enero,
          media_marzo = media.var.marzo,
          dif_medias = abs(media.var.marzo - media.var.enero)
        ))
}

# options(scipen=999)
ordered.compara.medias <- compara.medias[order(-rank(dif_medias))]
ordered.compara.medias
