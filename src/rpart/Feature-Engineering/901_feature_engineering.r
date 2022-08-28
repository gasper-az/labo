require("data.table")
require("rpart")
require("rpart.plot")
require("hash")

#---------------------------------------------------------------#
#-------------------- Funciones de utilidad --------------------#
#---------------------------------------------------------------#

`%notin%` <- Negate(`%in%`)

aplicar.Predicados <- function (dataset, predicados) {
  for (predicado in keys(predicados)) {
    predicado.value <- predicados[[predicado]]
    
    if (predicado %notin% colnames(dataset)) {
      # predicado[1] devuelve el valor literal, lo usamos como colname
      # eval(parse()) nos permite evaluar el predicado
      dataset[, predicado[1] := eval(parse(text = predicado.value))] 
    }
  }
}

save.File <- function(dataset, folder, file) {
  crear.Sub.Carpetas(folder)
  fwrite(dataset,
         file = paste0(folder, file),
         sep = ",")
}

crear.Sub.Carpetas <- function(path) {
  base <- ""
  subCarpetas <- unlist(strsplit(path, "/"))
  
  for (subCarpeta in subCarpetas) {
    base <- paste0(base, subCarpeta, "/")
    dir.create(base, showWarnings = FALSE)
  }
}

#---------------------------------------------------------------#
#-------------------- Variables - Carpertas --------------------#
#---------------------------------------------------------------#

# TODO: cambiar valores según la ubicación del repo
base.path <- "C:\\uba\\dmeyf\\"
dataset.path <- "./datasets/competencia1_2022.csv"
new.dataset.folder <- "./datasets/feature-engineering/v1/"
new.dataset.name <- "competencia1_2022_fe_v1.csv"

#---------------------------------------------------------------#
#-------------------- Acá comienza el programa -----------------#
#---------------------------------------------------------------#

setwd(base.path)

dataset <- fread(dataset.path)

predicados.logicos <- hash()
predicados.logicos[["predicado_01"]] <- "(ctrx_quarter < 14) & (mcuentas_saldo < -1256.1) & (cprestamos_personales < 2) & (cdescubierto_preacordado == 0)"
predicados.logicos[["predicado_02"]] <- "(ctrx_quarter < 14) & (mcuentas_saldo < -1256.1) & (cprestamos_personales < 2) & (cdescubierto_preacordado == 1) & (mpasivos_margen < 8.05)"
predicados.logicos[["predicado_03"]] <- "(ctrx_quarter < 14) & (mcuentas_saldo < -1256.1) & (cprestamos_personales < 2) & (cdescubierto_preacordado == 1) & (mpasivos_margen >= 8.05)"
predicados.logicos[["predicado_04"]] <- "(ctrx_quarter < 14) & (mcuentas_saldo < -1256.1) & (cprestamos_personales >= 2)"
predicados.logicos[["predicado_05"]] <- "(ctrx_quarter < 14) & (mcuentas_saldo >= -1256.1) & (mcaja_ahorro < 2601.1) & (Visa_fechaalta >= 4539)"
predicados.logicos[["predicado_06"]] <- "(ctrx_quarter < 14) & (mcuentas_saldo >= -1256.1) & (mcaja_ahorro < 2601.1) & (Visa_fechaalta < 4539) & (mprestamos_personales < 37.011e+3) & (cdescubierto_preacordado == 0)"
predicados.logicos[["predicado_07"]] <- "(ctrx_quarter < 14) & (mcuentas_saldo >= -1256.1) & (mcaja_ahorro < 2601.1) & (Visa_fechaalta < 4539) & (mprestamos_personales < 37.011e+3) & (cdescubierto_preacordado == 1)"
predicados.logicos[["predicado_08"]] <- "(ctrx_quarter < 14) & (mcuentas_saldo >= -1256.1) & (mcaja_ahorro < 2601.1) & (Visa_fechaalta < 4539) & (mprestamos_personales >= 37.011e+3)"
predicados.logicos[["predicado_09"]] <- "(ctrx_quarter < 14) & (mcuentas_saldo >= -1256.1) & (mcaja_ahorro >= 2601.1) & (ctarjeta_master < 1)"
predicados.logicos[["predicado_10"]] <- "(ctrx_quarter < 14) & (mcuentas_saldo >= -1256.1) & (mcaja_ahorro >= 2601.1) & (ctarjeta_master >= 1)"
predicados.logicos[["predicado_11"]] <- "(ctrx_quarter >= 14) & (Master_status >= 8) & (ctrx_quarter < 30) & (mprestamos_personales < 14.871e+3)"
predicados.logicos[["predicado_12"]] <- "(ctrx_quarter >= 14) & (Master_status >= 8) & (ctrx_quarter < 30) & (mprestamos_personales >= 14.871e+3)"
predicados.logicos[["predicado_13"]] <- "(ctrx_quarter >= 14) & (Master_status >= 8) & (ctrx_quarter >= 30) & (mpayroll < 4715.9) & (ctarjeta_visa < 1)"
predicados.logicos[["predicado_14"]] <- "(ctrx_quarter >= 14) & (Master_status >= 8) & (ctrx_quarter >= 30) & (mpayroll < 4715.9) & (ctarjeta_visa >= 1) & (mcaja_ahorro < 135.48)"
predicados.logicos[["predicado_15"]] <- "(ctrx_quarter >= 14) & (Master_status >= 8) & (ctrx_quarter >= 30) & (mpayroll < 4715.9) & (ctarjeta_visa >= 1) & (mcaja_ahorro >= 135.48)"
predicados.logicos[["predicado_16"]] <- "(ctrx_quarter >= 14) & (Master_status >= 8) & (ctrx_quarter >= 30) & (mpayroll >= 4715.9) & (ccaja_ahorro >= 4)"
predicados.logicos[["predicado_17"]] <- "(ctrx_quarter >= 14) & (Master_status >= 8) & (ctrx_quarter >= 30) & (mpayroll >= 4715.9) & (ccaja_ahorro < 4) & (mcuenta_corriente >= 1144.3)"
predicados.logicos[["predicado_18"]] <- "(ctrx_quarter >= 14) & (Master_status >= 8) & (ctrx_quarter >= 30) & (mpayroll >= 4715.9) & (ccaja_ahorro < 4) & (mcuenta_corriente < 1144.3)"
predicados.logicos[["predicado_19"]] <- "(ctrx_quarter >= 14) & (Master_status < 8) & (ctrx_quarter < 47) & (mpasivos_margen < 74.005) & (cpayroll_trx < 1) & (mprestamos_personales < 4555.7)"
predicados.logicos[["predicado_20"]] <- "(ctrx_quarter >= 14) & (Master_status < 8) & (ctrx_quarter < 47) & (mpasivos_margen < 74.005) & (cpayroll_trx < 1) & (mprestamos_personales >= 4555.7)"
predicados.logicos[["predicado_21"]] <- "(ctrx_quarter >= 14) & (Master_status < 8) & (ctrx_quarter < 47) & (mpasivos_margen < 74.005) & (cpayroll_trx >= 1)"
predicados.logicos[["predicado_22"]] <- "(ctrx_quarter >= 14) & (Master_status < 8) & (ctrx_quarter < 47) & (mpasivos_margen >= 74.005) & (tcallcenter == 1)"
predicados.logicos[["predicado_23"]] <- "(ctrx_quarter >= 14) & (Master_status < 8) & (ctrx_quarter < 47) & (mpasivos_margen >= 74.005) & (tcallcenter == 0) & (mpasivos_margen < 237.42)"
predicados.logicos[["predicado_24"]] <- "(ctrx_quarter >= 14) & (Master_status < 8) & (ctrx_quarter < 47) & (mpasivos_margen >= 74.005) & (tcallcenter == 0) & (mpasivos_margen >= 237.42)"
predicados.logicos[["predicado_25"]] <- "(ctrx_quarter >= 14) & (Master_status < 8) & (ctrx_quarter >= 47) & (mpayroll < 7043.5) & (mtarjeta_visa_consumo < 2130.4) & (mcuentas_saldo < 14.406e+3)"
predicados.logicos[["predicado_26"]] <- "(ctrx_quarter >= 14) & (Master_status < 8) & (ctrx_quarter >= 47) & (mpayroll < 7043.5) & (mtarjeta_visa_consumo < 2130.4) & (mcuentas_saldo >= 14.406e+3)"
predicados.logicos[["predicado_27"]] <- "(ctrx_quarter >= 14) & (Master_status < 8) & (ctrx_quarter >= 47) & (mpayroll < 7043.5) & (mtarjeta_visa_consumo >= 2130.4) & (mpasivos_margen < 698.5)"
predicados.logicos[["predicado_28"]] <- "(ctrx_quarter >= 14) & (Master_status < 8) & (ctrx_quarter >= 47) & (mpayroll < 7043.5) & (mtarjeta_visa_consumo >= 2130.4) & (mpasivos_margen >= 698.5)"
predicados.logicos[["predicado_29"]] <- "(ctrx_quarter >= 14) & (Master_status < 8) & (ctrx_quarter >= 47) & (mpayroll >= 7043.5) & (mpayroll >= 1.773e+6)"
predicados.logicos[["predicado_30"]] <- "(ctrx_quarter >= 14) & (Master_status < 8) & (ctrx_quarter >= 47) & (mpayroll >= 7043.5) & (mpayroll < 1.773e+6) & (Visa_fechaalta < 54)"
predicados.logicos[["predicado_31"]] <- "(ctrx_quarter >= 14) & (Master_status < 8) & (ctrx_quarter >= 47) & (mpayroll >= 7043.5) & (mpayroll < 1.773e+6) & (Visa_fechaalta >= 54)"

aplicar.Predicados(dataset, predicados.logicos)
save.File(dataset, new.dataset.folder, new.dataset.name)