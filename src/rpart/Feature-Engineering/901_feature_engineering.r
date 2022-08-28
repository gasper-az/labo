rm(list = ls())
gc()

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

procesar.archivo.reglas <- function(my.hash, rules.file.path) {
  rules.file <- file(rules.file.path, "r")
  lines <- readLines(rules.file)
  
  i <- 0
  
  for (line in lines) {
    name <- paste0("predicado_", i)
    i <- i + 1
    my.hash[[name]] <- trimws(line)
  }
  
  close(rules.file)
}

#---------------------------------------------------------------#
#-------------------- Variables - Carpertas --------------------#
#---------------------------------------------------------------#

# TODO: cambiar valores según la ubicación del repo
base.path <- "C:\\uba\\dmeyf\\"
dataset.path <- "./datasets/competencia1_2022.csv"
new.dataset.folder <- "./datasets/feature-engineering/v1.1/"
new.dataset.name <- "competencia1_2022_fe_v1.csv"

rules.file.path <- "./exp/KA2006/v1.1/rules.v1.1.txt"

#---------------------------------------------------------------#
#-------------------- Acá comienza el programa -----------------#
#---------------------------------------------------------------#

setwd(base.path)

dataset <- fread(dataset.path)

predicados.logicos <- hash()
procesar.archivo.reglas(predicados.logicos, rules.file.path)

aplicar.Predicados(dataset, predicados.logicos)
save.File(dataset, new.dataset.folder, new.dataset.name)