require("data.table")
require("rpart")
require("rpart.plot")

#---------------------------------------------------------------#
#-------------------- Funciones de utilidad --------------------#
#---------------------------------------------------------------#

`%notin%` <- Negate(`%in%`)

Aplicar.Predicados <- function (dataset, predicados) {
  for (predicado in predicados) {
    if (predicado %notin% colnames(dataset)) {
      # predicado[1] devuelve el valor literal, lo usamos como colname
      # eval(parse()) nos permite evaluar el predicado
      dataset[, predicado[1] := eval(parse(text = predicado))] 
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

predicados.logicos <- c(
  "ctrx_quarter <14",
  "Visa_fechaalta >= 4539"
)

aplicar.predicados(dataset, predicados.logicos)
save.File(dataset, new.dataset.folder, new.dataset.name)