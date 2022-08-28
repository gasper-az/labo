rm(list = ls())
gc()

require("data.table")
require("rpart")
require("rpart.plot")

#---------------------------------------------------------------#
#-------------------- Funciones de utilidad --------------------#
#---------------------------------------------------------------#

`%notin%` <- Negate(`%in%`)

save.rules <- function(model, file) {
  rules <- rpart.rules(model, style ="wide", eq = "==")
  pasted.rules <- do.call(paste, rules)
  parsed.rules <- unlist(lapply(pasted.rules, parse.rule))
  fwrite(list(parsed.rules), file)
}


parse.rule <- function(rule) {
  trimmed.rules <- trimws(unlist(strsplit(rule, "when")))
  if (!is.na(trimmed.rules[2])) {
    return(trimmed.rules[2])
  } else {
    return("")
  }
}

#---------------------------------------------------------------#
#-------------------- Variables - Carpertas --------------------#
#---------------------------------------------------------------#

# TODO: cambiar valores según la ubicación del repo
# TODO: cambiar valores según número de experimento
base.path <- "C:\\uba\\dmeyf\\"
folder.path <- "./exp/KA2006/v1.1/"
model.file.name <- "modelo.v1.1.rda"
rules.file.name <- "rules.v1.1.txt"

model.file.path <- paste0(folder.path, model.file.name)
rules.file.path <- paste0(folder.path, rules.file.name)


#---------------------------------------------------------------#
#-------------------- Acá comienza el programa -----------------#
#---------------------------------------------------------------#

setwd(base.path)

modelo <- readRDS(model.file.path)

save.rules(modelo, rules.file.path)