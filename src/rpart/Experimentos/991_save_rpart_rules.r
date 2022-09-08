rm(list = ls())
gc()

require("data.table")
require("rpart")
require("rpart.plot")
require("genTS") # para utiliar "is_empty"

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
    return(fix.to.pattern.in.rule(trimmed.rules[2]))
  } else {
    return("")
  }
}

fix.to.pattern.in.rule <- function(rule) {
  # busco patrones del tipo "columna == num1 to num2"
  # la idea es cambiarlo por algo como "columna >= num1 and columna < num2"
  my.regex <- "\\w{1,}\\s{1,}==\\s{1,}([+\\-]?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+\\-]?\\d+)?)\\s{1,}to\\s{1,}([+\\-]?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+\\-]?\\d+)?)"
  
  is.present <- grepl(my.regex, rule)
  
  if (!is.present) {
    return(rule)
  }
  
  res <- gregexec(my.regex, rule)
  matches.list <- regmatches(rule, res)
  
  # regex para numeros
  # regex.number <- "\\d+\\.*\\d*"
  # regex.number <- "(-?[0-9]*)((\\.?[0-9]+[eE]?[-\\+]?[0-9]+)|(\\.[0-9]+))*"
  regex.number <- "[+\\-]?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+\\-]?\\d+)?"
  
  # regex para texto
  regex.text <- "\\w{1,}"
  
  for (matches.row in matches.list) {
    for (matches in matches.row) {
      numbers <- gregexpr(regex.number, matches)
      matches.number <- regmatches(matches, numbers)
      
      text <- regexpr(regex.text, matches)
      match.text <- regmatches(matches, text)
      
      first.number <- matches.number[[1]][1]
      second.number <- matches.number[[1]][2]
      
      if (!is_empty(match.text) && !is_empty(first.number) && !is_empty(second.number)) {
        new.line <- paste0(match.text, " >= ", first.number, " & ", match.text, " < ", second.number)
        # rule <- gsub(matches, new.line, rule) 
        rule <- stri_replace(rule, new.line, fixed = matches)
      }
    }
  }
  
  return(rule)
}


#---------------------------------------------------------------#
#-------------------- Variables - Carpertas --------------------#
#---------------------------------------------------------------#

# TODO: cambiar valores según la ubicación del repo
# TODO: cambiar valores según número de experimento
base.path <- "C:\\uba\\dmeyf\\"
folder.path <- "./exp/KA4120/v1.5/v1.5.1/FeatureEngineering/"
model.file.name <- "modelo.v1.5.1.rda"
rules.file.name <- "rules.v1.5.1.txt"

model.file.path <- paste0(folder.path, model.file.name)
rules.file.path <- paste0(folder.path, rules.file.name)


#---------------------------------------------------------------#
#-------------------- Acá comienza el programa -----------------#
#---------------------------------------------------------------#

setwd(base.path)

modelo <- readRDS(model.file.path)

save.rules(modelo, rules.file.path)