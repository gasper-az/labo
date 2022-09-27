require("data.table")
require("lightgbm")

# Defino directorio y archivos de importancia a analizar
working.directory <- "C:\\uba\\repos\\labo\\src\\lightgbm\\LightGbm\\Salidas\\HT7232"
salida.importancia <- "impo_31.txt"
setwd(working.directory)

# cargo el archivo de importancia 
dataset.importancia  <- fread(salida.importancia)

# defino la medida
# puede ser: Gain, Cover, Frequency
medidas <- c("Gain", "Cover", "Frequency")

for (medida in medidas) {
  lgb.plot.importance(dataset.importancia, top_n = 25, measure = medida) 
}