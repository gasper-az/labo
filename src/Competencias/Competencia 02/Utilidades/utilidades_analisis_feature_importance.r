#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

# Defino directorio y archivos de importancia a analizar
working.directory <- "C:\\uba\\repos\\labo\\src\\Competencias\\Competencia 02\\Experimentos\\Experimento 01\\KA7241"
salida.importancia <- "impo.txt"
setwd(working.directory)

# cargo el archivo de importancia 
dataset.importancia  <- fread(salida.importancia)

# defino la medida
# puede ser: Gain, Cover, Frequency
medidas <- c("Gain", "Cover", "Frequency")

# Cantidad de variables a graficar
top_n <- 10L

for (medida in medidas) {
  lgb.plot.importance(dataset.importancia, top_n = top_n, measure = (medida))
}

dataset.importancia[, todas := Gain * Cover * Frequency]

dataset.importancia.ordenado <- dataset.importancia[order(-rank(todas))]
dataset.importancia.ordenado <- dataset.importancia.ordenado[, head(.SD, 10)]

## El siguiente código corresponde a la implementación de lgb.plot.importance
## src: https://github.com/microsoft/LightGBM/blob/master/R-package/R/lgb.plot.importance.R

# Refresh plot
op <- graphics::par(no.readonly = TRUE)
on.exit(graphics::par(op))

graphics::par(
  mar = c(
    op$mar[1L]
    , 10L
    , op$mar[3L]
    , op$mar[4L]
  )
)
dataset.importancia.ordenado[rev(seq_len(.N)),
                    graphics::barplot(
                      height = todas
                      , names.arg = Feature
                      , horiz = TRUE
                      , border = NA
                      , main = "Feature Importance"
                      , xlab = "Gain * Cover * Frequency"
                      , las = 1L
                      , cex.names = (cex <- 2.5 / log2(1.0 + top_n))
                    )]