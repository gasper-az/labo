#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require(ggplot2)

PARAM  <- list()
PARAM$base_dir <- "C:\\uba\\dmeyf\\exp\\Predicciones"
# PARAM$experimentos_pred_dirs <- c("modelo_01_bis", "modelo_02", "modelo_06", "modelo_07")
# PARAM$experimentos_a_promediar <- c("modelo_01_bis", "modelo_02", "modelo_06", "modelo_07")
PARAM$experimentos_pred_dirs <- c("modelo_01_bis", "modelo_06", "modelo_07")
PARAM$experimentos_a_promediar <- c("modelo_01_bis", "modelo_06", "modelo_07")

PARAM$experimentos_test_valid_invertidos <- c("modelo_06", "modelo_07")

PARAM$pred_file_name_pattern <- "pred.csv"
PARAM$test_file_name_pattern <- "pred_test.csv"
PARAM$vali_file_name_pattern <- "pred_vali.csv"

setwd(PARAM$base_dir)

##########################################################
#################### Funciones útiles ####################
##########################################################

calcularGanancia <- function(dt) {
  # tb_comparacion <- merge(real, predicho)
  # # Estoy seguro que tiene que existir una forma menos horrible de escribir la siguiente expresión
  # return (tb_comparacion[, sum(ifelse(clase_real == 1 & Predicted == 1, 78000, ifelse(clase_real == 0 & Predicted == 1, -2000, 0)))])

  return (dt[, sum(ifelse(clase_real == 1 & Predicted == 1, 78000, ifelse(clase_real == 0 & Predicted == 1, -2000, 0))) ])
}

###################################################################
#################### Aquí comienza el programa ####################
###################################################################

envios <- seq( from = 7000, to = 18000, by = 250 )

cols <- c("modelo", "corte", "ganancia")
df.test <- data.frame(matrix(nrow = 0, ncol = length(cols)))
colnames(df.test) <- cols

df.vali <- data.frame(matrix(nrow = 0, ncol = length(cols)))
colnames(df.vali) <- cols

for (experimento_dir in PARAM$experimentos_pred_dirs) {
  if (experimento_dir %in% PARAM$experimentos_test_valid_invertidos) {
    dt.test <- fread(paste(experimento_dir, PARAM$vali_file_name_pattern, sep = "/"))
    dt.vali <- fread(paste(experimento_dir, PARAM$test_file_name_pattern, sep = "/"))
  } else {
    dt.test <- fread(paste(experimento_dir, PARAM$test_file_name_pattern, sep = "/"))
    dt.vali <- fread(paste(experimento_dir, PARAM$vali_file_name_pattern, sep = "/"))
  }
  
  
  dt.test[, clase_real := ifelse(clase_ternaria == "BAJA+2", 1, 0)]
  dt.vali[, clase_real := ifelse(clase_ternaria == "BAJA+2", 1, 0)]
  
  setorder( dt.test, -prob )
  setorder( dt.vali, -prob )

  for (envio in envios) {
    dt.test[, Predicted := 0 ]
    dt.vali[, Predicted := 0 ]
    
    dt.test[1:envio, Predicted := 1L]
    dt.vali[1:envio, Predicted := 1L]
    
    # agrego las ganancias
    gan.test <- calcularGanancia(dt.test)
    gan.vali <- calcularGanancia(dt.vali)
    
    
    df.test[nrow(df.test) + 1, ] <- c(experimento_dir, as.integer(envio), as.double(gan.test))
    df.vali[nrow(df.vali) + 1, ] <- c(experimento_dir, as.integer(envio), as.double(gan.vali))
  }
}

# Calculo el promedio de los modelos que me interesan
for (envio in envios) {
  avg.test <- mean(as.double(df.test[df.test$modelo %in% PARAM$experimentos_a_promediar & df.test$corte == envio, "ganancia"]))
  avg.vali <- mean(as.double(df.vali[df.vali$modelo %in% PARAM$experimentos_a_promediar & df.vali$corte == envio, "ganancia"]))
  
  df.test[nrow(df.test) + 1, ] <- c("average", as.integer(envio), as.double(avg.test))
  df.vali[nrow(df.vali) + 1, ] <- c("average", as.integer(envio), as.double(avg.vali))
}

dt.test <- as.data.table(df.test)
dt.test[, corte := as.integer(corte)]
dt.test[, ganancia := as.double(ganancia)]

ggplot(dt.test, aes(corte, y=ganancia, group = modelo, color = modelo)) +
  geom_line() +
  ggtitle("Fold Test: envíos vs ganancia") +
  xlab("cantidad de envíos") +
  ylab("Ganancia") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(labels=as.factor(dt.test[, corte]),breaks=dt.test[, corte])

dt.vali <- as.data.table(df.vali)
dt.vali[, corte := as.integer(corte)]
dt.vali[, ganancia := as.double(ganancia)]

ggplot(dt.vali, aes(x=corte, y=ganancia, group = modelo, color = modelo)) +
  geom_line() +
  ggtitle("Fold Validate: envíos vs ganancia") +
  xlab("cantidad de envíos") +
  ylab("Ganancia") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(labels=as.factor(dt.vali[, corte]),breaks=dt.vali[, corte])
