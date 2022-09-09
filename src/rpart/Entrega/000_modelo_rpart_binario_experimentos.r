rm(list=ls())
gc()

require("data.table")
require("rpart")
require("rpart.plot")

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

#------------------------------------------------------------#
#-------------------- Creo clase binaria --------------------#
#------------------------------------------------------------#

dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

#-------------------------------------------------------------------#
#-------------------- Divido en train y testing --------------------#
#-------------------------------------------------------------------#

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#---------------------------------------------------------------------------------------------#
#-------------------- Marco variables en la que identifiqué Data Drifting --------------------#
#---------------------------------------------------------------------------------------------#

variables.drifting <- c(
  "Visa_mpagado",
  "mcomisiones_mantenimiento"
)

#--------------------------------------------------------------------------------------------#
#-------------------- Defino variables a quitar de la fórmula del modelo --------------------#
#--------------------------------------------------------------------------------------------#

variables.sacar <- c(
  "clase_ternaria",
  variables.drifting
)

#-----------------------------------------------------------------#
#-------------------- Creo fórmula del modelo --------------------#
#-----------------------------------------------------------------#

formula.modelo <- "clase_binaria ~ ."

for (variable in variables.sacar) {
  formula.modelo <- paste(formula.modelo, variable, sep = " -")
}

#--------------------------------------------------------#
#-------------------- Creo el modelo --------------------#
#--------------------------------------------------------#

modelo  <- rpart(
  formula  = formula.modelo,
  data     = dtrain,
  xval     = 0,
  cp       = -0.54,
  minsplit = 1073,
  minbucket= 278,
  maxdepth = 9
)

#-------------------------------------------------------------------------------#
#-------------------- Aplico modelo a datos nuevos (202103) --------------------#
#-------------------------------------------------------------------------------#

prediccion  <- predict( object=  modelo,
                        newdata= dapply,
                        type = "prob")

#---------------------------------------------------------------------------------#
#-------------------- Creo columna con probabilidad de BAJA+2 --------------------#
#---------------------------------------------------------------------------------#

#prediccion es una matriz con DOS columnas, llamadas "NO", "SI"
#cada columna es el vector de probabilidades 
#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dfinal  <- copy(dapply[, list(numero_de_cliente)])
dfinal[, prob_SI := prediccion[, "SI"]]
set.seed(semilla)
dfinal[, azar := runif(nrow(dapply))]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder(dfinal, -prob_SI, azar)

#-------------------------------------------------------------------------------------------------------------------------#
#-------------------- Grabo salida para kaggle, en función de los cortes y la probabilidad de prob_si --------------------#
#-------------------------------------------------------------------------------------------------------------------------#

dir.create( "./exp/" )
dir.create( "./exp/HT0909" )
dir.create( "./exp/HT0909/v1.0" )

for(corte in c(7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000)) {
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]

  fwrite(
    dfinal[, list(numero_de_cliente, Predicted)], #solo los campos para Kaggle
    file= paste0("./exp/HT0909/v1.0/KA4120_005_", corte, ".csv"),
    sep=  ","
  )
}

#-------------------------------------------------------------------#
#-------------------- Guardo el modelo como PDF --------------------#
#-------------------------------------------------------------------#

pdf(file = "./exp/HT0909/v1.0/rpart.pdf", width=28, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()