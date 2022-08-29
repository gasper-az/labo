#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\uba\\dmeyf\\")   #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/feature-engineering/v1.2/competencia1_2022_fe_v1.2.csv")   #TODO: cambiar path

columnas.a.quitar <- c(
)

if (length(columnas.a.quitar) > 0) {
  dataset[, c(columnas.a.quitar):=NULL] 
}

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#             fecha   cp          minsplit minbucket maxdepth xval_folds ganancia   iteracion
# 1: 20220828 210535 -0.9770186     1851       600       20          5    21070000       107
# 2: 20220828 211014 -0.6438273     1801       209        8          5    21012000       111

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      5,
                 cp=       -0.6438273,   #esto significa no limitar la complejidad de los splits
                 minsplit=  1801,     #minima cantidad de registros para que se haga el split
                 minbucket= 209,     #tamaño minimo de una hoja
                 maxdepth=  8)    #profundidad maxima del arbol


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
# TODO: cambiar el número del experimento
dir.create( "./exp/" )
dir.create( "./exp/KA2006" )
dir.create( "./exp/KA2006/v1.2" )
dir.create( "./exp/KA2006/v1.2/FeatureEngineering" )

# TODO: cambiar el número del experimento
fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2006/v1.2/FeatureEngineering/K101_001.csv",
        sep=  "," )

# guardo el modelo en un archivo, para aplicar F.E en sus reglas
# TODO: cambiar el número del experimento
saveRDS(modelo, "./exp/KA2006/v1.2/FeatureEngineering/modelo.v1.2.rda")