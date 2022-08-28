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
dataset  <- fread("./datasets/competencia1_2022.csv")

columnas.a.quitar <- c(
)

if (length(columnas.a.quitar) > 0) {
  dataset[, c(columnas.a.quitar):=NULL] 
}

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -0.4005395,   #esto significa no limitar la complejidad de los splits
                 minsplit=  50,     #minima cantidad de registros para que se haga el split
                 minbucket= 23,     #tamaño minimo de una hoja
                 maxdepth=  7)    #profundidad maxima del arbol


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

# TODO: cambiar el número del experimento
fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2006/v1.2/K101_001.csv",
        sep=  "," )

# guardo el modelo en un archivo, para aplicar F.E en sus reglas
# TODO: cambiar el número del experimento
saveRDS(modelo, "./exp/KA2006/v1.2/modelo.v1.2.rda")