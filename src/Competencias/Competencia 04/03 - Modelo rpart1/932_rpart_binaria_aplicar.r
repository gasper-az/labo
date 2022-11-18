#Aplicacion de los mejores hiperparametros encontrados en una bayesiana
#Utilizando clase_binaria =  [  SI = { "BAJA+1", "BAJA+2"} ,  NO="CONTINUA ]

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")



PARAM  <- list()
PARAM$semilla <- 763369
PARAM$mes_futuro <- 202109
PARAM$ultimo_mes_clase <- 202107
PARAM$experimento <- "RP932_compFinal_modelo03"
PARAM$exp_input  <- "FE9250_compFinal_modelo03"
PARAM$file_name  <- "dataset.csv.gz"

setwd( "~/buckets/b1/" )

#cargo el dataset
dataset  <- fread(paste0("./exp/", PARAM$exp_input, "/", PARAM$file_name))


#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==PARAM$ultimo_mes_clase, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]


dtrain  <- dataset[ foto_mes==PARAM$ultimo_mes_clase ]
dapply  <- dataset[ foto_mes==PARAM$mes_futuro ]


# Entreno el modelo
# obviamente rpart no puede ve  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dtrain,
                 xval=      0,
                 cp=        -0.54,
                 minsplit=  1073,
                 minbucket= 278,
                 maxdepth=  9)


#aplico el modelo a los datos nuevos
prediccion  <- predict( object=  modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con DOS columnas, llamadas "NO", "SI"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dfinal  <- copy( dapply[ , list(numero_de_cliente) ] )
dfinal[ , prob_SI := prediccion[ , "SI"] ]


# por favor cambiar por una semilla propia
# que sino el Fiscal General va a impugnar la prediccion
set.seed(PARAM$semilla)  
dfinal[ , azar := runif( nrow(dapply) ) ]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder( dfinal, -prob_SI, azar )

dir.create( paste0("./exp/", PARAM$experimento) )

fwrite( dfinal,
        file= "prediccion_rpart.csv",
        sep= "\t" )

# Los cortes por el momento no son necesarios

# for( corte  in  c( 7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000 ) )
# {
#   #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
#   dfinal[ , Predicted := 0L ]
#   dfinal[ 1:corte , Predicted := 1L ]
# 
# 
#   fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
#            file= paste0( "./exp/KA4120/KA4120_005_",  corte, ".csv"),
#            sep=  "," )
# }
