#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

setwd("C:\\uba\\dmeyf" )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset  <- fread( "./datasets/competencia1_2022.csv")

#uso esta semilla para los canaritos
set.seed(763369)

#agrego una variable canarito, random distribucion uniforme en el intervalo [0,1]
dataset[ ,  canarito1 :=  runif( nrow(dataset) ) ]

#agrego los siguientes canaritos
for( i in 13:100 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]


#Primero  veo como quedan mis arboles
  modelo  <- rpart(formula= "clase_ternaria ~ . -mcomisiones_mantenimiento -Visa_mpagado",
                 data= dataset[ foto_mes==202101 ,],
                 model= TRUE,
                 xval= 5, #0,
                 cp= -0.547091686,# -1,
                 minsplit= 946, #200,
                 minbucket= 469, #100,
                 maxdepth= 9) #10)


pdf(file = "C:/uba/repos/labo/src/RepensandoOverfitting/Clase05/arbol_canaritos_desconfiado_BO.pdf", width=28, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

