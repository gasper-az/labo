#Aplicacion de los mejores hiperparametros encontrados en una bayesiana
#Utilizando clase_binaria =  [  SI = { "BAJA+1", "BAJA+2"} ,  NO="CONTINUA ]

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")


#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\uba\\dmeyf\\")   #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv" )

# Feature engineering
dataset[, predicado_01 := ifelse((ctrx_quarter >= 14 & ctrx_quarter < 30 & mcaja_ahorro >= 16275 & mtarjeta_visa_consumo >= 3748), 1, 0)]
dataset[, predicado_02 := ifelse((ctrx_quarter >= 14 & ctrx_quarter < 30 & mcaja_ahorro >= 2604 & mcaja_ahorro < 7610 & mtarjeta_visa_consumo >= 3748), 1, 0)]
dataset[, predicado_03 := ifelse((ctrx_quarter <  14 & mcuentas_saldo >= 366 & (Master_fechaalta < 468 | is.na(Master_fechaalta)) & ctarjeta_visa >= 0.5), 1, 0)]
dataset[, predicado_04 := ifelse((ctrx_quarter >= 30 & ctrx_quarter < 49 & mcaja_ahorro >= 15 & mcaja_ahorro < 573 & Visa_msaldototal >= 3527 & !is.na(Visa_msaldototal)), 1, 0)]
dataset[, predicado_05 := ifelse((ctrx_quarter >= 14 & ctrx_quarter < 30 & mcaja_ahorro < 2604 & (Visa_msaldototal < 1624 | is.na(Visa_msaldototal)) & mprestamos_personales >= 14858), 1, 0)]
dataset[, predicado_06 := ifelse((ctrx_quarter <  14 & mcuentas_saldo < -1256 & cprestamos_personales >= 1.5), 1, 0)]
dataset[, predicado_07 := ifelse((ctrx_quarter >= 30 & ctrx_quarter < 49 & mcaja_ahorro < 573 & (Visa_msaldototal < 3527 | is.na(Visa_msaldototal))), 1, 0)]
dataset[, predicado_08 := ifelse((ctrx_quarter >= 14 & ctrx_quarter < 30 & mcaja_ahorro < 439 & mpasivos_margen >= 5.8 & (Visa_msaldototal < 1624 | is.na(Visa_msaldototal)) & mprestamos_personales <  14858), 1, 0)]
dataset[, predicado_09 := ifelse((ctrx_quarter >= 14 & ctrx_quarter < 30 & mcaja_ahorro >= 439 & mcaja_ahorro < 2604 & mpasivos_margen >= 5.8 & (Visa_msaldototal < 1624 | is.na(Visa_msaldototal)) & mprestamos_personales < 14858), 1, 0)]
dataset[, predicado_10 := ifelse((ctrx_quarter <  14 & mcuentas_saldo < -1256 & (Master_fechaalta < 793 | is.na(Master_fechaalta)) & cprestamos_personales < 1.5 & ctarjeta_master >= 0.5), 1, 0)]
dataset[, predicado_11 := ifelse((ctrx_quarter >= 14 & ctrx_quarter < 30 & mcaja_ahorro <  2604 & mpasivos_margen < 5.8 & (Visa_msaldototal < 1624 | is.na(Visa_msaldototal)) & mprestamos_personales < 14858), 1, 0)]
dataset[, predicado_12 := ifelse((ctrx_quarter <  14 & mcuentas_saldo < -1256 & Master_fechaalta >= 793 & !is.na(Master_fechaalta) & cprestamos_personales < 1.5 & ctarjeta_master >= 0.5), 1, 0)]
dataset[, predicado_13 := ifelse((ctrx_quarter <  14 & mcuentas_saldo < -1256 & cprestamos_personales < 1.5 & ctarjeta_master <  0.5), 1, 0)]


#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]


dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo


# Entreno el modelo
# obviamente rpart no puede ve  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

# mis resultados:
# fecha			      cp				    minsplit	  minbucket	  maxdepth	xval_folds	ganancia		iteracion
# 20220908 043923	-0.497441301	1421.061337	110.5774895	15			5			22493333.33		78                ===> Utilizo este para el v1.5.2 y v1.5.3 (esta última aplica data drifting)
# 20220908 045148	-0.999975611	1766.373991	102.9455509	17			5			22466666.67		114

# 20220908 043515	-0.392226668	1293.914332	89.18725811	14			5			22426666.67		66                ===> utilizo para v1.5.5

# 20220908 043841	-0.559106273	1638.043231	126.3152622	16			5			22346666.67		76
# 20220908 042847	-0.367361085	991.4506996	105.0783475	20			5			22320000		48
# 20220908 044234	-0.494918764	1497.776964	123.3637543	14			5			22280000		87
# 20220908 044543	-0.38467727		1317.103632	95.83824552	20			5			22280000		96
# 20220908 043205	-0.382901556	1228.657693	30.90702078	20			5			22253333.33		57
# 20220908 043419	-0.422729943	1468.679858	88.4977345	20			5			22053333.33		63
# 20220908 044709	-0.510435405	1288.155792	98.9384287	20			5			22040000		100
# 20220908 043944	-0.639678996	1418.057752	159.3529733	13			5			21973333.33		79
# 20220908 042500	-0.256722239	1293.326129	326.6622914	13			5			21960000		37
# 20220908 043354	-0.187489324	929.7876112	322.894778	20			5			21946666.67		62
# 20220908 043653	-0.101152555	617.6898623	06.5699948	14			5			21933333.33		71
# 20220908 044149	-0.187632544	1228.723042	38.47842717	13			5			21920000		85
# 20220908 042417	-0.428106111	1041.794688	316.59899	16			5			21906666.67		35
# 20220908 043630	-0.21347829		1357.721254	316.5219643	17			5			21906666.67		70
# 20220908 044008	-0.574195614	1210.036696	157.266693	20			5			21906666.67		80
# 20220908 044213	-0.646563895	997.4365512	162.3018737	20			5			21893333.33		86
# 20220908 044754	-0.489019537	1329.675555	202.6479737	20			5			21893333.33		102
# 20220908 042356	-0.395474783	622.5916141	307.8224372	20			5			21866666.67		34
# 20220908 042604	-0.405167282	1402.609249	200.7936433	17			5			21813333.33		40
# 20220908 044413	-0.388328942	1596.372191	282.0462016	20			5			21746666.67		92
# 20220908 044513	-0.083832149	1538.263043	15.7090477	20			5			21720000		95
# 20220908 042744	-0.442293059	1631.728204	1.444717569	11			5			21640000		45
# 20220908 044604	-0.187468673	1340.459432	166.60021	14			5			21640000		97
# 20220908 043739	-0.11551062		1022.201775	273.8385656	20			5			21613333.33		73
# 20220908 044300	-0.627205269	1551.031686	27.52618659	20			5			21573333.33		88
# 20220908 044029	-0.714972918	1590.470265	340.7484519	16			5			21533333.33		81
# 20220908 043902	-0.595498146	2034.049703	316.1334611	15			5			21426666.67		77
# 20220908 044626	-0.265192504	1159.335604	300.6360177	20	5	21386666.67	98
# 20220908 043139	-0.353374845	796.1757762	273.5066292	20	5	21373333.33	56
# 20220908 041437	-0.392653159	1248.533694	354.123288	9	  5	21360000	1

# 20220908 043304	-0.426880397	1266.426979	304.2984535	9	  5	21360000	60                  ===> utilizo para v1.5.4

# 20220908 044128	-0.462582909	1314.726079	131.6300142	16	5	21346666.67	84
# 20220908 044452	-0.637743372	701.9041757	331.5991054	16	5	21346666.67	94
# 20220908 043247	-0.375469371	1155.209304	247.6126025	19	5	21333333.33	59
# 20220908 045024	-0.57377371	  1669.967599	6.716119974	9	  5	21320000	110



modelo  <- rpart(formula=   "clase_binaria ~ .  -Visa_mpagado -mcomisiones_mantenimiento -clase_ternaria",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=         5,
                 cp=          -0.392226668,#  -0.54, -0.89
                 minsplit=  1294,   # 1073, 621
                 minbucket=  90,   # 278, 309
                 maxdepth=     14)  #  9, 12


names(head(modelo$variable.importance, 20))
#----------------------------------------------------------------------------
# habilitar esta seccion si el Fiscal General  Alejandro Bolaños  lo autoriza
#----------------------------------------------------------------------------

# corrijo manualmente el drifting de  Visa_fultimo_cierre
dapply[ Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
dapply[ Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
dapply[ Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
dapply[ Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
dapply[ Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
dapply[ Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
dapply[ Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]

# corrijo manualmente el drifting de  Visa_fultimo_cierre
dapply[ Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
dapply[ Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
dapply[ Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
dapply[ Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
dapply[ Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
dapply[ Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
dapply[ Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]


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
set.seed(763369)  
dfinal[ , azar := runif( nrow(dapply) ) ]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder( dfinal, -prob_SI, azar )


dir.create( "./exp/" )
dir.create( "./exp/KA4120" )
dir.create( "./exp/KA4120/v1.5" )
dir.create( "./exp/KA4120/v1.5/v1.5.5" )
dir.create( "./exp/KA4120/v1.5/v1.5.5/FeatureEngineering" )


for( corte  in  c( 7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000 ) )
{
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]


  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
           file= paste0( "./exp/KA4120/v1.5/v1.5.5/FeatureEngineering/KA4120_005_",  corte, ".csv"),
           sep=  "," )
}


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)

# TODO: cambiar el número del experimento
saveRDS(modelo, "./exp/KA4120/v1.5/v1.5.5/FeatureEngineering/modelo.v1.5.5.rda")