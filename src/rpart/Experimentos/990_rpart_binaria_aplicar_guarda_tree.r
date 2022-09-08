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


#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]


dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo


# Entreno el modelo
# obviamente rpart no puede ve  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

# mis resultados:
# fecha	          	cp            	minsplit		minbucket	  maxdepth	xval_folds	ganancia		iteracion
# 20220908 005934	-0.999923287	1206.212084		60.83645159		8			    5			22346666.67		111
# 20220908 005507	-0.669933415	1314.611169		112.4862817		9			    5			22320000		97
# 20220908 005451	-0.750623841	895.2157353		126.6267952		9			    5			22240000		96
# 20220908 005209	-0.62607374		961.3509085		63.09667341		9			    5			22160000		87
# 20220908 003550	-0.628447147	504.723472		70.51516441		8			    5			22146666.67		29
# 20220908 005919	-0.512061991	1282.610027		119.3122105		9			    5			22053333.33		110
# 20220908 004639	-0.641452032	1090.822929		131.2605219		8			    5			2.20E+07		67
# 20220908 005422	-0.625708342	1031.662706		68.25559884		8			    5			21973333.33		94
# 20220908 005834	-0.999942033	1487.795192		130.9809627		13		    5			21933333.33		107

# 20220908 010023	-0.999966889	805.1090635		223.2513189		8			    5			21906666.67		114   
# 20220908 004624	-0.77040592		1210.986365		167.2060917		8			    5			21866666.67		66
# 20220908 010038	-0.966660921	1195.048535		97.46881016		7			    5			21826666.67		115
# 20220908 005652	-0.687040021	1019.049236		290.0131553		19		    5			21813333.33		102
# 20220908 005315	-0.701911592	1306.549261		74.59456546		18		    5			21746666.67		90
# 20220908 004712	-0.786028921	825.8128782		29.21903638		9			    5			21666666.67		69
# 20220908 005731	-0.832812661	1375.852003		233.1407041		13		    5			21666666.67		104
# 20220908 005029	-0.640584956	357.2626281		177.518469		8			    5			21600000		81
# 20220908 003534	-0.523714731	33.62363126		1.01302907		8			    5			21586666.67		28
# 20220908 004032	-0.689318728	848.3539744		1.460946733		8			    5			21573333.33		46
# 20220908 005753	-0.999960521	1247.500284		170.032989		18			5			21546666.67		105

# 20220908 004511	-0.721781683	419.8622938		202.2662566		9			5			21520000		62
# 20220908 004048	-0.267344085	21.17700548		4.889117449		8			5			21453333.33		47
# 20220908 004017	-0.442855655	649.3676414		2.060231356		9			5			21426666.67		45
# 20220908 004915	-0.631701184	725.3519015		10.87094026		7			5			21426666.67		77
# 20220908 003607	-0.704270217	1185.454556		207.0173349		10			5			21386666.67		30

# 20220908 003502	-0.547091686	945.7404496		468.6570617		9			5			21373333.33		26        ===> Uso este en la versión 1.5.1
# 20220908 003715	-0.747114675	10.52440001		3.873634526		8			5			21373333.33		34
# 20220908 004252	-0.85382971		654.8167566		198.1667215		15			5			21373333.33		54
# 20220908 004901	-0.725941811	1896.100107		111.9863284		8			5			21360000		76
# 20220908 005407	-0.564573484	469.554429		22.16660748		8			5			21320000		93
# 20220908 003744	-0.584609395	77.99139465		38.70484731		6			5			21306666.67		36
# 20220908 005435	-0.533018871	318.7794139		143.8048541		5			5			21253333.33		95
# 20220908 005853	-0.999937565	1159.287725		362.5364435		12			5			21240000		108
# 20220908 003659	-0.793744102	391.3837984		191.1218345		12			5			21226666.67		33

# 20220908 003936	-0.598989289	533.152021		264.7374644		8			5			21213333.33		42



modelo  <- rpart(formula=   "clase_binaria ~ .  -Visa_mpagado -mcomisiones_mantenimiento -clase_ternaria",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=         5,
                 cp=          -0.547091686,#  -0.54, -0.89
                 minsplit=  946,   # 1073, 621
                 minbucket=  469,   # 278, 309
                 maxdepth=     9)  #  9, 12


names(head(modelo$variable.importance, 20))
#----------------------------------------------------------------------------
# habilitar esta seccion si el Fiscal General  Alejandro Bolaños  lo autoriza
#----------------------------------------------------------------------------

# corrijo manualmente el drifting de  Visa_fultimo_cierre
# dapply[ Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
# dapply[ Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
# dapply[ Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
# dapply[ Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
# dapply[ Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
# dapply[ Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
# dapply[ Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]

# corrijo manualmente el drifting de  Visa_fultimo_cierre
# dapply[ Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
# dapply[ Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
# dapply[ Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
# dapply[ Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
# dapply[ Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
# dapply[ Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
# dapply[ Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]


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
dir.create( "./exp/KA4120/v1.5/v1.5.1" )
dir.create( "./exp/KA4120/v1.5/v1.5.1/FeatureEngineering" )


for( corte  in  c( 7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000 ) )
{
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]


  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
           file= paste0( "./exp/KA4120/v1.5/v1.5.1/FeatureEngineering/KA4120_005_",  corte, ".csv"),
           sep=  "," )
}


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)

# TODO: cambiar el número del experimento
saveRDS(modelo, "./exp/KA4120/v1.5/v1.5.1/FeatureEngineering/modelo.v1.5.1.rda")