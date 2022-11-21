#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("primes")
require("lightgbm")

PARAM <- list()
PARAM$semilla_particion <- 763369
PARAM$semilla <- 763381

setwd("C:\\uba\\repos\\labo\\src\\Competencias\\Competencia 04\\Comparaciones\\Mediante gráficas\\02 - Modelo LightGBM 2")

## Leo el archivo de predicción
tb_prediccion  <- fread( "tb_prediccion.txt" )

tb_cortes  <- data.table(  particion=integer(),
                           semilla= integer(),
                           corte= integer(),
                           ganancia= numeric(),
                           public_gan=  numeric(),
                           private_gan= numeric() )

for( corte in seq( from=2000, to=20000, by=100 ) )
{
  total    <- tb_prediccion[ x <= corte,  sum( ganancia,    na.rm=TRUE ) ]
  public   <- tb_prediccion[ x <= corte & fold==1, 2*sum( ganancia, na.rm=TRUE ) ]
  private  <- tb_prediccion[ x <= corte & fold==2, 2*sum( ganancia, na.rm=TRUE ) ]
  
  tb_cortes  <-  rbind( tb_cortes,
                        list( PARAM$semilla_particion,
                              PARAM$semilla,
                              corte,
                              total,
                              public,
                              private ) )
}

setwd("C:\\uba\\repos\\labo\\src\\Competencias\\Competencia 04\\Comparaciones\\Mediante gráficas\\05 - Modelo LightGBM 4")

## Leo el archivo de predicción
tb_prediccion2  <- fread( "tb_prediccion.txt" )

tb_cortes2  <- data.table(  particion=integer(),
                           semilla= integer(),
                           corte= integer(),
                           ganancia= numeric(),
                           public_gan=  numeric(),
                           private_gan= numeric() )

for( corte in seq( from=2000, to=20000, by=100 ) )
{
  total    <- tb_prediccion2[ x <= corte,  sum( ganancia,    na.rm=TRUE ) ]
  public   <- tb_prediccion2[ x <= corte & fold==1, 2*sum( ganancia, na.rm=TRUE ) ]
  private  <- tb_prediccion2[ x <= corte & fold==2, 2*sum( ganancia, na.rm=TRUE ) ]
  
  tb_cortes2  <-  rbind( tb_cortes2,
                        list( PARAM$semilla_particion,
                              PARAM$semilla,
                              corte,
                              total,
                              public,
                              private ) )
}


xtope  <- 18000

plot( x= tb_prediccion[ x < xtope & fold==1, x ] ,
      y= tb_prediccion[ x < xtope & fold==1, gan_public ],
      col= "blue",
      main=  paste0( "Curvas Ganancia, particion: ", PARAM$semilla_particion ),
      xlab= "Envios",
      ylab= "Ganancia",
      ylim= c(1, 28000000 ),
      # col= "black",
      type= "l",
)

lines( x= tb_prediccion2[ x < xtope & fold==1, x ] ,
       y= tb_prediccion2[ x < xtope & fold==1, gan_public ],
       col= "red",
       pch= 15
)

legend("topleft",
       legend= c("Public 2", "Public 5"),
       col= c( "blue", "red"),
       lty= c(1,1,1),
       pch= c(20,15,15),
)


plot( x= tb_prediccion[ x < xtope & fold==2, x ] ,
      y= tb_prediccion[ x < xtope & fold==2, gan_private ],
      col= "blue",
      main=  paste0( "Curvas Ganancia, particion: ", PARAM$semilla_particion ),
      xlab= "Envios",
      ylab= "Ganancia",
      ylim= c(1, 280000000 ),
      # col= "black",
      type= "l",
)

lines( x= tb_prediccion2[ x < xtope & fold==2, x ] ,
       y= tb_prediccion2[ x < xtope & fold==2, gan_private ],
       col= "red",
       pch= 15
)

legend("topleft",
       legend= c("Public 2", "Public 5"),
       col= c( "blue", "red"),
       lty= c(1,1,1),
       pch= c(20,15,15),
)

# plot( x= tb_prediccion[ 1:xtope, x],
#       y= tb_prediccion[ 1:xtope, gan_acum],
#       main=  paste0( "Curvas Ganancia, particion: ", PARAM$semilla_particion ),
#       xlab= "Envios",
#       ylab= "Ganancia",
#       ylim= c(1, 28000000 ),
#       col= "black",
#       type= "l",
# )
# 
# lines( x= tb_prediccion[ x < xtope & fold==1, x ] ,
#        y= tb_prediccion[ x < xtope & fold==1, gan_public ],
#        col= "blue",
#        pch= 15 
# )
# 
# lines( x= tb_prediccion[ x < xtope & fold==2, x ] ,
#        y= tb_prediccion[ x < xtope & fold==2, gan_private ],
#        col= "red",
#        pch= 15 
# )
# 
# 
# legend("topleft", 
#        legend= c("todo", "Public", "Private"),
#        col= c( "black", "blue", "red"),
#        lty= c(1,1,1),
#        pch= c(20,15,15), 
# )