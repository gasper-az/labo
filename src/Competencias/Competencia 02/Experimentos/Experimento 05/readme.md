# [CANCELADO]: debo enfocarme en Data y concept drifting

# Data Mining en Economía y Finanzas - Competencia 02 - Experimento 01

## Índice

- [Detalles](#detalles)
- [Feature Engineering](#feature-engineering)
- [Data Drifting y Concept Drifting](#data-drifting-y-concept-drifting)
- [Hiperparámetros](#hiperparámetros---lightgbm)
- [Ejecución y resultados de script de LightGBM](#ejecución-y-resultados-de-script-de-lightgbm)
- [Análisis - Feature importance](#análisis---feature-importance)
  - [Gain](#feature-importance---gain)'
  - [Cover](#feature-importance---cover)
  - [Frequency](#feature-importance---frequency)
  - [Conclusión](#feature-importance---conclusión)

## Detalles

En este experimento hacemos uso del algoritmo de [*LightGBM*][link-documentacion-lightgbm], aplicado al dataset *original* de la *competencia 02* ([link de descarga][link-dataset-competencia-02]).
TODO: FE y DD + CD
Luego realizamos una ejecución del script [lightgbm_binaria_BO.r][script-ligthgbm-bo] para hacer un tunning de los siguientes hiperparámetros:

- max_bin
- learning_rate
- num_iterations
- num_leaves
- min_data_in_leaf
- feature_fraction
- envios
- bagging_fraction
- bagging_freq
- lambda_l1
- lambda_l2
- min_gain_to_split
- max_depth

Luego, analizamos el resultado de dicha optimización de hiperparámetros para LightGBM (en nuestro caso, el archivo [HT7234.txt][salida-ligthgbm-bo]), en donde lo ordenamos por *ganancia* descendente. De ahí tomamos los valores de los hiperparámetros de nuestro interés, y los implementamos en el script [lightgbm_final.r][script-ligthgbm-ejecucion].
Una vez que ejecutemos este script con los valores de los hiperparámetros obtenidos, generamos una serie de archivos para entregar, en los definimos en función de varios puntos de corte.
Realizamos una entrega en [Kaggle][link-kaggle-competencia-02] por cada uno de estos archivos, y posteriormente analizamos el valor de la *ganancia* obtenido en el *public leaderboard*.

## Feature Engineering

Comenzamos aplicando el mismo Feature Engineering que se aplicó en el [Experimento 04][experimento-04], con la diferencia que para conformar las nuevas variables **NO** utilizaremos a aquellas variables afectadas por *Data Drifting* y *Concept Drifting*, obteniendo lo siguiente como resultado:

```{ r}
dataset[, master_fe_suma_all :=
        Master_msaldototal + Master_msaldopesos + Master_msaldodolares +
        Master_mconsumospesos + Master_mconsumosdolares + Master_mlimitecompra +
        Master_madelantopesos + Master_madelantodolares + Master_mpagado +
        Master_mpagospesos + Master_mpagosdolares + Master_mconsumototal +
        Master_mpagominimo
]

dataset[, visa_fe_suma_all :=
        Visa_mfinanciacion_limite + Visa_msaldototal + Visa_msaldodolares + 
        Visa_mconsumosdolares + Visa_mlimitecompra + Visa_madelantopesos +
        Visa_mpagospesos + Visa_mpagominimo
]

dataset[, tarjetas_fe_suma_all := master_fe_suma_all + visa_fe_suma_all]

dataset[, pesos_fe_suma_menos_tarjetas :=
          mrentabilidad + mrentabilidad_annual + mactivos_margen +
          mcuenta_corriente_adicional + mcaja_ahorro_adicional + mautoservicio + 
          mtarjeta_master_consumo + mprestamos_personales + mprestamos_prendarios +
          mprestamos_hipotecarios + mplazo_fijo_dolares + mplazo_fijo_pesos +
          minversion1_pesos + minversion1_dolares + minversion2 +
          mpayroll + mpayroll2 + mttarjeta_master_debitos_automaticos +
          mpagodeservicios + mpagomiscuentas + mcajeros_propios_descuentos +
          mtarjeta_visa_descuentos + mtarjeta_master_descuentos + mcomisiones_mantenimiento +
          mforex_buy + mforex_sell + mtransferencias_recibidas +
          mtransferencias_emitidas + mextraccion_autoservicio + mcheques_depositados +
          mcheques_emitidos + mcheques_depositados_rechazados + mcheques_emitidos_rechazados +
          matm + matm_other
]
```

La única excepción se hará con la variable *cociente_fe_01*, ya que la misma fue detectada como importante:

```{ r }
dataset[, cociente_fe_01 := ctrx_quarter/ranked_mcuentas_saldo]
```

Además, en el [Experimento 04][experimento-04] llegamos a obtener a las siguientes variables como importantes:

- pesos_fe_suma_menos_tarjetas
- mrentabilidad_annual
- ranked_mcaja_ahorro
- Master_Fvencimiento
- cociente_fe_01
- ranked_mcuentas_saldo

En base a estas, conformamos una nueva serie de variables de Feature Engineering:

```{ r }
dataset[, cociente_fe_02 := pesos_fe_suma_menos_tarjetas/mrentabilidad_annual]
dataset[, cociente_fe_03 := pesos_fe_suma_menos_tarjetas/ranked_mcaja_ahorro]
dataset[, cociente_fe_04 := pesos_fe_suma_menos_tarjetas/Master_Fvencimiento]
dataset[, cociente_fe_05 := pesos_fe_suma_menos_tarjetas/cociente_fe_01]
dataset[, cociente_fe_06 := pesos_fe_suma_menos_tarjetas/ranked_mcuentas_saldo]

dataset[, cociente_fe_07 := mrentabilidad_annual/ranked_mcaja_ahorro]
dataset[, cociente_fe_08 := mrentabilidad_annual/Master_Fvencimiento]
dataset[, cociente_fe_09 := mrentabilidad_annual/cociente_fe_01]
dataset[, cociente_fe_10 := mrentabilidad_annual/ranked_mcuentas_saldo]

dataset[, cociente_fe_11 := ranked_mcaja_ahorro/Master_Fvencimiento]
dataset[, cociente_fe_12 := ranked_mcaja_ahorro/cociente_fe_01]
dataset[, cociente_fe_13 := ranked_mcaja_ahorro/ranked_mcuentas_saldo]

dataset[, cociente_fe_14 := Master_Fvencimiento/cociente_fe_01]
dataset[, cociente_fe_15 := Master_Fvencimiento/ranked_mcuentas_saldo]

dataset[, cociente_fe_16 := cociente_fe_01/ranked_mcuentas_saldo]
```

Como las dos variables más importantes son *pesos_fe_suma_menos_tarjetas* y *mrentabilidad_annual*, ya que son las únicas que aparecen entre las *10* variables más importantes en las tres métricas de *Gain*, *frequency*, y *cover*, creamos un par más de variables nuevas en función de ellas.

```{ r }
dataset[, suma_fe_01 := pesos_fe_suma_menos_tarjetas + mrentabilidad_annual]
dataset[, vabs_fe_01 := abs(pesos_fe_suma_menos_tarjetas - mrentabilidad_annual)]
dataset[, prod_fe_01 := pesos_fe_suma_menos_tarjetas * mrentabilidad_annual]
```

## Data Drifting y Concept Drifting

TODO

## Hiperparámetros - LightGBM

En este caso nos proponemos optimizar los hiperparámetros de LightGBM con la idea de reducir el *overfitting*, para lo cual nos basamos en [esta documentación][link-documentacion-lightgbm-overfitting],
la cual propone enfocarnos en los siguientes hiperparámetros:

- num_leaves
- min_data_in_leaf
- bagging_fraction
- bagging_freq
- feature_fraction
- lambda_l1
- lambda_l2
- min_gain_to_split
- max_depth

Respecto al rango de búsqueda de los hiperparámetros, nos basamos en [esta documentación][link-documentacion-lightgbm-recomendacion-hp]:

| Hiperparámetro | Rango Min | Rango Max |
| - |   -   | - |
| *learning_rate* | 0.005 | 0.3 |
| *num_leaves* | 16 | 1024 |
| *min_data_in_leaf* | 20 | 8000 |
| *feature_fraction* | 0.001 | 1.0 |
| *envios* | 5000 | 15000 |
| *bagging_fraction* | 0.0001 | 0.9999 |
| *bagging_freq* | 1 | 999 |
| *lambda_l1* | 0.0001 | 100 |
| *lambda_l2* | 0.0001 | 100 |
| *min_gain_to_split* | 0.0001 | 15 |
| *max_depth* | 6 | 20 |

Respecto al hiperparámetro de *max_depth*, en este intento decidimos buscar entre 6 y 20, pero en futuras pruebas acotaremos este rango de búsqueda en función de los resultados que obtengamos.
Además, respecto a experimentos anteriores, decidimos modificar el rango de búsqueda del hiperparámetro *feature_fraction* entre *0.001* y *1.0*.
En este [link][link-documentacion-lightgbm-parametros] se puede encontrar más información sobre los hiperparámetros que analizamos.

Estos son los mejores resultados que se obtuvieron al realizar una *optimización bayesiana* para tunear los hiperparámetros a utilizar en LightGBM.
Para ello, utilizamos el script [lightgbm_binaria_BO.r][script-ligthgbm-bo], obteniendo como salida el archivo [HT7234.txt][salida-ligthgbm-bo].
El restultado que tomamos es el que se obtuvo en la iteración *134*, el cual arroja una ganancia de **27270000**.

| Hiperparámetro | valor |
| - |   -   |
| *max_bin* | 31 |
| *learning_rate* | 0.00952482742610648 |
| *num_iterations* | 1298 |
| *num_leaves* | 743 |
| *min_data_in_leaf* | 450 |
| *feature_fraction* | 0.605783962510153 |
| *semilla* (*training strategy*) | 763369 |
| *semilla (hyperparameter tunning)* | 763381 |
| *envios* | 8685 |
| *bagging_fraction* | 0.987784990036988 |
| *bagging_freq* | 99 |
| *lambda_l1* | 0.943100967973046 |
| *lambda_l2* | 74.0466609734954 |
| *min_gain_to_split* | 0.0698103369382356 |
| *max_depth* | 10 |

## Ejecución y resultados de script de LightGBM

> **Importante**: a la hora de querer replicar esto, tener en cuenta de setear el *working directory*, y, sobre todo, la semilla **763369**

Aplicamos los hiperparámetros obtenidos mediante la optimización bayesiana en el script [lightgbm_final.r][script-ligthgbm-ejecucion], generando los archivos de entraga, uno por cada punto de corte (*cantidad de envíos*) entre *5000* y *12000*, con saltos de a *500*.
Luego, hacemos una entrega de cada archivo en la competancia de [Kaggle][link-kaggle-competencia-02], obteniendo los siguientes resultados en el *public leaderboard*.

| Cantidad de envíos | Score en public leaderboard |
| - | - |
| 5000 | 0 |
| 5500 | 0 |
| 6000 | 0 |
| 6500 | 17.30821 |
| 7000 | 17.58021 |
| 7500 | 17.74821 |
| 8000 | 18.08022 |
| 8500 | 17.90021 |
| 9000 | 0 |
| 9500 | 0 |
| 10000 | 0 |
| 10500 | 0 |
| 11000 | 0 |
| 11500 | 0 |
| 12000 | 0 |

---

![Experimento ganancias public leaderboard][imagen-experimento-ganancias-public-leaderboard]

---

## Análisis - Feature importance

Finalmente, procedemos a analizar el *feature importance* que nos devuelve el script [lightgbm_final.r][script-ligthgbm-ejecucion]. Para ello, hacemos uso del script [utilidades_analisis_feature_importance.r][script-utilidades-analisis-feature-importance].
Aquí procesamos el archivo [impo.txt][salida-ka-importance], el cual tiene la importancia en el modelo generado de cada variable de nuestro dataset, en función de tres medidas:

- Gain
- Cover
- Frequency

Procedemos a graficar las *10* variables más importances, en función de las tres medidas mencionadas anteriormente.

### Feature importance - Gain

---

![Feature importance gain][imagen-experimento-feature-importance-gain]

---

### Feature importance - Cover

---

![Feature importance cover][imagen-experimento-feature-importance-cover]

---

### Feature importance - Frequency

---

![Feature importance frequency][imagen-experimento-feature-importance-frequency]

---

### Feature importance - Conclusión

Para analizar cuales son las mejores, procedemos a realizar el cálculo del producto de las tres medidas *Gain*, *Cover* y *Frequency*, obteniendo el siguiente gráfico como resultado:

---

![Feature importance todas][imagen-experimento-feature-importance-todas]

---

TODO
Según el gráfico anterior, podemos decir que las siguientes variables son las más importantes para el modelo generado:

| Variable | Posición en Gain | Posición en Cover | Posición en Frequency |
| - | - | - | - |
| pesos_fe_suma_menos_tarjetas  | 1ro                 | 2do                 | 2do |
| mrentabilidad_annual          | 4to                 | 1ro                 | 1ro |
| ctrx_quarter                  | 2do                 | *Posterior a 10mo*  | *Posterior a 10mo* |
| ranked_mcaja_ahorro           | 10mo                | *Posterior a 10mo*  | 7mo |
| Master_Fvencimiento           | *Posterior a 10mo*  | 3ro                 | 4to |
| ranked_mcuenta_corriente      | 8vo                 | *Posterior a 10mo*  | *Posterior a 10mo* |
| cociente_fe_01                | *Posterior a 10mo*  | 9no                 | 5to |
| cliente_edad                  | *Posterior a 10mo*  | *Posterior a 10mo*  | 3ro |
| mactivos_margen               | *Posterior a 10mo*  | *Posterior a 10mo*  | *Posterior a 10mo*  |
| ranked_mcuentas_saldo         | 7mo                 | *Posterior a 10mo*  | 10mo |

Si solamente nos quedamos con aquellos que aparezcan entre las *10* primeras posiciones de todas las medidas, obtenemos las siguientes variables importantes:

| Variable | Posición en Gain | Posición en Cover | Posición en Frequency |
| - | - | - | - |
| pesos_fe_suma_menos_tarjetas  | 1ro | 4to | 7mo |
| mrentabilidad_annual          | 2do | 2do | 2do |

Si ahora también consideramos a aquellos que aparezcan entre las *10* primeras posiciones de al menos dos medidas, obtenemos las siguientes variables importantes:
| - | - | - | - |
| pesos_fe_suma_menos_tarjetas  | 1ro   | 4to | 7mo   |
| mrentabilidad_annual          | 2do   | 2do | 2do   |
| ranked_mcaja_ahorro           | 10mo  | n/a | 7mo   |
| Master_Fvencimiento           | n/a   | 3ro | 4to   |
| cociente_fe_01                | n/a   | 9no | 5to   |
| ranked_mcuentas_saldo         | 7mo   | n/a | 10mo  |

TODO:
Ahora bien, comparémoslo con el impo_78.txt

<!-- Links a scripts -->
[script-ligthgbm-bo]: ../../lightgbm_binaria_BO.r
[script-ligthgbm-ejecucion]: ../../lightgbm_final.r
[script-utilidades-analisis-feature-importance]: ../../Utilidades/utilidades_analisis_feature_importance.r

<!-- Links a resultados -->
[salida-ligthgbm-bo]: ./HT7234/HT7234.txt

<!-- Links a imágenes -->
[imagen-experimento-ganancias-public-leaderboard]: ./Otros%20archivos/experimento-ganancias-public-leaderboard.png
[imagen-experimento-feature-importance-gain]: ./Otros%20archivos/experimento-feature-importance-gain.png
[imagen-experimento-feature-importance-cover]: ./Otros%20archivos/experimento-feature-importance-cover.png
[imagen-experimento-feature-importance-frequency]: ./Otros%20archivos/experimento-feature-importance-frequency.png
[imagen-experimento-feature-importance-todas]: ./Otros%20archivos/experimento-feature-importance-todas.png

<!-- Links a archivos de salida -->
[salida-ka-importance]: ./KA7244/impo.txt

<!-- Links a otros experimentos -->
[experimento-04]: ../Experimento%2004/readme.md

<!-- Links externos -->
[link-dataset-competencia-02]: https://storage.googleapis.com/dmeyf2022/competencia2_2022.csv.gz
[link-kaggle-competencia-02]: https://www.kaggle.com/competitions/dm-eyf-2022-segunda
[link-documentacion-lightgbm]: https://lightgbm.readthedocs.io/en/latest/index.html
[link-documentacion-lightgbm-parametros]: https://lightgbm.readthedocs.io/en/latest/Parameters.html
[link-documentacion-lightgbm-overfitting]: http://devdoc.net/bigdata/LightGBM-doc-2.2.2/Parameters-Tuning.html#deal-with-over-fitting
[link-documentacion-lightgbm-recomendacion-hp]: https://towardsdatascience.com/kagglers-guide-to-lightgbm-hyperparameter-tuning-with-optuna-in-2021-ed048d9838b5
