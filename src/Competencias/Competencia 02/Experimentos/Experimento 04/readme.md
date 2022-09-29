# Data Mining en Economía y Finanzas - Competencia 02 - Experimento 04

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

TODO

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
| *feature_fraction* | 0.2 | 1.0 |
| *envios* | 5000 | 15000 |
| *bagging_fraction* | 0.0001 | 0.9999 |
| *bagging_freq* | 1 | 999 |
| *lambda_l1* | 0.0001 | 100 |
| *lambda_l2* | 0.0001 | 100 |
| *min_gain_to_split* | 0.0001 | 15 |
| *max_depth* | 1 | 30 |

Respecto al hiperparámetro de *max_depth*, en este intento decidimos buscar entre 1 y 30, pero en futuras pruebas acotaremos este rango de búsqueda en función de los resultados que obtengamos.
En este [link][link-documentacion-lightgbm-parametros] se puede encontrar más información sobre los hiperparámetros que analizamos.

Estos son los mejores resultados que se obtuvieron al realizar una *optimización bayesiana* para tunear los hiperparámetros a utilizar en LightGBM.
Para ello, utilizamos el script [lightgbm_binaria_BO.r][script-ligthgbm-bo], obteniendo como salida el archivo [HT7234.txt][salida-ligthgbm-bo].
El restultado que tomamos es el que se obtuvo en la iteración *78*, el cual arroja una ganancia de **27510000**.

| Hiperparámetro | valor |
| - |   -   |
| *max_bin* | 31 |
| *learning_rate* | 0.0276269972244999 |
| *num_iterations* | 506 |
| *num_leaves* | 387 |
| *min_data_in_leaf* | 2196 |
| *feature_fraction* | 0.551258816057135 |
| *semilla* (*training strategy*) | 763369 |
| *semilla (hyperparameter tunning)* | 763381 |
| *envios* | 9487 |
| *bagging_fraction* | 0.940588588893522 |
| *bagging_freq* | 413 |
| *lambda_l1* | 2.37704587212364 |
| *lambda_l2* | 50.9789242993526 |
| *min_gain_to_split* | 0.395834823316337 |
| *max_depth* | 4 |

## Ejecución y resultados de script de LightGBM

> **Importante**: a la hora de querer replicar esto, tener en cuenta de setear el *working directory*, y, sobre todo, la semilla **763369**

Aplicamos los hiperparámetros obtenidos mediante la optimización bayesiana en el script [lightgbm_final.r][script-ligthgbm-ejecucion], generando los archivos de entraga, uno por cada punto de corte (*cantidad de envíos*) entre *5000* y *12000*, con saltos de a *500*.
Luego, hacemos una entrega de cada archivo en la competancia de [Kaggle][link-kaggle-competencia-02], obteniendo los siguientes resultados en el *public leaderboard*.

| Cantidad de envíos | Score en public leaderboard |
| - | - |
| 5000 | 16.92820 |
| 5500 | 17.76421 |
| 6000 | 18.06422 |
| 6500 | 18.41222 |
| 7000 | 18.78022 |
| 7500 | 18.14022 |
| 8000 | 18.14822 |
| 8500 | 17.76421 |
| 9000 | 17.27621 |
| 9500 | 17.52421 |
| 10000 | 17.67621 |
| 10500 | 17.22421 |
| 11000 | 16.70420 |
| 11500 | 16.72020 |
| 12000 | 16.70820 |

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

<!-- Links externos -->
[link-dataset-competencia-02]: https://storage.googleapis.com/dmeyf2022/competencia2_2022.csv.gz
[link-kaggle-competencia-02]: https://www.kaggle.com/competitions/dm-eyf-2022-segunda
[link-documentacion-lightgbm]: https://lightgbm.readthedocs.io/en/latest/index.html
[link-documentacion-lightgbm-parametros]: https://lightgbm.readthedocs.io/en/latest/Parameters.html
[link-documentacion-lightgbm-overfitting]: http://devdoc.net/bigdata/LightGBM-doc-2.2.2/Parameters-Tuning.html#deal-with-over-fitting
[link-documentacion-lightgbm-recomendacion-hp]: https://towardsdatascience.com/kagglers-guide-to-lightgbm-hyperparameter-tuning-with-optuna-in-2021-ed048d9838b5
