# Data Mining en Economía y Finanzas - Competencia 02 - Experimento 01

## Índice

- [Detalles](#detalles)
- [Hiperparámetros](#hiperparámetros---lightgbm)
- [Ejecución y resultados de script de LightGBM](#ejecución-y-resultados-de-script-de-lightgbm)
- [Análisis - Feature importance](#análisis---feature-importance)
  - [Gain](#feature-importance---gain)'
  - [Cover](#feature-importance---cover)
  - [Frequency](#feature-importance---frequency)
  - [Conclusión](#feature-importance---conclusión)

## Detalles

Este experimento corresponde al *punto 4* de la "tarea" que consiste en aplicar los algoritmos de *XGBoost* y [*LightGBM*][link-documentacion-lightgbm].
Aquí, hacemos uso del algoritmo de LightGBM, aplicado al dataset *original* de la *competencia 02* ([link de descarga][link-dataset-competencia-02]).
Lo que realizamos fue, primero, realizar una ejecución del script [lightgbm_binaria_BO.r][script-ligthgbm-bo] para hacer un tunning de los siguientes hiperparámetros:

- max_bin
- learning_rate
- num_iterations
- num_leaves
- min_data_in_leaf
- feature_fraction
- envios

Luego, analizamos el resultado de dicha optimización de hiperparámetros para LightGBM (en nuestro caso, el archivo [HT7231.txt][salida-ligthgbm-bo]), en donde lo
ordenamos por *ganancia* descendente. De ahí tomamos los valores de los hiperparámetros de nuestro interés, y los implementamos en el script [lightgbm_final.r][script-ligthgbm-ejecucion].
Una vez que ejecutemos este script con los valores de los hiperparámetros obtenidos, generamos una serie de archivos para entregar, en los definimos en función de varios puntos de corte.
Realizamos una entrega en [Kaggle][link-kaggle-competencia-02] por cada uno de estos archivos, y posteriormente analizamos el valor de la *ganancia* obtenido en el *public leaderboard*.

## Hiperparámetros - LightGBM

Estos son los mejores resultados que se obtuvieron al realizar una *optimización bayesiana* para tunear los hiperparámetros a utilizar en LightGBM.
Para ello, utilizamos el script [lightgbm_binaria_BO.r][script-ligthgbm-bo], obteniendo como salida el archivo [HT7231.txt][salida-ligthgbm-bo].
El restultado que tomamos es el que se obtuvo en la iteración *24*, el cual arroja una ganancia de **26960000**.

| Hiperparámetro | valor |
| - |   -   |
| *max_bin* | 31 |
| *learning_rate* | 0.005201496 |
| *num_iterations* | 1444 |
| *num_leaves* | 1024 |
| *min_data_in_leaf* | 1968 |
| *feature_fraction* | 0.750240992 |
| *semilla* (*training strategy*) | 763369 |
| *semilla (hyperparameter tunning)* | 763381 |
| *envios* | 8282 |

## Ejecución y resultados de script de LightGBM

> **Importante**: a la hora de querer replicar esto, tener en cuenta de setear el *working directory*, y, sobre todo, la semilla **763369**

Aplicamos los hiperparámetros obtenidos mediante la optimización bayesiana en el script [lightgbm_final.r][script-ligthgbm-ejecucion], generando los archivos de entraga, uno por cada punto de corte (*cantidad de envíos*) entre *5000* y *12000*, con saltos de a *500*.
Luego, hacemos una entrega de cada archivo en la competancia de [Kaggle][link-kaggle-competencia-02], obteniendo los siguientes resultados en el *public leaderboard*.

| Cantidad de envíos | Score en public leaderboard |
| - | - |
| 5000 | 17.93621 |
| 5500 | 18.10022 |
| 6000 | 18.45622 |
| 6500 | 18.42422 |
| 7000 | 18.61622 |
| 7500 | 18.76422 |
| 8000 | 18.86823 |
| 8500 | 19.20023 |
| 9000 | 19.02023 |
| 9500 | 19.15223 |
| 10000 | 18.71622 |
| 10500 | 18.00821 |
| 11000 | 17.67221 |
| 11500 | 17.63221 |
| 12000 | 17.25221 |

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
| ctrx_quarter | 1ro | 4to | 7mo |
| mcuentas_saldo | 2do | 2do | 2do |
| mpayroll | 3ro | 1ro | *Posterior a 10mo* |
| mrentabilidad_annual | 7mo | 3ro | 1mo |
| mcaja_ahorro | 4to | *Posterior a 10mo* | 4to |
| mpasivos_margen | 5to | *Posterior a 10mo* | 10mo |
| mprestamos_personales | 6to | 6to | *Posterior a 10mo* |
| mtarjeta_visa_consumo | 9no | *Posterior a 10mo* | *Posterior a 10mo* |
| cliente_edad | *Posterior a 10mo* | *Posterior a 10mo* | 3ro |
| mactivos_margen | *Posterior a 10mo* | *Posterior a 10mo* | 8vo |

Si solamente nos quedamos con aquellos que aparezcan entre las *10* primeras posiciones de todas las medidas, obtenemos las siguientes variables importantes:

| Variable | Posición en Gain | Posición en Cover | Posición en Frequency |
| - | - | - | - |
| ctrx_quarter | 1ro | 4to | 7mo |
| mcuentas_saldo | 2do | 2do | 2do |
| mrentabilidad_annual | 7mo | 3ro | 1mo |

<!-- Links a scripts -->
[script-ligthgbm-bo]: ../../lightgbm_binaria_BO.r
[script-ligthgbm-ejecucion]: ../../lightgbm_final.r
[script-utilidades-analisis-feature-importance]: ../../Utilidades/utilidades_analisis_feature_importance.r

<!-- Links a resultados -->
[salida-ligthgbm-bo]: ./HT7231/HT7231.txt

<!-- Links a imágenes -->
[imagen-experimento-ganancias-public-leaderboard]: ./Otros%20archivos/experimento-01-ganancias-public-leaderboard.png
[imagen-experimento-feature-importance-gain]: ./Otros%20archivos/experimento-01-feature-importance-gain.png
[imagen-experimento-feature-importance-cover]: ./Otros%20archivos/experimento-01-feature-importance-cover.png
[imagen-experimento-feature-importance-frequency]: ./Otros%20archivos/experimento-01-feature-importance-frequency.png
[imagen-experimento-feature-importance-todas]: ./Otros%20archivos/experimento-01-feature-importance-todas.png

<!-- Links a archivos de salida -->
[salida-ka-importance]: ./KA7241/impo.txt

<!-- Links externos -->
[link-dataset-competencia-02]: https://storage.googleapis.com/dmeyf2022/competencia2_2022.csv.gz
[link-kaggle-competencia-02]: https://www.kaggle.com/competitions/dm-eyf-2022-segunda
[link-documentacion-lightgbm]: https://lightgbm.readthedocs.io/en/latest/index.html
