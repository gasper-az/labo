# Data Mining en Economía y Finanzas - Competencia 02 - Experimento 07

## Índice

- [Detalles](#detalles)
- [Data Drifting y Concept Drifting](#data-drifting-y-concept-drifting)
- [Feature Engineering](#feature-engineering)
- [Hiperparámetros](#hiperparámetros---lightgbm)
- [Ejecución y resultados de script de LightGBM](#ejecución-y-resultados-de-script-de-lightgbm)
- [Análisis - Feature importance](#análisis---feature-importance)
  - [Gain](#feature-importance---gain)
  - [Cover](#feature-importance---cover)
  - [Frequency](#feature-importance---frequency)
  - [Conclusión](#feature-importance---conclusión)

## Detalles

En este experimento hacemos uso del algoritmo de [*LightGBM*][link-documentacion-lightgbm], aplicado al dataset *original* de la *competencia 02* ([link de descarga][link-dataset-competencia-02]).
También haremos uso de ciertas variables de *Feature Engineering*, que fueron creadas y utilizadas par la [competencia 01][link-kaggle-competencia-01].
Además, estaremos analizando y resolviendo problemas de *Data Drifting* en ciertas variables del dataset.
Luego realizamos una ejecución del script [lightgbm_binaria_BO.r][script-ligthgbm-bo] para hacer un tunning de los siguientes hiperparámetros:

- learning_rate
- feature_fraction
- min_data_in_leaf
- num_leaves
- envios

El motivo por el cual decidimos no agregar más hiperparámetros de LigthGBM se debe a que no obtuvimos buenos resultados con otros hiperparámetros (ver [Experimento 4][experimento-04-readme] y [Experimento 5][experimento-05-readme])

Luego, analizamos el resultado de dicha optimización de hiperparámetros para LightGBM (en nuestro caso, el archivo [HT7237.txt][salida-ligthgbm-bo]), en donde lo ordenamos por *ganancia* descendente. De ahí tomamos los valores de los hiperparámetros de nuestro interés, y los implementamos en el script [lightgbm_final.r][script-ligthgbm-ejecucion].
Una vez que ejecutemos este script con los valores de los hiperparámetros obtenidos, generamos una serie de archivos para entregar, en los definimos en función de varios puntos de corte.
Realizamos una entrega en [Kaggle][link-kaggle-competencia-02] por cada uno de estos archivos, y posteriormente analizamos el valor de la *ganancia* obtenido en el *public leaderboard*.

## Data Drifting y Concept Drifting

### Ranking básico

Variables en las que detectamos Data Drifting:

- mcomisiones
- mpasivos_margen
- mcuenta_corriente
- mcaja_ahorro
- mcaja_ahorro_dolares
- mcuentas_saldo
- mtarjeta_visa_consumo
- ccuenta_debitos_automaticos
- mcuenta_debitos_automaticos
- ccomisiones_otras
- mcomisiones_otras
- chomebanking_transacciones
- ccajas_otras
- mactivos_margen
- mrentabilidad
- mrentabilidad_annual
- Master_mfinanciacion_limite
- Master_Finiciomora
- Master_fultimo_cierre
- Visa_Finiciomora
- Visa_msaldopesos
- Visa_mconsumospesos
- Visa_madelantodolares
- Visa_fultimo_cierre
- Visa_mpagado
- Visa_mpagosdolares
- Visa_mconsumototal
- Visa_msaldototal

Aplicamos un ranking mediante la función *frank* de la siguiente forma:

```{r}
dataset[foto_mes==mes.analizar, (new.var.name) := (frankv(dataset[foto_mes==mes.analizar], cols = var, na.last = TRUE, ties.method = "modo") - 1) / (.N - 1)]
```

En donde *mes.analizar* corresponde al mes donde entrenamos nuestro dataset (*202103*) o el mes que queremos predecir (*202105*).
En donde *modo* es uno de los posibles valores que recibe el parámetro *ties.method* de la función *frank* para tratar aquellos casos de empates [link a la documentación][link-documentacion-frank].

Encontramos que este método de ranking con el modo *dense* (el cual permite que no haya grietas/espacios en el ranking) es útil para las siguientes variables:

- mpasivos_margen
- mcaja_ahorro_dolares
- mtarjeta_visa_consumo
- mcuenta_debitos_automaticos
- chomebanking_transacciones
- ccajas_otras
- Visa_mconsumospesos
- Visa_mconsumototal
- mcuentas_saldo
- Visa_msaldototal

Ahora bien, si utilizamos el mismo método de ranking, pero con el modo *random* para tratar los empates, tenemos buenos resultados con estas variables:

- mcaja_ahorro
- mcomisiones
- ccomisiones_otras
- mcomisiones_otras
- Visa_msaldopesos
- mrentabilidad
- mrentabilidad_annual
- mactivos_margen

Respecto al resto de los casos, no encontramos resultados favorables con este método de ranking:

- ccuenta_debitos_automaticos
- Master_mfinanciacion_limite
- Master_fultimo_cierre
- Visa_fultimo_cierre
- mcuenta_corriente
- Visa_madelantodolares
- Visa_mpagosdolares
- Master_Finiciomora
- Visa_Finiciomora
- Visa_mpagado

### Ranking por positivos, negativos, y ceros

Para poder arreglar el problem de data drifting en las variables restantes, decidimos aplicar un método de rankeo de la siguiente forma:

```{r}
dataset[foto_mes==mes.analizar, (new.var.name) := ifelse(var >= 0,
                                                  (ifelse(var > 0,
                                                          (frankv(dataset[foto_mes==mes.analizar], cols = var, na.last = TRUE, ties.method = "modo") - 1) / (.N - 1), ## mayores a cero
                                                          0)), # cero
                                                  -(frankv(dataset[foto_mes==mes.analizar], cols = var, na.last = TRUE, ties.method = "modo") - 1) / (.N - 1) ## menores a cero
                                                )
      ]
```

En donde *mes.analizar* corresponde al mes donde entrenamos nuestro dataset (*202103*) o el mes que queremos predecir (*202105*).
En donde *modo* es uno de los posibles valores que recibe el parámetro *ties.method* de la función *frank* para tratar aquellos casos de empates [link a la documentación][link-documentacion-frank].

Este método, implementado con el modo *dense* para tratar empates, es útil en esta variables:

- ccuenta_debitos_automaticos
- Master_mfinanciacion_limite
- Master_fultimo_cierre
- Visa_fultimo_cierre
- mcuenta_corriente
- Visa_madelantodolares
- Visa_mpagosdolares

Ahora bien, si implementamos el modo *last*, obtenemos resultados favorables con las variables *Master_Finiciomora* y *Visa_Finiciomora*. En cuanto a la variable *Visa_mpagado*, los mejores resultados los obtuvimos con el modo *first*.

### Conclusiones ranking

Decidimos entonces aplicar a las variables que detectamos y categorizamos como afectadas por Data Drifting ambos métodos de ranking.
Entonces, crearemos un set de variables nuevas, cuyo nombre será de la forma `r_nombre_original`. Finalmente, eliminaremos del dataset a las variables originales.

Algo a tener en cuenta es que, como algunas de estas variables eran utilizadas en el *Feature Engineering* original (el que implementamos en la competencia anterior), debemos entonces actualizar las variables creadas.
Para ello, reemplazaremos las variables afectadas por sus correspondientes rankeadas a la hora de hacer *Feature Engineering*.

> Los resultados del fix de Data Drifting se pueden ver graficados en [este archivo][salida-fix-data-drifting]

## Feature Engineering

```{ r}
dataset[, master_fe_suma_all := 
          r_Master_mfinanciacion_limite +
          Master_msaldototal + Master_msaldopesos + Master_msaldodolares +
          Master_mconsumospesos + Master_mconsumosdolares + Master_mlimitecompra +
          Master_madelantopesos + Master_madelantodolares + Master_mpagado +
          Master_mpagospesos + Master_mpagosdolares + Master_mconsumototal +
          Master_mpagominimo
]

dataset[, visa_fe_suma_all := Visa_mfinanciacion_limite +
          r_Visa_msaldototal +
          r_Visa_msaldopesos + Visa_msaldodolares + r_Visa_mconsumospesos +
          Visa_mconsumosdolares + Visa_mlimitecompra + Visa_madelantopesos +
          r_Visa_madelantodolares + r_Visa_mpagado + Visa_mpagospesos +
          r_Visa_mpagosdolares + r_Visa_mconsumototal + Visa_mpagominimo
]

dataset[, tarjetas_fe_suma_all := master_fe_suma_all + visa_fe_suma_all]

dataset[, pesos_fe_suma_menos_tarjetas := 
          r_mrentabilidad + r_mrentabilidad_annual + r_mactivos_margen +
          mcomisiones_mantenimiento + r_mpasivos_margen + r_mcuenta_corriente +
          r_mcaja_ahorro + r_mcaja_ahorro_dolares + r_mcomisiones + mcuenta_corriente_adicional + mcaja_ahorro_adicional +          
          r_mcuentas_saldo + mautoservicio + r_mtarjeta_visa_consumo +
          mtarjeta_master_consumo + mprestamos_personales + mprestamos_prendarios +
          mprestamos_hipotecarios + mplazo_fijo_dolares + mplazo_fijo_pesos +
          minversion1_pesos + minversion1_dolares + minversion2 + 
          mpayroll + mpayroll2 + r_mcuenta_debitos_automaticos +
          mttarjeta_master_debitos_automaticos + mpagodeservicios + mpagomiscuentas +
          mcajeros_propios_descuentos + mtarjeta_visa_descuentos + mtarjeta_master_descuentos +
          r_mcomisiones_otras + mforex_buy + mforex_sell +
          mtransferencias_recibidas + mtransferencias_emitidas + mextraccion_autoservicio +
          mcheques_depositados + mcheques_emitidos + mcheques_depositados_rechazados +
          mcheques_emitidos_rechazados + matm + matm_other
]

dataset[, pesos_fe_suma_all :=
          pesos_fe_suma_menos_tarjetas +
          tarjetas_fe_suma_all
]

dataset[, cociente_fe_01 := ctrx_quarter/r_mcuentas_saldo]
dataset[, cociente_fe_02 := ctrx_quarter/r_mcomisiones]
dataset[, cociente_fe_03 := r_mcuentas_saldo/r_mcomisiones]

```

## Hiperparámetros - LightGBM

Respecto al rango de búsqueda de los hiperparámetros, nos basamos en [esta documentación][link-documentacion-lightgbm-recomendacion-hp]:

| Hiperparámetro | Rango Min | Rango Max |
| - |   -   | - |
| *learning_rate* | 0.001 | 0.3 |
| *feature_fraction* | 0.001 | 1.0 |
| *min_data_in_leaf* | 0 | 8000 |
| *num_leaves* | 0 | 1024 |
| *envios* | 5000 | 15000 |

En este [link][link-documentacion-lightgbm-parametros] se puede encontrar más información sobre los hiperparámetros que analizamos.

Estos son los mejores resultados que se obtuvieron al realizar una *optimización bayesiana* para tunear los hiperparámetros a utilizar en LightGBM.
Para ello, utilizamos el script [lightgbm_binaria_BO.r][script-ligthgbm-bo], obteniendo como salida el archivo [HT7237.txt][salida-ligthgbm-bo].
El restultado que tomamos es el que se obtuvo en la iteración *70*, el cual arroja una ganancia de **27330000**.

| Hiperparámetro | valor |
| - |   -   |
| *learning_rate* | 0.0450579481474516 |
| *feature_fraction* | 0.347009781655412 |
| *min_data_in_leaf* | 5094 |
| *num_leaves* | 21 |
| *envios* | 7434 |

## Ejecución y resultados de script de LightGBM

> **Importante**: a la hora de querer replicar esto, tener en cuenta de setear el *working directory*, y, sobre todo, la semilla **763369**

Aplicamos los hiperparámetros obtenidos mediante la optimización bayesiana en el script [lightgbm_final.r][script-ligthgbm-ejecucion], generando los archivos de entraga, uno por cada punto de corte (*cantidad de envíos*) entre *5000* y *12000*, con saltos de a *500*.
Luego, hacemos una entrega de cada archivo en la competancia de [Kaggle][link-kaggle-competencia-02], obteniendo los siguientes resultados en el *public leaderboard*.

| Cantidad de envíos | Score en public leaderboard |
| - | - |
| 5000 | 17.71621 |
| 5500 | 18.35622 |
| 6000 | 19.42423 |
| 6500 | 19.89624 |
| 7000 | 20.14824 |
| 7500 | 20.06024 |
| 8000 | 20.19624 |
| 8500 | 19.53623 |
| 9000 | 19.40023 |
| 9500 | 19.33223 |
| 10000 | 19.03223 |
| 10500 | 19.20423 |
| 11000 | 18.72822 |
| 11500 | 18.03222 |
| 12000 | 17.44821 |

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
| pesos_fe_suma_menos_tarjetas  | 1ro                 | 3ro                 | 2do |
| r_mcuentas_saldo              | 5to                 | 2do                 | 1ro |
| ctrx_quarter                  | 2do                 | *Posterior a 10mo*  | *Posterior a 10mo* |
| mpayroll                      | 8vo                 | 1ro                 | *Posterior a 10mo* |
| r_mrentabilidad_annual        | 10mo                | 6to                 | 3ro |
| r_mcaja_ahorro                | *Posterior a 10mo*  | *Posterior a 10mo*  | 5to |
| r_mtarjeta_visa_consumo       | 9no                 | *Posterior a 10mo*  | *Posterior a 10mo*  |
| cociente_fe_02                | 3ro                 | *Posterior a 10mo*  | *Posterior a 10mo*  |
| cociente_fe_03                | *Posterior a 10mo*  | *Posterior a 10mo*  | 4to  |
| r_mpasivos_margen             | 7mo                 | *Posterior a 10mo*  | *Posterior a 10mo*  |

Si solamente nos quedamos con aquellos que aparezcan entre las *10* primeras posiciones de todas las medidas, obtenemos las siguientes variables importantes:

| Variable | Posición en Gain | Posición en Cover | Posición en Frequency |
| - | - | - | - |
| pesos_fe_suma_menos_tarjetas  | 1ro  | 4to | 7mo |
| r_mcuentas_saldo              | 5to  | 2do | 1ro |
| r_mrentabilidad_annual        | 10mo | 6to | 3ro |

Esto nos da un nuevo punto de partida para posibles futuros experimentos, en donde podamos crear nuevas variables de *Feature Engineering* basándonos en estas *3* variables importantes.

<!-- Links a scripts -->
[script-ligthgbm-bo]: ../../lightgbm_binaria_BO.r
[script-ligthgbm-ejecucion]: ../../lightgbm_final.r
[script-utilidades-analisis-feature-importance]: ../../Utilidades/utilidades_analisis_feature_importance.r

<!-- Links a resultados -->
[salida-ligthgbm-bo]: ./HT7237/HT7237.txt

<!-- Links a imágenes -->
[imagen-experimento-ganancias-public-leaderboard]: ./Otros%20archivos/experimento-ganancias-public-leaderboard.png
[imagen-experimento-feature-importance-gain]: ./Otros%20archivos/experimento-feature-importance-gain.png
[imagen-experimento-feature-importance-cover]: ./Otros%20archivos/experimento-feature-importance-cover.png
[imagen-experimento-feature-importance-frequency]: ./Otros%20archivos/experimento-feature-importance-frequency.png
[imagen-experimento-feature-importance-todas]: ./Otros%20archivos/experimento-feature-importance-todas.png

<!-- Links a archivos de salida -->
[salida-ka-importance]: ./KA7247/impo.txt
[salida-fix-data-drifting]: ./Otros%20archivos/fix_data_drifting_competencia_02.pdf

<!-- Links a otros experimentos -->
[experimento-04-readme]: ../Experimento%2004/readme.md
[experimento-05-readme]: ../Experimento%2005/readme.mds

<!-- Links externos -->
[link-dataset-competencia-02]: https://storage.googleapis.com/dmeyf2022/competencia2_2022.csv.gz
[link-kaggle-competencia-02]: https://www.kaggle.com/competitions/dm-eyf-2022-segunda
[link-kaggle-competencia-01]: https://www.kaggle.com/competitions/dm-eyf-2022-primera
[link-documentacion-lightgbm]: https://lightgbm.readthedocs.io/en/latest/index.html
[link-documentacion-lightgbm-parametros]: https://lightgbm.readthedocs.io/en/latest/Parameters.html
[link-documentacion-lightgbm-recomendacion-hp]: https://towardsdatascience.com/kagglers-guide-to-lightgbm-hyperparameter-tuning-with-optuna-in-2021-ed048d9838b5
[link-documentacion-frank]: https://www.rdocumentation.org/packages/data.table/versions/1.14.2/topics/frank
