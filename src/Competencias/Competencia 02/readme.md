# Data Mining en Economía y Finanzas - Competencia 02 - Experimento 07

## Índice

- [Detalles](#detalles)
- [Data Drifting y Concept Drifting](#data-drifting-y-concept-drifting)
- [Feature Engineering](#feature-engineering)
- [Hiperparámetros](#hiperparámetros---lightgbm)
- [Ejecución y resultados de script de LightGBM](#ejecución-y-resultados-de-script-de-lightgbm)
  - [Eligiendo un punto de corte como entrega para Kaggle](#eligiendo-un-punto-de-corte-como-entrega-para-kaggle)
- [Resultados private leaderboard](#resultados-private-leaderboard)
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
Realizamos una entrega en [Kaggle][link-kaggle-competencia-02] por cada uno de estos archivos, y posteriormente analizamos el valor de la *ganancia* obtenido en el *public leaderboard*, para finalmente elegir uno de estos archivos como entrega final.

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
> *Nota*: el archivo anterior lo generamos con [este script][script-utilidades-grafico-fix-data-drifting]

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

> **NOTA**: solamente analizamos los siguientes hiperparámetros, ya que en los [Experimento 4][experimento-04-readme] y [Experimento 5][experimento-05-readme] no obtuvimos buenos resultados utilizando otros hiperparámetros.

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

### Eligiendo un punto de corte como entrega para Kaggle

> **TL;DR;**: Elijo **8500** como entrega de la [competancia kaggle 02][link-kaggle-competencia-02].

Ahora bien, tenemos que elegir a uno de estos puntos graficados como entrega final de la [competancia 02][link-kaggle-competencia-02]. Para ello, analizamos el pdf que se nos entregó en la [clase-08][link-clase-08-pdf-hackeo-kaggle].

Nuestro gráfico se caracteriza por lo siguiente: tenemos una *meseta*, en nuestro caso, entre los puntos *7000* y *8000*, a la cual sigue una caída bruzca en la ganancia del *public leaderboard* (punto de corte *8500*), y de ahí en más la ganancia sigue disminuyendo, exceptuando una pequeña subida de ganancia en el punto *10500*.

En el [documento pdf][link-clase-08-pdf-hackeo-kaggle] que analizamos, notamos ciertos casos que nos llamaron la atención, ya que se caracterizan por tener una especie de *meseta* en los valores del public, que luego decae. Algunos de ellos se encuentran en las páginas *39*, *57*, *72*, y *84*. Si bien, presentan una caída luego de una *meseta* en los valores del public, los valores del *private* tiene mejor ganancia que incluso los puntos anteriores (y, en algunos casos, posteriores) a la caída de ganancia del public. Esto nos da un pie para seleccionar nuestra entrega, es decir, debemos elegir aquellos puntos donde la ganancia del public *cae* luego de tener cierta *tendencia estable*.

> **Conclusión**: Por este motivo, elijo el punto de corte **8500** como entrega de la [competancia kaggle 02][link-kaggle-competencia-02].

## Resultados Private Leaderboard

| Cantidad de envíos | Score en public leaderboard |
| - | - |
| 5000 | 18.60377 |
| 5500 | 19.80375 |
| 6000 | 19.77575 |
| 6500 | 20.34375 |
| 7000 | 20.01175 |
| 7500 | 20.01975 |
| 8000 | 20.44375 |
| 8500 | 20.06375 |
| 9000 | 20.27975 |
| 9500 | 19.94775 |
| 10000 | 19.52776 |
| 10500 | 19.91575 |
| 11000 | 19.83175 |
| 11500 | 19.80775 |
| 12000 | 19.19176 |

---

![Experimento ganancias private leaderboard][imagen-experimento-ganancias-private-leaderboard]

---

**Resultados**
El punto de corte **8500** elegido como entrega obtuvo una ganancia de *20.06375* en el private leaderboad, la cual es mayor que la que obtuvo en el public (*19.53623*). Igualmente, no es el mejor resultado en el private, ya que el punto de corte de 9000 obtuvo una ganancia de *20.27975* en el private (*19.40023* en public); el de 6500 obtuvo *20.34375* (*19.89624* en public), y el de 8000 obtuvo **20.44375** (*20.19624* en el public), siendo este último el que mayor valor obtuvo en el private.

| Cantidad de envíos | Score en public leaderboard | Score Private Leaderboard | Mejora el Private respecto al Public? |
| - | - | - | - |
| **8000**  | **20.19624** | **20.44375** | **Si** |
| 6500  | 19.89624 | 20.34375 | **Si** |
| 9000  | 19.40023 | 20.27975 | **Si** |
| 8500  | 19.53623 | 20.06375 | **Si** |
| 7500  | 20.06024 | 20.01975 | No |
| 7000  | 20.14824 | 20.01175 | No |
| 9500  | 19.33223 | 19.94775 | **Si** |
| 10500 | 19.20423 | 19.91575 | **Si** |
| 11000 | 18.72822 | 19.83175 | **Si** |
| 11500 | 18.03222 | 19.80775 | **Si** |
| 5500  | 18.35622 | 19.80375 | **Si** |
| 6000  | 19.42423 | 19.77575 | **Si** |
| 10000 | 19.03223 | 19.52776 | **Si** |
| 12000 | 17.44821 | 19.19176 | **Si** |
| 5000  | 17.71621 | 18.60377 | **Si** |

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
[script-ligthgbm-bo]: ./lightgbm_binaria_BO.r
[script-ligthgbm-ejecucion]: ./lightgbm_final.r
[script-utilidades-grafico-fix-data-drifting]: ./Utilidades//utilidades_graficar_densidades_data_drifting.r
[script-utilidades-analisis-feature-importance]: ./Utilidades/utilidades_analisis_feature_importance.r

<!-- Links a resultados -->
[salida-ligthgbm-bo]: ./Experimentos/Experimento%2007/HT7237/HT7237.txt

<!-- Links a imágenes -->
[imagen-experimento-ganancias-public-leaderboard]: ./Experimentos/Experimento%2007/Otros%20archivos/experimento-ganancias-public-leaderboard.png
[imagen-experimento-ganancias-private-leaderboard]: ./Experimentos/Experimento%2007/Otros%20archivos/experimento-ganancias-private-leaderboard.png
[imagen-experimento-feature-importance-gain]: ./Experimentos/Experimento%2007/Otros%20archivos/experimento-feature-importance-gain.png
[imagen-experimento-feature-importance-cover]: ./Experimentos/Experimento%2007/Otros%20archivos/experimento-feature-importance-cover.png
[imagen-experimento-feature-importance-frequency]: ./Experimentos/Experimento%2007/Otros%20archivos/experimento-feature-importance-frequency.png
[imagen-experimento-feature-importance-todas]: ./Experimentos/Experimento%2007/Otros%20archivos/experimento-feature-importance-todas.png

<!-- Links a archivos de salida -->
[salida-ka-importance]: ./Experimentos/Experimento%2007/KA7247/impo.txt
[salida-fix-data-drifting]: ./Experimentos/Experimento%2007/Otros%20archivos/fix_data_drifting_competencia_02.pdf

<!-- Links a otros experimentos -->
[experimento-04-readme]: ./Experimentos/Experimento%2004/readme.md
[experimento-05-readme]: ./Experimentos/Experimento%2005/readme.md

<!-- Links externos -->
[link-dataset-competencia-02]: https://storage.googleapis.com/dmeyf2022/competencia2_2022.csv.gz
[link-kaggle-competencia-02]: https://www.kaggle.com/competitions/dm-eyf-2022-segunda
[link-kaggle-competencia-01]: https://www.kaggle.com/competitions/dm-eyf-2022-primera
[link-documentacion-lightgbm]: https://lightgbm.readthedocs.io/en/latest/index.html
[link-documentacion-lightgbm-parametros]: https://lightgbm.readthedocs.io/en/latest/Parameters.html
[link-documentacion-lightgbm-recomendacion-hp]: https://towardsdatascience.com/kagglers-guide-to-lightgbm-hyperparameter-tuning-with-optuna-in-2021-ed048d9838b5
[link-documentacion-frank]: https://www.rdocumentation.org/packages/data.table/versions/1.14.2/topics/frank
[link-clase-08-pdf-hackeo-kaggle]: https://storage.googleapis.com/dmeyf2022/KaggleHack.pdf
