# Data Mining en Economía y Finanzas - Competencia 02 - Experimento 04

## Índice

- [Detalles](#detalles)
- [Data Drifting y Concept Drifting](#data-drifting-y-concept-drifting)
- [Feature Engineering](#feature-engineering)
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
TODO: explicar (y linkear) experimentos fallidos de HP BO de LightGBM (4 y 5)
Luego realizamos una ejecución del script [lightgbm_binaria_BO.r][script-ligthgbm-bo] para hacer un tunning de los siguientes hiperparámetros:

- learning_rate
- feature_fraction
- min_data_in_leaf
- num_leaves
- envios

Luego, analizamos el resultado de dicha optimización de hiperparámetros para LightGBM (en nuestro caso, el archivo [HT7234.txt][salida-ligthgbm-bo]), en donde lo ordenamos por *ganancia* descendente. De ahí tomamos los valores de los hiperparámetros de nuestro interés, y los implementamos en el script [lightgbm_final.r][script-ligthgbm-ejecucion].
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
dataset[, (new.var.name) := (frankv(dataset, cols = var, na.last = TRUE, ties.method = "dense") - 1) / (.N - 1)]
```

Encontramos que este método de ranking es útil para las siguientes variables:

- mpasivos_margen
- mcuenta_corriente
- mcaja_ahorro_dolares
- mtarjeta_visa_consumo
- mcuenta_debitos_automaticos
- chomebanking_transacciones
- ccajas_otras
- Visa_mconsumospesos
- Visa_madelantodolares
- Visa_mpagosdolares
- Visa_mconsumototal

Pero el mismo método de ranking NO es útil en estos casos:

- mcomisiones
- mcaja_ahorro
- mcuentas_saldo
- ccuenta_debitos_automaticos
- ccomisiones_otras
- mactivos_margen
- mcomisiones_otras
- mrentabilidad
- mrentabilidad_annual
- Master_mfinanciacion_limite
- Master_Finiciomora
- Master_fultimo_cierre
- Visa_Finiciomora
- Visa_msaldopesos
- Visa_fultimo_cierre
- Visa_mpagado
- Visa_msaldototal

### Ranking por positivos, negativos, y ceros

Ahora procedemos a realizar un ranking similar al anterior, pero aplicandolo de forma separada a valores positivos, negativos, y ceros de las variables mencionadas:

```{r}
dataset[, (new.var.name) := ifelse(var >= 0,
                                      (ifelse(var > 0,
                                              (frankv(dataset, cols = var, na.last = TRUE, ties.method = "dense") - 1) / (.N - 1), ## mayores a cero
                                              0)), # cero
                                      -(frankv(dataset, cols = var, na.last = TRUE, ties.method = "dense") - 1) / (.N - 1) ## menores a cero
                                     )
```

### Conclusiones ranking

Decidimos entonces aplicar a las variables que detectamos y categorizamos como afectadas por Data Drifting ambos métodos de ranking.
Entonces, crearemos un set de variables nuevas, cuyo nombre será de la forma `r_nombre_original`. Finalmente, eliminaremos del dataset a las variables originales.

Algo a tener en cuenta es que, como algunas de estas variables eran utilizadas en el *Feature Engineering* original (el que implementamos en la competencia anterior), debemos entonces actualizar las variables creadas.
Para ello, reemplazaremos las variables afectadas por sus correspondientes rankeadas a la hora de hacer *Feature Engineering*.

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
Para ello, utilizamos el script [lightgbm_binaria_BO.r][script-ligthgbm-bo], obteniendo como salida el archivo [HT7234.txt][salida-ligthgbm-bo].
El restultado que tomamos es el que se obtuvo en la iteración *70*, el cual arroja una ganancia de **27330000**.

| Hiperparámetro | valor |
| - |   -   |
| *learning_rate* | 0.00320371648958052 |
| *feature_fraction* | 0.429891409638461 |
| *min_data_in_leaf* | 1985 |
| *num_leaves* | 723 |
| *envios* | 7775 |

## Ejecución y resultados de script de LightGBM

> **Importante**: a la hora de querer replicar esto, tener en cuenta de setear el *working directory*, y, sobre todo, la semilla **763369**

Aplicamos los hiperparámetros obtenidos mediante la optimización bayesiana en el script [lightgbm_final.r][script-ligthgbm-ejecucion], generando los archivos de entraga, uno por cada punto de corte (*cantidad de envíos*) entre *5000* y *12000*, con saltos de a *500*.
Luego, hacemos una entrega de cada archivo en la competancia de [Kaggle][link-kaggle-competencia-02], obteniendo los siguientes resultados en el *public leaderboard*.

| Cantidad de envíos | Score en public leaderboard |
| - | - |
| 5000 | 14.36817 |
| 5500 | 15.62419 |
| 6000 | 16.77220 |
| 6500 | 16.93620 |
| 7000 | 17.16020 |
| 7500 | 17.28421 |
| 8000 | 17.05220 |
| 8500 | 16.24819 |
| 9000 | 16.39620 |
| 9500 | 16.91220 |
| 10000 | 16.63620 |
| 10500 | 15.95619 |
| 11000 | 15.39618 |
| 11500 | 14.84418 |
| 12000 | 14.30417 |

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

| Variable | Posición en Gain | Posición en Cover | Posición en Frequency |
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
[link-documentacion-lightgbm-recomendacion-hp]: https://towardsdatascience.com/kagglers-guide-to-lightgbm-hyperparameter-tuning-with-optuna-in-2021-ed048d9838b5
