# Instrucciones

1. Ejecutar los scripts en el siguiente orden (preferentemente en Google Cloud):
    1. `906_reparar_dataset.r`
    1. `914_corregir_drifting.r`
    1. `925_FE_historia.r`
    1. `932_training_strategy_under.r`
    1. `942_HT_lightgbm_under.r`
    1. `992_ZZ_lightgbm_under.r`
    1. `995_ZZ_lightgbm_semillero.R`
    1. `996_ZZ_lightgbm_semillero_prom.r`
1. Guardar la salida del script `995_ZZ_lightgbm_semillero.R` (preferentemente con el nombre `promedio.semillas.modelo3.csv`) en la carpeta [ZZAVG](../ZZAVG)
