# Instrucciones

1. Generar la predicción del **modelo 01**
    1. Seguir instrucciones del archivo [readme.md](./01%20-%20Modelo%20LightGBM//readme.md)
1. Generar la predicción del **modelo 02**
    1. Seguir instrucciones del archivo [readme.md](./02%20-%20Modelo%20LightGBM/readme.md)
1. Generar la predicción del **modelo 03**
    1. Seguir instrucciones del archivo [readme.md](./03%20-%20Modelo%20LightGBM/readme.md)
1. Asegurarse que todas las predicciones deseadas estén en la carpeta [ZZAVG](./ZZAVG/)
    1. **Nota**: para evitar estos pasos, descomprima el archivo [ZZAVG.zip](./ZZAVG.zip) que se encuentra en esta carpeta.
1. Ejecutar el script [999_ensemble_avg_semillerios.r](./999_ensemble_avg_semillerios.r)
    1. Este script se puede ejecutar localmente. Para ello, actualice la variable `PARAM$base_dir` (línea 19) con el directorio actual
    1. Este script generará la carpeta `envios` en este mismo directorio
        1. **Nota**: para evitar estos pasos, descomprima el archivo [envios.zip](./envios.zip) que se encuentra en esta carpeta.
1. Ir a la carpeta [envios](./envios/), y subir el archivo [ensamble_semillerio_modelos_1_2_3_10500](./envios/ensamble_semillerio_modelos_1_2_3_10500.csv) como entrega en Kaggle
    1. **Nota**: este archivo corresponde al *punto de corte* **10500**
