# Tesis

En este repositorio se encuentra el código empleado para obtener los resultados de los modelos empleados en la tesis de Juan Pablo López Escamilla.

El respositorio está organizado de la siguiente manera:

- En la carpeta 'datos' se encuentran los datos de las variables utilizadas en la tesis. Estos datos se encuentran en el formato de descarga, no están limpios.

- En la carpeta 'munge' se encuentra el código empleado para limpiar los datos. Los datos limpios se guardan en 'cache/variables'

-  En la carpeta 'src' está el código empleado para ajustar los DLM, calcular los pronósticos, las medidas de precisión y graficar. Sigue el siguiente orden:
    1. La carpeta 'src/funciones' contiene el archivo 'funciones_dlm.R'. Este archivo contiene las funciones necesarias para ajustar los DLM, las distribuciones filtradas y los pronósticos.
    2. En la carpeta 'src/modelos/historicos' se encuentra el archivo 'dlm_hist.R'. En este archivo se estiman los valores desconocidos y los valores iniciales del DLM utilizando datos de 2001 T1 a 2011 T4. Una vez calculados estos valores, se guardan en 'cache/output_modelo_historico'  y 'vecm_hist.R'.
    3. En la carpeta 'src/modelos/pronosticos' está el archivo 'dlm_prons.R' En este archivo se utilizan los valores estimados anteriormente para aplicar el DLM de 2012 T1 a 2022 T4. Se guarda el modelo obtenido como 'cache/modelos/modelo_dlm.rds'. En la carpeta también se encuentra el archivo 'dlm_interv_prons.R', donde se realiza la intervención y se posteriormente se guarda el modelo 'cache/modelos/modelo_dlm_interv.rds'.
    4.  
