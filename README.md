# Tesis

En este repositorio se encuentra el código empleado para obtener los resultados de los modelos empleados en la tesis de Juan Pablo López Escamilla.

El respositorio está organizado de la siguiente manera:

- En la carpeta 'datos' se encuentran los datos de las variables utilizadas en la tesis. Estos datos se encuentran en el formato de descarga, no están limpios.

- En la carpeta 'munge' se encuentra el código empleado para limpiar los datos. Los datos limpios se guardan en 'cache/variables'

-  En la carpeta 'src' está el código empleado para ajustar los DLM, calcular los pronósticos, las medidas de precisión y graficar. Sigue el siguiente orden:
    1. La carpeta 'src/funciones' contiene el archivo 'funciones_dlm.R'. Este archivo contiene las funciones necesarias para ajustar los DLM, las distribuciones filtradas y los pronósticos.
    2. En la carpeta 'src/modelos/historicos' se encuentra el archivo 'dlm_hist.R'. En este archivo se estiman los valores desconocidos y los valores iniciales del DLM utilizando datos de 2001 T1 a 2011 T4. Una vez calculados estos valores, se guardan en 'cache/output_modelo_historico'  y 'vecm_hist.R'.
    3. En la carpeta 'src/modelos/pronosticos' está el archivo 'dlm_prons.R' En este archivo se utilizan los valores estimados anteriormente para aplicar el DLM de 2012 T1 a 2022 T4. Se guarda el modelo obtenido como 'cache/modelos/modelo_dlm.rds'. En la carpeta también se encuentra el archivo 'dlm_interv_prons.R', donde se realiza la intervención y se posteriormente se guarda el modelo 'cache/modelos/modelo_dlm_interv.rds'.
    4.  En la carpeta 'src/salida_prons' están los archivos 'dlm_salidas.R' y 'dlm_interv_salidas.R'. En estos archivos se calculan los pronósticos pasados y sus intervalos, se calculan sus medidas de precisión y se guardan en 'cache/resultados/dlm' y en 'cache/resultados/dlm_interv' respectivamente. En la carpeta también se localiza el archivo 'suavizamiento.R'. Aquí se calculan las distribuciones filtradas del DLM y DLM intervenido y se guardan en  'cache/resultados/dlm' y 'cache/resultados/dlm_interv'. Para mayor comodidad, también se crearon archivos pdf con las medidas de precisión de los pronósticos de los modelos en la carpeta 'docs'.
    5.  La carpeta 'src/graficas' contiene código utilizado para graficar los resultados. Los pronósticos para 2023 y 2024 del DLM intervenido se calculan y grafican en 'src/graficas/graficas_prons_futuro.R'. Todas las gráficas se guardan en la carpeta 'graphs/modelos/dlm', y  'graphs/modelos/dlm_interv'.

- En el caso del VECM primero se definieron los lags y el número de cointegraciones utilizando datos de 2001 T1 a 2011 T4 en el archivo 'src/modelos/historicos/vecm_hist.R'. Con base en esto, en el archivo 'src/salida_prons/vecm_salidas.R' se reestiman los VECM en cada periodo, se calculan sus pronósticos y medidas de precisión (en la carpeta docs hay un pdf con estas medidas). Con esto, se grafican los pronósticos en el archivo 'src/graficas/graficas_vecm.R'. Las gráficas se guardan en 'graphs/modelos/vecm'.
