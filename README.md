# Tesis

En este repositorio se encuentra el código empleado para obtener los resultados de los modelos empleados en la tesis de Juan Pablo López Escamilla.

El respositorio está organizado de la siguiente manera:

- En la carpeta 'datos' se encuentran los datos de las variables utilizadas en la tesis. Estos datos se encuentran en el formato de descarga, no están limpios.

- En la carpeta 'munge' se encuentra el código empleado para limpiar los datos. Los datos limpios se guardan en 'cache/variables'

-  En la carpeta 'src' está el código empleado para ajustar los modelos, calcular los pronósticos, las medidas de precisión y graficar. Sigue el siguiente orden:
  1. La carpeta 'src/funciones' contiene el archivo 'funciones_dlm.R'. Este archivo contiene las funciones necesarias para ajustar los DLM, las distribuciones filtradas y los pronósticos. 
