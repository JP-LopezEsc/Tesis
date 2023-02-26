
library(tidyverse)
library(janitor)
library(lubridate)



# Efectivo ------------------------------------------------------------------


datos_efectivo <- readxl::read_excel("datos/billetes_monedas_publico.xlsx", 
                                     range = "A18:B283") %>% 
  rename(fecha = 1, efectivo = 2) %>% 
  mutate(fecha = as.Date(fecha), efectivo = efectivo/1000000) %>% 
  filter(fecha >= '2010-01-01', fecha <='2022-11-01')

write_rds(datos_efectivo, 'datos/variables/efectivo.rds')



# Actividad -----------------------------------------------------------------


datos_actividad <- readxl::read_excel("datos/actividad economica.xlsx", 
                                      range = "A6:NZ23") %>% 
  clean_names() %>% 
  rename(Concepto = 1) %>% 
  filter(Concepto == 'Total') %>% 
  pivot_longer(names_to = 'mes', cols = c(2:390), values_to = 'actividad') %>% 
  mutate(mes2 = case_when(
    str_detect(mes, 'enero') ~ '01-01',
    str_detect(mes, 'febrero') ~ '02-01',
    str_detect(mes, 'marzo') ~ '03-01',
    str_detect(mes, 'abril') ~ '04-01',
    str_detect(mes, 'mayo') ~ '05-01',
    str_detect(mes, 'junio') ~ '06-01',
    str_detect(mes, 'julio') ~ '07-01',
    str_detect(mes, 'agosto') ~ '08-01',
    str_detect(mes, 'septiembre') ~ '09-01',
    str_detect(mes, 'octubre') ~ '10-01',
    str_detect(mes, 'noviembre') ~ '11-01',
    str_detect(mes, 'diciembre') ~ '12-01',
  )) %>% 
  drop_na(mes2) %>% 
  cbind(data.frame('anio'=rep(1993:2022,12) %>% sort())) %>% 
  mutate(fecha = paste0(anio,'-',mes2)) %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  filter(fecha >= '2010-01-01', fecha <='2022-11-01') %>% 
  select(1,6,3)

write_rds(datos_actividad, 'datos/variables/actividad.rds')



# Cetes 182d ----------------------------------------------------------------

##Tomamos la primera decha de cada mes
datos_cetes_182d <- readxl::read_excel('datos/cetes 182d.xlsx', range = 'A18:B809') %>% 
  rename('fecha'=1, 'cetes_182d'=2) %>% 
  mutate(fecha = as.Date(fecha), 
         mes_anio = paste0(month(fecha), '_', year(fecha))) %>% 
  group_by(mes_anio) %>% 
  filter(fecha == min(fecha)) %>% 
  ungroup() %>% 
  select(-3) %>% 
  filter(fecha >= '2010-01-01', fecha <='2022-11-30')

write_rds(datos_cetes_182d, 'datos/variables/cetes_182d.rds')  
