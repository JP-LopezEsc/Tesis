
library(tidyverse)
library(janitor)
library(lubridate)
library(zoo)
library(openxlsx)
Sys.setlocale(locale = "es_ES.UTF-8")


# Efectivo ------------------------------------------------------------------


datos_efectivo <- readxl::read_excel("datos/m0.xlsx", 
                                     range = "A18:B283") %>% 
  rename(fecha2 = 1, efectivo = 2) %>% 
  mutate(fecha2 = as.Date(fecha2), efectivo = efectivo/1000000) %>% 
  filter(fecha2 >= '2001-01-01', fecha2 <='2022-12-01')

datos_efectivo$fecha <- as.yearqtr(datos_efectivo$fecha2, "%Y-%m-%d")

datos_efectivo_last <- datos_efectivo %>% 
  group_by(fecha) %>% 
  filter(fecha2 == max(fecha2)) %>% 
  ungroup() %>% 
  dplyr::select(3,2)

write_rds(datos_efectivo_last, 'cache/variables/efectivo.rds')



# INPC mensual --------------------------------------------------------------

#Los datos están en inflacion mensual, se define funcion para calcular
#inflacion trimestral
calcula_inflacion_trim <- function(i_mensuales){
  i_trim <- 1
  for(i in 1:length(i_mensuales)){
    i_trim <- i_trim*(1+i_mensuales[i]/100)
  }
  return((i_trim-1)*100)
}


datos_inpc_trim <- readxl::read_excel('datos/inflacion_mensual.xls', range = 'A5:B269') %>% 
  rename('fecha' = 1, 'inpc' = 2) %>% 
  mutate(fecha = str_replace(fecha, '/','-')) %>%
  mutate(fecha = as.Date(paste0(fecha,'-01'))) %>%
  mutate(fecha2 = as.yearqtr(fecha, "%Y-%m-%d")) %>% 
  group_by(fecha2) %>% 
  mutate(calcula_inflacion_trim(inpc)) %>% 
  select('fecha' = 3, 'inpc' = 4) %>% 
  distinct()

write_rds(datos_inpc_trim, 'cache/variables/inpc_trim.rds')

# PIB -----------------------------------------------------------------------

#PIB en miles de millones de pesos

datos_pib <- readxl::read_excel("datos/tabulados_pibt/PIBTR_2.xlsx", 
                                      range = "A6:KP12") %>% 
  clean_names() %>% 
  rename(Concepto = 1) %>% 
  filter(Concepto == '__abB.1bP - Producto interno bruto') %>% 
  pivot_longer(names_to = 'trim', cols = c(2:301), values_to = 'pib') %>% 
  mutate(trim2 = case_when(
    str_detect(trim, 't1') ~ 'Q1',
    str_detect(trim, 't2') ~ 'Q2',
    str_detect(trim, 't3') ~ 'Q3',
    str_detect(trim, 't4') ~ 'Q4'
  )) %>% 
  drop_na(trim2) %>% 
  cbind(data.frame('anio'=rep(1980:2022,4) %>% sort())) %>% 
  mutate(fecha = paste0(anio,'-',trim2), pib = pib/1000) %>% 
  filter(fecha >= '2001-Q1', fecha <='2022-Q4') %>% 
  select(7,4)

datos_pib$fecha <- as.yearqtr(datos_pib$fecha, format = '%Y-Q%q')

write_rds(datos_pib, 'cache/variables/pib.rds')



# FIX -----------------------------------------------------------------------

datos_fix <- readxl::read_excel('datos/tipoCambio.xls', range = 'A7:D8409') %>% 
  mutate(fecha = as.Date(Fecha, '%d/%m/%Y')) %>% 
  mutate(trim = as.yearqtr(fecha, "%Y-%m-%d")) %>% 
  mutate(fix = as.numeric(`Determinación`)) %>% 
  select(5,6,7) %>% 
  drop_na()

datos_fix_last <- datos_fix %>% 
  group_by(trim) %>% 
  filter(fecha == max(fecha)) %>% 
  ungroup() %>% 
  select(fecha=2, 3) %>% 
  filter(fecha>='2001 Q1', fecha <= '2022 Q4')

write_rds(datos_fix_last, 'cache/variables/fix.rds')


# TIIE ----------------------------------------------------------------------

datos_tiie <- readxl::read_excel('datos/tiie.xlsx', range = 'A18:B4351') %>% 
  rename('fecha'=1, 'tiie'=2) %>% 
  mutate(fecha2 = as.yearqtr(fecha, "%Y-%m-%d")) %>% 
  filter(fecha2>='2001Q1', fecha2 <= '2022Q4')

datos_tiie_last <- datos_tiie %>% 
  group_by(fecha2) %>% 
  filter(fecha == max(fecha)) %>% 
  ungroup() %>% 
  select('fecha' = 3, 2)

write_rds(datos_tiie_last, 'cache/variables/tiie.rds')



# Archivo xlsx --------------------------------------------------------------

#Se crea un archivo xslx con las variables que al final fueron seleccionadas

datos_pib %>% 
  left_join(datos_inpc_trim) %>% 
  write.xlsx('datos/pib_inpc_clean.xlsx')

