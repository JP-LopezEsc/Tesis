
library(tidyverse)
library(janitor)
library(lubridate)
library(zoo)
Sys.setlocale(locale = "es_ES.UTF-8")


# Efectivo ------------------------------------------------------------------


datos_efectivo_trim <- readxl::read_excel("datos/m0.xlsx", 
                                     range = "A18:B283") %>% 
  rename(fecha = 1, efectivo = 2) %>% 
  mutate(fecha = as.Date(fecha), efectivo = efectivo/1000000) %>% 
  filter(fecha >= '2001-01-01', fecha <='2022-12-01')

datos_efectivo_trim$fecha <- as.yearqtr(datos_efectivo_trim$fecha, "%Y-%m-%d")

datos_efectivo_trim <- datos_efectivo_trim %>% 
  group_by(fecha) %>% 
  summarise('efectivo'=sum(efectivo))

write_rds(datos_efectivo_trim, 'cache/variables/efectivo_trim.rds')

# Actividad -----------------------------------------------------------------

# 
# datos_actividad <- readxl::read_excel("datos/actividad economica.xlsx", 
#                                       range = "A6:NZ23") %>% 
#   clean_names() %>% 
#   rename(Concepto = 1) %>% 
#   filter(Concepto == 'Total') %>% 
#   pivot_longer(names_to = 'mes', cols = c(2:390), values_to = 'actividad') %>% 
#   mutate(mes2 = case_when(
#     str_detect(mes, 'enero') ~ '01-01',
#     str_detect(mes, 'febrero') ~ '02-01',
#     str_detect(mes, 'marzo') ~ '03-01',
#     str_detect(mes, 'abril') ~ '04-01',
#     str_detect(mes, 'mayo') ~ '05-01',
#     str_detect(mes, 'junio') ~ '06-01',
#     str_detect(mes, 'julio') ~ '07-01',
#     str_detect(mes, 'agosto') ~ '08-01',
#     str_detect(mes, 'septiembre') ~ '09-01',
#     str_detect(mes, 'octubre') ~ '10-01',
#     str_detect(mes, 'noviembre') ~ '11-01',
#     str_detect(mes, 'diciembre') ~ '12-01',
#   )) %>% 
#   drop_na(mes2) %>% 
#   cbind(data.frame('anio'=rep(1993:2022,12) %>% sort())) %>% 
#   mutate(fecha = paste0(anio,'-',mes2)) %>% 
#   mutate(fecha = as.Date(fecha)) %>% 
#   filter(fecha >= '2010-01-01', fecha <='2022-12-01') %>% 
#   select(1,6,3)
# 
# write_rds(datos_actividad, 'datos/variables/actividad.rds')



# Cetes 182d ----------------------------------------------------------------
# 
# ##Tomamos la primera decha de cada mes
# datos_cetes_182d <- readxl::read_excel('datos/cetes 182d.xlsx', range = 'A18:B809') %>% 
#   rename('fecha'=1, 'cetes_182d'=2) %>% 
#   mutate(fecha = as.Date(fecha), 
#          mes_anio = paste0(month(fecha), '_', year(fecha))) %>% 
#   group_by(mes_anio) %>% 
#   filter(fecha == min(fecha)) %>% 
#   ungroup() %>% 
#   select(-3) %>% 
#   filter(fecha >= '2010-01-01', fecha <='2022-12-30')
# 
# write_rds(datos_cetes_182d, 'datos/variables/cetes_182d.rds')  
# 
# datos_cetes_182d_trim <- readxl::read_excel('datos/cetes 182d.xlsx', range = 'A18:B809') %>% 
#   rename('fecha'=1, 'cetes_182d'=2) %>% 
#   mutate(fecha2 = as.yearqtr(fecha, "%Y-%m-%d")) %>% 
#   group_by(fecha2) %>% 
#   filter(fecha == min(fecha)) %>% 
#   ungroup() %>% 
#   filter(fecha2>='2010Q1', fecha2 <= '2022Q4') %>% 
#   select('fecha' = 3, 2)
# 
# write_rds(datos_cetes_182d_trim, 'datos/variables/cetes_182d_trim.rds')
# 
# # INPC ----------------------------------------------------------------------
# 
# datos_inpc <- readxl::read_excel('datos/inpc.xls', range = 'A5:B642') %>% 
#   rename('fecha' = 1, 'inpc' = 2) %>% 
#   mutate(fecha = str_replace(fecha, '/','-')) %>% 
#   mutate(fecha = as.Date(paste0(fecha,'-01'))) %>% 
#   filter(fecha >= '2010-01-01', fecha <='2022-12-01')
# 
# write_rds(datos_inpc, 'datos/variables/inpc.rds')
# 
datos_inpc_trim  <- readxl::read_excel('datos/inpc.xls', range = 'A5:B642') %>%
  rename('fecha' = 1, 'inpc' = 2) %>%
  mutate(fecha = str_replace(fecha, '/','-')) %>%
  mutate(fecha = as.Date(paste0(fecha,'-01'))) %>%
  mutate(fecha2 = as.yearqtr(fecha, "%Y-%m-%d")) %>%
  group_by(fecha2) %>%
  filter(fecha == min(fecha)) %>%
  ungroup() %>%
  filter(fecha2>='2001Q1', fecha2 <= '2022Q4') %>%
  select('fecha' = 3, 2)

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
  drop_na() %>% 
  group_by(trim) %>% 
  filter(fecha == min(fecha)) %>% 
  select(fecha=2, 3) %>% 
  filter(fecha>='2001 Q1', fecha <= '2022 Q4')

write_rds(datos_fix, 'cache/variables/fix_trim.rds')



# Cetes 28d ----------------------------------------------------------------
# 
# ##Tomamos la primera decha de cada mes
# datos_cetes_28d <- readxl::read_excel('datos/cetes 28d.xlsx', range = 'A18:B2352') %>% 
#   rename('fecha'=1, 'cetes_28d'=2) %>% 
#   mutate(fecha = as.Date(fecha), 
#          mes_anio = paste0(month(fecha), '_', year(fecha))) %>% 
#   group_by(mes_anio) %>% 
#   filter(fecha == min(fecha)) %>% 
#   ungroup() %>% 
#   select(-3) %>% 
#   filter(fecha >= '2001-01-01', fecha <='2022-12-30') %>% 
#   mutate(cetes_28d = as.numeric(cetes_28d))
# 
# write_rds(datos_cetes_28d, 'cache/variables/cetes_28d.rds')  

datos_cetes_28d_trim <- readxl::read_excel('datos/cetes 28d.xlsx', range = 'A18:B2352') %>% 
  rename('fecha'=1, 'cetes_28d'=2) %>% 
  mutate(fecha2 = as.yearqtr(fecha, "%Y-%m-%d")) %>% 
  group_by(fecha2) %>% 
  filter(fecha == min(fecha)) %>% 
  ungroup() %>% 
  filter(fecha2>='2001Q1', fecha2 <= '2022Q4') %>% 
  select('fecha' = 3, 2) %>% 
  mutate(cetes_28d = as.numeric(cetes_28d))

write_rds(datos_cetes_28d_trim, 'cache/variables/cetes_28d_trim.rds')

