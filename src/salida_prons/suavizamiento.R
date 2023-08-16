library(tidyverse)

source('src/funciones/funciones_dlm.R')

datos_efectivo <- read_rds('cache/variables/efectivo.rds') %>% 
  filter(fecha > 2011.75) 



# DLM -----------------------------------------------------------------------


modelo_dlm <- read_rds('cache/modelos/modelo_dlm.rds')

modelo_dlm_suav <- suavizamiento_V_desc(modelo_dlm)

df_dlm_suav <- data.frame("fecha" = datos_efectivo %>% dplyr::select(fecha), 
                                 "y_real" = datos_efectivo$efectivo, 
                                 "y_suav" = exp(modelo_dlm_suav$ft_k_filt %>% unlist()), 
                                 "CI_inf" = exp(modelo_dlm_suav$CI_inf %>% unlist()),
                                 "CI_sup" = exp(modelo_dlm_suav$CI_sup %>% unlist())) %>% 
  mutate(fecha = as.numeric(fecha))

write_rds(df_dlm_suav,'cache/resultados/dlm/dlm_suavizamiento.rds')





# DLM Interv ----------------------------------------------------------------

modelo_dlm_interv <- read_rds('cache/modelos/modelo_dlm_interv.rds')

modelo_dlm_interv_suav <- suavizamiento_V_desc(modelo_dlm_interv)

df_dlm_interv_suav <- data.frame("fecha" = datos_efectivo %>% dplyr::select(fecha), 
                                 "y_real" = datos_efectivo$efectivo, 
                                 "y_suav" = exp(modelo_dlm_interv_suav$ft_k_filt %>% unlist()), 
                                 "CI_inf" = exp(modelo_dlm_interv_suav$CI_inf %>% unlist()),
                                 "CI_sup" = exp(modelo_dlm_interv_suav$CI_sup %>% unlist())) %>% 
  mutate(fecha = as.numeric(fecha))

write_rds(df_dlm_interv_suav,'cache/resultados/dlm_interv/dlm_interv_suavizamiento.rds')

