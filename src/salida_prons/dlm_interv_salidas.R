################################################################################
## Crea data frames de los pronosticos de 1 a 8 pasos del DLM interv y los guarda.
##
################################################################################

library(tidyverse)
source('src/funciones/funciones_dlm.R')

#Datos efectivo
datos_efectivo <- read_rds('cache/variables/efectivo.rds') %>% 
  filter(fecha > 2011.75) %>% 
  mutate(efectivo = log(efectivo))


# Se lee el dlm intervenido obtenido en 'scr/modelos/pronosticos/dlm_interv_prons.R'
modelo_dlm_interv <- read_rds('cache/modelos/modelo_dlm_interv.rds')

# El VECM hace sus propios pronósticos del PIB y de la inflación. Para que sea justa
# la comparación, utilizaré pronósticos de encuestas que hace Banxico para cada
# periodo para obtener los pronósticos del dlm. Estos datos están disponibles en 
# https://www.banxico.org.mx/publicaciones-y-prensa/encuestas-sobre-las-expectativas-de-los-especialis/encuestas-expectativas-del-se.html
# Esos pronósticos de Banxico se guardaron en el archivo 'cache/variables/prons_banxico.rds'
prons_F_banxico <- read_rds('cache/variables/prons_banxico.rds')[1:44]

k <- 3

# Se define la intervención a realizar
list_interv <- list("t_int" = list(34),
                    "at_int" = list(modelo_dlm_interv$at[[34]]),
                    "Rt_int" = list(modelo_dlm_interv$Rt[[34]]))

prons_dlm_interv <- pronosticos_k_pasos(prons_F = prons_F_banxico, 
                                        k, 
                                        modelo = modelo_dlm_interv, 
                                        lista_interv = list_interv,
                                        per_anticip_interv = 1)


df_dlm_interv <- data.frame("fecha" = datos_efectivo %>% dplyr::select(fecha), 
                            "y_real" = datos_efectivo$efectivo, 
                            "y_pronostico" = modelo_dlm_interv$ft %>% unlist(), 
                            "CI_inf" = modelo_dlm_interv$CI_inf %>% unlist(),
                            "CI_sup" = modelo_dlm_interv$CI_sup %>% unlist()) %>% 
  mutate(fecha = as.numeric(fecha))

df_medidas <- data.frame('k' = rep(NA,8), 'error_medio' = rep(NA,8), 'MSE' = rep(NA,8), 
                         'MAE' = rep(NA,8), 'porcent_MAPE' = rep(NA,8), 'theil_U' = rep(NA,8))


for(k in 1:8){
  
  prons_dlm_interv <- pronosticos_k_pasos(prons_F = prons_F_banxico, 
                                          k = k, 
                                          modelo = modelo_dlm_interv, 
                                          lista_interv = list_interv,
                                          per_anticip_interv = 1)
  
  df_prons_dlm_interv <- data.frame("fecha" = datos_efectivo$fecha[k:nrow(datos_efectivo)], 
                                    "y_real" = datos_efectivo$efectivo[k:nrow(datos_efectivo)], 
                                    "y_pronostico" = prons_dlm_interv$ft_k %>% unlist(), 
                                    "CI_inf" = prons_dlm_interv$CI_inf %>% unlist(),
                                    "CI_sup" = prons_dlm_interv$CI_sup %>% unlist()) %>% 
    mutate(fecha = as.numeric(fecha))
  
  
  residsk_dlm_interv <- df_prons_dlm_interv %>% 
    mutate(resids = y_real - y_pronostico) %>% 
    dplyr::select(resids)
  
  df_medidas$k[k] <- k
  df_medidas$error_medio[k] <- mean(residsk_dlm_interv$resids)
  df_medidas$MSE[k] <- mean(residsk_dlm_interv$resids^2)
  df_medidas$MAE[k] <- mean(abs(residsk_dlm_interv$resids))
  df_medidas$porcent_MAPE[k] <- mean(abs(residsk_dlm_interv$resids / df_prons_dlm_interv$y_real)*100)
  fpe <- (df_prons_dlm_interv$y_pronostico[(k+1):nrow(df_prons_dlm_interv)] - df_prons_dlm_interv$y_real[1:(nrow(df_prons_dlm_interv)-k)]) / df_prons_dlm_interv$y_real[1:(nrow(df_prons_dlm_interv)-k)] 
  ape <- (df_prons_dlm_interv$y_real[(k+1):nrow(df_prons_dlm_interv)] - df_prons_dlm_interv$y_real[1:(nrow(df_prons_dlm_interv)-k)]) / df_prons_dlm_interv$y_real[1:(nrow(df_prons_dlm_interv)-k)] 
  df_medidas$theil_U[k] <- sqrt(sum((fpe - ape)^2)/sum(ape^2))
}



