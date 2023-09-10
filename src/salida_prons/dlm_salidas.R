################################################################################
## Crea data frames de los pronosticos de 1 a 8 pasos del DLM y los guarda.
##
################################################################################

library(tidyverse)
library(gridExtra)
source('src/funciones/funciones_dlm.R')

#Datos efectivo
datos_efectivo <- read_rds('cache/variables/efectivo.rds') %>% 
  filter(fecha > 2011.75) 


# Se lee el dlm obtenido en 'scr/modelos/pronosticos/dlm_prons.R'
modelo_dlm <- read_rds('cache/modelos/modelo_dlm.rds')

# El VECM hace sus propios pronósticos del PIB y de la inflación. Para que sea justa
# la comparación, utilizaré pronósticos de encuestas que hace Banxico para cada
# periodo para obtener los pronósticos del dlm. Estos datos están disponibles en 
# https://www.banxico.org.mx/publicaciones-y-prensa/encuestas-sobre-las-expectativas-de-los-especialis/encuestas-expectativas-del-se.html
# Esos pronósticos de Banxico se guardaron en el archivo 'cache/variables/prons_banxico.rds'
prons_F_banxico <- read_rds('cache/variables/prons_banxico.rds')[1:44]


df_medidas <- data.frame('k' = rep(NA,8), 'error_medio' = rep(NA,8), 'MSE' = rep(NA,8), 
                         'MAE' = rep(NA,8), 'porcent_MAPE' = rep(NA,8), 'theil_U' = rep(NA,8),
                         'porcent_en_interv' = rep(NA,8))


for(k in 1:8){
  
  prons_dlm <- pronosticos_k_pasos(prons_F = prons_F_banxico, 
                                    k = k, 
                                    modelo = modelo_dlm, 
                                    lista_interv = list(),
                                    per_anticip_interv = 1)
  
  df_prons_dlm <- data.frame("fecha" = datos_efectivo$fecha[k:nrow(datos_efectivo)], 
                                    "y_real" = datos_efectivo$efectivo[k:nrow(datos_efectivo)], 
                                    "y_pronostico" = exp(prons_dlm$ft_k %>% unlist()), 
                                    "CI_inf" = exp(prons_dlm$CI_inf %>% unlist()),
                                    "CI_sup" = exp(prons_dlm$CI_sup %>% unlist())) %>% 
    mutate(fecha = as.numeric(fecha))
  
  
  residsk_dlm <- df_prons_dlm %>% 
    mutate(resids = y_real - y_pronostico) %>% 
    dplyr::select(resids)
  
  df_medidas$k[k] <- k
  df_medidas$error_medio[k] <- mean(residsk_dlm$resids)
  df_medidas$MSE[k] <- mean(residsk_dlm$resids^2)
  df_medidas$MAE[k] <- mean(abs(residsk_dlm$resids))
  df_medidas$porcent_MAPE[k] <- mean(abs(residsk_dlm$resids / df_prons_dlm$y_real)*100)
  
  #Para calcular U de Theil se requiere conocer el efectivo en 2011 Q4
  #Se necesita comparar el primer pronostico contra el valor histórico anterior,
  #que es 2011 Q4
  datos_efectivo_U <-  read_rds('cache/variables/efectivo.rds') %>% 
    filter(fecha >= 2011.75)
  
  df_theul_U <-df_prons_dlm %>% 
    dplyr::select(fecha, y_pronostico) %>% 
    cbind('y_t_real' = datos_efectivo_U$efectivo[1:nrow(df_prons_dlm)],
          'y_t_mas_k_real' = datos_efectivo_U$efectivo[(k+1):nrow(datos_efectivo_U)])
  
  fpe <- (df_theul_U$y_pronostico - df_theul_U$y_t_real) / df_theul_U$y_t_real
  ape <- (df_theul_U$y_t_mas_k_real - df_theul_U$y_t_real) / df_theul_U$y_t_real
  df_medidas$theil_U[k] <- sqrt(sum((fpe - ape)^2)/sum(ape^2))
  
  
  porcent_interv <- df_prons_dlm %>% 
    mutate(dentro_intervalo = ifelse(CI_inf <= y_real & y_real <= CI_sup,1,0)) %>% 
    pull(dentro_intervalo) %>% 
    mean()*100
  
  df_medidas$porcent_en_interv[k] <- porcent_interv
  
  write_rds(df_prons_dlm,
            paste0('cache/resultados/dlm/dlm_prons_', k, '_pasos.rds'))
}


df_medidas
write_rds(df_medidas, 
          'cache/resultados/dlm/dlm_medidas.rds')


# Para mayor comodidad, guardar pdf en carpeta docs
pdf("docs/dlm_medidas.pdf", height=11, width=10)
grid.table(df_medidas  %>% 
             mutate_if(is.numeric, ~sprintf("%.2f",.)))
dev.off()

