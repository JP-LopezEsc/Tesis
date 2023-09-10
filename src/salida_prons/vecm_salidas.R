################################################################################
## Crea data frames de los pronosticos de 1 a 8 pasos del VECM y los guarda.
##
################################################################################

library(tidyverse)
library(urca)
library(tseries)
library(vars)
library(tsDyn) 
library(gridExtra)
Sys.setlocale(locale = "es_ES.UTF-8")


# Efectivo ----------------------------------------------------------------------

datos_efectivo <- read_rds('cache/variables/efectivo.rds') %>% 
  mutate(efectivo = log(efectivo))


# PIB ----------------------------------------------------------------------

datos_pib <- read_rds('cache/variables/pib.rds') %>% 
  mutate(pib = log(pib))


# INPC ----------------------------------------------------------------------

datos_inpc <- read_rds('cache/variables/inpc_trim.rds') 



# Con los datos hasta 2011 Q4 se obtuvo que se tienen 1 lags y r=1 cointegraciones en el VECM, 
# se reestimaran los vecm en cada periodo, pero manteniendo estos parametros.

datos_estacionalidad <- datos_efectivo %>% 
  mutate(Q1 = ifelse(fecha-floor(fecha)==0, 0.75, -0.25)) %>% 
  mutate(Q2 = ifelse(fecha-floor(fecha)==0.25, 0.75, -0.25)) %>% 
  mutate(Q3 = ifelse(fecha-floor(fecha)==0.75, 0.75, -0.25)) %>% 
  dplyr::select(Q1:Q3) %>% 
  as.data.frame()

datos_nivel <- data.frame(datos_efectivo$efectivo, datos_pib$pib, datos_inpc$inpc)

pronosticos_k_pasos_vecm <- function(datos, datos_estacionalidad, k, inicio, r, lags_var){
  
  df_prons <- data.frame(matrix(ncol = 4, nrow=0))
  
  for(i in inicio:(nrow(datos)-k)){
    
    coint_test <- ca.jo(datos[1:i,], ecdet = 'none', type  = 'eigen', K = lags_var, 
                        spec = 'transitory', season = 4, dumvar = NULL)
    
    modelo_var<- vec2var(coint_test, r=r)
    
    pred_var <- predict(modelo_var, n.ahead=k)
    
    df_prons <- rbind(df_prons, pred_var$fcst$datos_efectivo.efectivo[k,])
  }
  colnames(df_prons) <- c('y_pronostico', 'CI_inf', 'CI_sup', 'CI')
  return(df_prons)
}

df_medidas <- data.frame('k' = rep(NA,8), 'error_medio' = rep(NA,8), 'MSE' = rep(NA,8), 
                         'MAE' = rep(NA,8), 'porcent_MAPE' = rep(NA,8), 'theil_U' = rep(NA,8),
                         'porcent_en_interv' = rep(NA,8))

datos_efectivo_prons <- read_rds('cache/variables/efectivo.rds') %>% 
  filter(fecha > 2011.75) 
 

for(k in 1:8){

  # El VECM se escribe como VAR. El VECM tiene 1 lags y el VAR tiene 2
  prons_vecm <- pronosticos_k_pasos_vecm(datos_nivel, datos_estacionalidad = datos_estacionalidad,
                                        k=k, inicio=44, r=1, lags_var=2)
  
  df_prons_vecm <- data.frame("fecha" = datos_efectivo_prons$fecha[k:nrow(datos_efectivo_prons)], 
                             "y_real" = datos_efectivo_prons$efectivo[k:nrow(datos_efectivo_prons)], 
                             "y_pronostico" = exp(prons_vecm$y_pronostico %>% unlist()), 
                             "CI_inf" = exp(prons_vecm$CI_inf %>% unlist()),
                             "CI_sup" = exp(prons_vecm$CI_sup %>% unlist())) %>% 
    mutate(fecha = as.numeric(fecha))
  
  
  residsk_vecm <- df_prons_vecm %>% 
    mutate(resids = y_real - y_pronostico) %>% 
    dplyr::select(resids)
  
  df_medidas$k[k] <- k
  df_medidas$error_medio[k] <- mean(residsk_vecm$resids)
  df_medidas$MSE[k] <- mean(residsk_vecm$resids^2)
  df_medidas$MAE[k] <- mean(abs(residsk_vecm$resids))
  df_medidas$porcent_MAPE[k] <- mean(abs(residsk_vecm$resids / df_prons_vecm$y_real)*100)
  
  #Para calcular U de Theil se requiere conocer el efectivo en 2011 Q4
  #Se necesita comparar el primer pronostico contra el valor histórico anterior,
  #que es 2011 Q4
  datos_efectivo_U <-  read_rds('cache/variables/efectivo.rds') %>% 
    filter(fecha >= 2011.75)
  
  df_theul_U <- df_prons_vecm %>% 
    dplyr::select(fecha, y_pronostico) %>% 
    cbind('y_t_real' = datos_efectivo_U$efectivo[1:nrow(df_prons_vecm)],
          'y_t_mas_k_real' = datos_efectivo_U$efectivo[(k+1):nrow(datos_efectivo_U)])
  
  fpe <- (df_theul_U$y_pronostico - df_theul_U$y_t_real) / df_theul_U$y_t_real
  ape <- (df_theul_U$y_t_mas_k_real - df_theul_U$y_t_real) / df_theul_U$y_t_real
  df_medidas$theil_U[k] <- sqrt(sum((fpe - ape)^2)/sum(ape^2))
  
  porcent_interv <- df_prons_vecm %>% 
    mutate(dentro_intervalo = ifelse(CI_inf <= y_real & y_real <= CI_sup,1,0)) %>% 
    pull(dentro_intervalo) %>% 
    mean()*100
  
  df_medidas$porcent_en_interv[k] <- porcent_interv
  
  write_rds(df_prons_vecm,
            paste0('cache/resultados/vecm/vecm_prons_', k, '_pasos.rds'))
}

df_medidas
write_rds(df_medidas, 'cache/resultados/vecm/vecm_medidas.rds')

# Para mayor comodidad, guardar pdf en carpeta docs
pdf("docs/vecm_medidas.pdf", height=11, width=10)
grid.table(df_medidas  %>% 
             mutate_if(is.numeric, ~sprintf("%.2f",.)))
dev.off()

