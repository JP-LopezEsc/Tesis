library(tidyverse)
library(urca)
library(tseries)
library(vars)
library(tsDyn) 
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
  colnames(df_prons) <- c('fcst', 'lower', 'upper', 'CI')
  return(df_prons)
}

# El VECM se escribe como VAR. El VECM tiene 1 lags y el VAR tiene 2

k <- 8

prons_vecm <- pronosticos_k_pasos_vecm(datos_nivel, datos_estacionalidad = datos_estacionalidad,
                                       k=k, inicio=44, r=1, lags_var=2)



# Predicciones --------------------------------------------------------------

datos_efectivo_prons <- read_rds('cache/variables/efectivo.rds') %>% 
  filter(fecha >= 2012.0) %>% 
  mutate(efectivo = log(efectivo))
  

df_pred <- data.frame("fecha" = datos_efectivo_prons$fecha[k:nrow(datos_efectivo_prons)], 
                                  "y_real" = datos_efectivo_prons$efectivo[k:nrow(datos_efectivo_prons)], 
                                  "y_pronostico" = prons_vecm$fcst %>% unlist(), 
                                  "CI_inf" = prons_vecm$lower %>% unlist(),
                                  "CI_sup" =  prons_vecm$upper %>% unlist()) %>% 
  mutate(fecha = as.numeric(fecha))


df_pred

ggplot(data = df_pred, aes(x = fecha)) +
  geom_point(aes(y = y_real, shape = "Observaciones"), size = 2) +
  geom_line(aes(y = y_pronostico, color = 'Pronósticos'), size = 1) +
  geom_line(aes(y = CI_inf), color = "forestgreen", alpha = 0.3) +
  geom_line(aes(y = CI_sup), color = "forestgreen", alpha = 0.3) + 
  geom_ribbon(aes(ymax = CI_sup, ymin = CI_inf, fill = 'Intervalo al 95%'), 
              alpha = 0.3) +
  theme_bw() +
  scale_colour_manual(
    name = "", values = c("Intervalo al 95%" = "transparent",
                          "Pronósticos" = "forestgreen")) +
  scale_fill_manual(
    name = "",  values = c("Intervalo al 95%" = "forestgreen",
                           "Pronósticos" = "transparent")) +
  theme(legend.position = "bottom") +
  labs(shape = "") +
  ylab('Circulación') +
  xlab('Fecha') +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.text = element_text(size=20)) 



resids <- df_pred %>%
  mutate(resids = y_real - y_pronostico) %>%
  dplyr::select(resids)

mean(resids$resids)

var(df_pred$y_pronostico)

mean(resids$resids^2)


