library(tidyverse)
library(urca)
library(tseries)
library(vars)
library(tsDyn) 
Sys.setlocale(locale = "es_ES.UTF-8")


# Efectivo ----------------------------------------------------------------------

datos_efectivo <- read_rds('cache/variables/last/efectivo_last.rds') %>% 
  mutate(efectivo = log(efectivo))


# PIB ----------------------------------------------------------------------

datos_pib <- read_rds('cache/variables/pib.rds') %>% 
  mutate(pib = log(pib))


# INPC ----------------------------------------------------------------------

datos_inpc <- read_rds('cache/variables/inpc_trim.rds') 



# Con los datos hasta 2011 se obtuvo que se tienen 1 lags y r=1 cointegraciones en el VECM, 
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
  
  for(i in inicio:nrow(datos)-k){
    
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

prons_vecm <- pronosticos_k_pasos_vecm(datos_nivel, datos_estacionalidad = datos_estacionalidad,
                                       k=8, inicio=45, r=1, lags_var=2)



# Predicciones --------------------------------------------------------------


df_pred <- read_rds('cache/variables/efectivo.rds') %>% 
  filter(fecha >= 2012.0) %>% 
  dplyr::select(fecha, efectivo) %>% 
  mutate(efectivo = log(efectivo)) %>% 
  cbind(prons_vecm) %>% 
  tibble()

df_pred

ggplot(data = df_pred, aes(x = fecha)) +
  geom_point(aes(y = efectivo, shape = "Observaciones"), size = 2) +
  geom_line(aes(y = fcst, color = 'Pronósticos'), size = 1) +
  geom_line(aes(y = lower), color = "orange", alpha = 0.3) +
  geom_line(aes(y = upper), color = "orange", alpha = 0.3) + 
  geom_ribbon(aes(ymax = upper, ymin = lower, fill = 'Intervalo al 95%'), 
              alpha = 0.3) +
  theme_bw() +
  scale_colour_manual(
    name = "", values = c("Intervalo al 95%" = "transparent",
                          "Pronósticos" = "orange")) +
  scale_fill_manual(
    name = "",  values = c("Intervalo al 95%" = "orange",
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
  mutate(resids = efectivo - fcst) %>%
  dplyr::select(resids)

mean(resids$resids)

var(df_pred$fcst)

mean(resids$resids^2)

SumResSquared <- sum(resids$resids^2)
TotalSumSquares <- sum((df_pred$efectivo - mean(df_pred$efectivo))^2)
RSquared <- 1 - (SumResSquared/TotalSumSquares)
RSquared


ggplot(data = df_pred, aes(x = fecha)) +
  geom_point(aes(y = exp(efectivo), shape = "Observaciones"), size = 2) +
  geom_line(aes(y = exp(efectivo.fcst), color = 'Pronósticos'), size = 1) +
  geom_line(aes(y = exp(efectivo.lower)), color = "blue", alpha = 0.3) +
  geom_line(aes(y = exp(efectivo.upper)), color = "blue", alpha = 0.3) + 
  geom_ribbon(aes(ymax = exp(efectivo.upper), ymin = exp(efectivo.lower), fill = 'Intervalo al 95%'), 
              alpha = 0.3) +
  theme_bw() +
  scale_colour_manual(
    name = "", values = c("Intervalo al 95%" = "transparent",
                          "Pronósticos" = "black")) +
  scale_fill_manual(
    name = "",  values = c("Intervalo al 95%" = "blue",
                           "Pronósticos" = "transparent")) +
  theme(legend.position = "bottom") +
  labs(shape = "") +
  ylab('Circulación') +
  xlab('Fecha') +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.text = element_text(size=20))

