library(tidyverse)
library(urca)
library(tseries)
library(vars)
library(tsDyn) 
Sys.setlocale(locale = "es_ES.UTF-8")


# Efectivo ----------------------------------------------------------------------

datos_efectivo <- read_rds('cache/variables/last/efectivo_last.rds') %>% 
  filter(fecha <= 2011.75) %>% 
  mutate(efectivo = log(efectivo))

dif_efectivo <- diff(datos_efectivo$efectivo)
plot(dif_efectivo, main = "Dif efectivo", type = 'l')

# PIB ----------------------------------------------------------------------

datos_pib <- read_rds('cache/variables/pib.rds') %>% 
  filter(fecha <= 2011.75) %>% 
  mutate(pib = log(pib))

dif_pib <- diff(datos_pib$pib)
plot(dif_pib, main = "Dif PIB", type = 'l')


# TIIE ----------------------------------------------------------------------

datos_tiie <- read_rds('cache/variables/last/tiie_last.rds') %>% 
  filter(fecha <= 2011.75) %>% 
  mutate(tiie = log(tiie))

dif_tiie <- diff(datos_tiie$tiie)
plot(dif_tiie, main = "Dif TIIE", type = 'l')



# INPC ----------------------------------------------------------------------

datos_inpc <- read_rds('cache/variables/last/inpc_last.rds') %>% 
  filter(fecha <= 2011.75) %>% 
  mutate(inpc = log(inpc))

dif_inpc <- diff(datos_inpc$inpc)
plot(dif_inpc, main = "Dif INPC", type = 'l')


# Se rechaza que el efectivo sea I(0)
adf.test(datos_efectivo$efectivo, k = 4)
# El p-value apenas es mayor a 0.05, no seré tan exigente
# No se rechaza que el efectivo sea I(1)
adf.test(dif_efectivo, k = 4)

# Se rechaza que el PIB sea I(0)
adf.test(datos_pib$pib, k=2)
# No se rechaza que el PIB ses I(1)
adf.test(dif_pib, k = 2)

# Se rechaza que TIIE sea I(0)
adf.test(datos_tiie$tiie, k=4)
# Se rechaza que TIIE sea I(1)
adf.test(dif_tiie, k = 4)

# Se rechaza que el INPC sea I(0)
adf.test(datos_inpc$inpc, k=4)
# No se rechaza que el PIB ses I(1)
adf.test(dif_inpc, k = 4)


# El Aikaike indica que el mejor número de lags es 5. 
# El VECM debe tener un lag menos que el VAR, entonces el VECM es de 4 lags.

datos_nivel <- data.frame(datos_efectivo$efectivo, datos_pib$pib, datos_inpc$inpc)

VARselect(datos_nivel, type = 'const', lag.max = 6,
          season = 4)


# El estadistico de prueba para r=0 es mayor al valor critico 5%, por lo que rechazo r=0. 
# Cuando la hipotesis nula es r<=1, el estadistico de prueba no supera ningun valor critico. 
# Entonces se acepta r<=1. Como se rechazo r=0, entonces r=1 es el número de cointegraciones.
#K indica el número de lags que se obtuvo anteriormente
coint_test <- ca.jo(datos_nivel, ecdet = 'none', type  = 'eigen', K = 5, 
                    spec = 'transitory', season = 4, dumvar = NULL)
summary(coint_test)


# Ahora que sabemos que se tienen 4 lags y r=1 cointegraciones, se puede estimar el VECM

datos_estacionalidad <- datos_efectivo %>% 
  mutate(Q1 = ifelse(fecha-floor(fecha)==0, 0.75, -0.25)) %>% 
  mutate(Q2 = ifelse(fecha-floor(fecha)==0.25, 0.75, -0.25)) %>% 
  mutate(Q3 = ifelse(fecha-floor(fecha)==0.75, 0.75, -0.25)) %>% 
  dplyr::select(Q1:Q3) %>% 
  as.data.frame()

modelo_vecm <- VECM(datos_nivel, lag=4, r=1,
                    estim = 'ML',
                    LRinclude = 'none',
                    exogen = datos_estacionalidad)

summary(modelo_vecm)

coefA(modelo_vecm)

coefB(modelo_vecm)

# El VECM se escribe como VAR. El VECM tiene 4 lags y el VAR tiene 5
modelo_var<- vec2var(coint_test, r=1)
modelo_var




# Predicciones --------------------------------------------------------------


pred_var <- predict(modelo_var, n.ahead=44)
pred_var

df_pred <- read_rds('cache/variables/last/efectivo_last.rds') %>% 
  filter(fecha >= 2012.0) %>% 
  dplyr::select(fecha, efectivo) %>% 
  mutate(efectivo = log(efectivo)) %>% 
  cbind(data.frame('efectivo' = pred_var$fcst$datos_efectivo.efectivo))

df_pred

ggplot(data = df_pred, aes(x = fecha)) +
  geom_point(aes(y = efectivo, shape = "Observaciones"), size = 2) +
  geom_line(aes(y = efectivo.fcst, color = 'Pronósticos'), size = 1) +
  geom_line(aes(y = efectivo.lower), color = "blue", alpha = 0.3) +
  geom_line(aes(y = efectivo.upper), color = "blue", alpha = 0.3) + 
  geom_ribbon(aes(ymax = efectivo.upper, ymin = efectivo.lower, fill = 'Intervalo al 95%'), 
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


