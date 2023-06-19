library(tidyverse)
library(urca)
library(tseries)
library(vars)
library(tsDyn) 
Sys.setlocale(locale = "es_ES.UTF-8")

# https://www.r-bloggers.com/2021/11/vector-autoregressive-model-var-using-r/

#https://www.r-bloggers.com/2021/12/vector-error-correction-model-vecm-using-r/

################################################################################
## Datos
##
################################################################################

# Datos efectivo --------------------------------------------------------------

datos_efectivo <- read_rds('cache/variables/last/efectivo_last.rds') %>% 
  filter(fecha <= 2011.75) %>% 
  mutate(efectivo = log(efectivo))

ggplot(datos_efectivo, aes(fecha, efectivo)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("Efectivo")

dif_efectivo <- diff(datos_efectivo$efectivo)
plot(dif_efectivo, main = "Dif efectivo", type = 'l')



# Datos PIB -----------------------------------------------------------------


datos_pib <- read_rds('cache/variables/pib.rds') %>% 
  filter(fecha <= 2011.75) %>% 
  mutate(pib = log(pib))



ggplot(datos_pib, aes(fecha, pib)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("PIB")


dif_pib <- diff(datos_pib$pib)
plot(dif_pib, main = "Dif PIB", type = 'l')




# TIIE ----------------------------------------------------------------------


datos_tiie <- read_rds('cache/variables/last/tiie_last.rds') %>% 
  filter(fecha <= 2011.75) %>% 
  mutate(tiie = log(tiie))


ggplot(datos_tiie, aes(fecha, tiie)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("TIIE")


dif_tiie <- diff(datos_tiie$tiie)
plot(dif_tiie, main = "Dif TIIE", type = 'l')



# INPC ----------------------------------------------------------------------

datos_inpc <- read_rds('cache/variables/last/inpc_last.rds') %>% 
  filter(fecha <= 2011.75) %>% 
  mutate(inpc = log(inpc))


ggplot(datos_inpc, aes(fecha, inpc)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("INPC")


dif_inpc <- diff(datos_inpc$inpc)
plot(dif_inpc, main = "Dif INPC", type = 'l')



# FIX -----------------------------------------------------------------------


datos_fix <- read_rds('cache/variables/last/fix_last.rds') %>% 
  filter(fecha <= 2011.75) %>% 
  mutate(fix = log(fix))


ggplot(datos_fix, aes(fecha, fix)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("FIX")


dif_fix <- diff(datos_fix$fix)
plot(dif_fix, main = "Dif FIX", type = 'l')



# Ordenes de integracion ----------------------------------------------------

adf.test(datos_efectivo$efectivo, k = 4)
adf.test(dif_efectivo, k = 2)

adf.test(datos_pib$pib, k=2)
adf.test(dif_pib, k = 2)

adf.test(datos_tiie$tiie, k=4)
adf.test(dif_tiie, k = 4)

adf.test(datos_inpc$inpc, k=4)
adf.test(dif_inpc, k = 4)

adf.test(datos_fix$fix, k=4)
adf.test(dif_fix, k = 4)

# Todas están cointegradas de orden uno, menos TIIE.
# Usaré efectivo, pib, inpc y fix

# Escojo 5 lags. El VECM tendrá 5-1 = 4 lags
VARselect(data.frame(datos_efectivo$efectivo, datos_pib$pib, 
                     datos_inpc$inpc), type = 'const', lag.max = 6,
          season = 4)



# Estacionalidad ------------------------------------------------------------


datos_estacionalidad <- datos_efectivo %>% 
  mutate(Q1 = ifelse(fecha-floor(fecha)==0, 0.75, -0.25)) %>% 
  mutate(Q2 = ifelse(fecha-floor(fecha)==0.25, 0.75, -0.25)) %>% 
  mutate(Q3 = ifelse(fecha-floor(fecha)==0.75, 0.75, -0.25)) %>% 
  dplyr::select(Q1:Q3) %>% 
  as.data.frame()
  
  
# VECM ----------------------------------------------------------------------

datos_nivel <- data.frame(datos_efectivo$efectivo, datos_pib$pib, 
           datos_inpc$inpc)

nr_lev <- nrow(datos_nivel)

coint_ca.jo <- ca.jo(datos_nivel, ecdet = 'none', type  = 'eigen', K = 5, 
  spec = 'transitory', season = 4, dumvar = NULL)
summary(coint_ca.jo)

summary(ca.jo(datos_nivel, ecdet = 'none', type  = 'trace', K = 5, 
      spec = 'transitory', season = 4, dumvar = NULL))

# El estadistico de prueba para r=0 es mayor al valor critico 5%, 
# por lo que rechazo r=0. Cuando la hipotesis nula es r<=1, el estadistico de prueba
# no supera ningun valor critico. Entonces se acepta r<=1. Como se rechazo r=0,
#entonces r=1

#========================================================
# VECM model estimation
#========================================================

#————————————————
# VECM estimation
#————————————————
# VECM(data, lag, r = 1, 
#      include = c(“const”, “trend”, “none”, “both”),
#      beta = NULL, estim = c(“2OLS”, “ML”), 
#      LRinclude = c(“none”, “const”,”trend”, “both”), 
#      exogen = NULL)
#————————————————

#Lag de vecm es lag de var -1
VECM_tsDyn <- VECM(datos_nivel, lag=4, r=1,
                   estim = 'ML',
                   LRinclude = 'none',
                   exogen = datos_estacionalidad)

#————————————————
# restricted VECM -> input for r
#————————————————
cajorls_ca.jo <- cajorls(coint_ca.jo, r=1)
coint_ca.jo@V
coint_ca.jo@W
coint_ca.jo@GAMMA
#————————————————
# the VAR representation of a VECM from ca.jo
#————————————————
# vec2var: Transform a VECM to VAR in levels
# ca.jo is transformed to a VAR in level
# r : The cointegration rank 
#————————————————
vec2var_ca.jo <- vec2var(coint_ca.jo, r=1)


#========================================================
# Estimation Results
#========================================================

#———————————————-
# parameter estimates from each model
#———————————————-
VECM_tsDyn
cajorls_ca.jo
vec2var_ca.jo


#========================================================
# Forecast
#========================================================

# forecasting horizon
nhor <- 12 

#———————————————-
# Forecast from VECM() in tsDyn
#———————————————-

# quarterly centered dummy variables for forecast
estacion_f <- rbind(tail(datos_estacionalidad,4),
                     tail(datos_estacionalidad,4),
                     tail(datos_estacionalidad,4))

VECM_pred_tsDyn <- predict(VECM_tsDyn, 
                           exoPred = estacion_f, n.ahead=nhor)


# historical data + forecast data
df <- rbind(datos_nivel, VECM_pred_tsDyn)

for(i in 1:3) {
  matplot(df[,i], type=c('l'), col = c('blue')) 
  abline(v=nr_lev, col='blue')
}

VECM_pred_tsDyn

#———————————————-
# Forecast from ca.jo() using vec2var()
#———————————————-

pred_vec2var_ca.jo <- predict(vec2var_ca.jo, n.ahead=nhor)


m.pred_vec2var_ca.jo <- cbind(
  pred_vec2var_ca.jo$fcst$datos_efectivo.efectivo[,1], 
  pred_vec2var_ca.jo$fcst$datos_pib.pib[,1],
  pred_vec2var_ca.jo$fcst$datos_inpc.inpc[,1])

colnames(m.pred_vec2var_ca.jo) <- colnames(datos_nivel)

m.pred_vec2var_ca.jo

#———————————————-
# Comparison of two sets of forecast
#———————————————-

VECM_pred_tsDyn - m.pred_vec2var_ca.jo

