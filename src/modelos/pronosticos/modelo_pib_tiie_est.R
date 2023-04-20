



library(tidyverse)
library(janitor)
library(lubridate)
library('MARSS')
library('broom')
library(ppcor)
Sys.setlocale(locale = "es_ES.UTF-8")

################################################################################
## Datos
##
################################################################################

datos_pib <- read_rds('cache/variables/pib.rds') %>% 
  filter(fecha >=2014.0) %>% 
  mutate(pib = log(pib))



ggplot(datos_pib, aes(fecha, pib)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("PIB")



# Datos TIIE ----------------------------------------------------------

datos_tiie <- read_rds('cache/variables/last/tiie_last.rds') %>% 
  filter(fecha >=2014.0) %>% 
  mutate(tiie = log(tiie))



ggplot(datos_tiie, aes(fecha, tiie)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("TIIE")


# Datos efectivo --------------------------------------------------------------

datos_efectivo <- read_rds('cache/variables/last/efectivo_last.rds') %>% 
  filter(fecha >=2014.0) %>% 
  mutate(efectivo = log(efectivo))

ggplot(datos_efectivo, aes(fecha, efectivo)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("Efectivo")

cor(datos_efectivo$efectivo, datos_pib$pib)
cor(datos_efectivo$efectivo, datos_tiie$tiie)

pcor(data.frame(datos_efectivo$efectivo, datos_pib$pib, datos_tiie$tiie))


# Estacionalidad ------------------------------------------------------------

datos_estacionalidad <- datos_efectivo %>% 
  mutate(Q4 = ifelse(fecha - floor(fecha) == 0.75,1,0))




################################################################################
## Modelo con varianza "conocida"
##
################################################################################
source('src/funciones/funciones_dlm.R')

Vt <- read_rds('cache/outputs_modelos/V.rds')

Gt <- read_rds('cache/outputs_modelos/G.rds')

Wt <- read_rds('cache/outputs_modelos/W.rds')

C0 <- read_rds('cache/outputs_modelos/C0.rds')

m0 <- read_rds('cache/outputs_modelos/m0.rds')

datos_F <- datos_pib %>% 
  left_join(datos_tiie) %>% 
  left_join(datos_estacionalidad) %>% 
  mutate(intercept = 1) %>% 
  dplyr::select(6,2,3,5)



dlm_1 <- actualizacion_dlm(y = datos_efectivo$efectivo, variables_F = datos_F, 
                           m0 = m0, C0 = C0, G = Gt, W = Wt, V = Vt, lista_interv = list())


df_graficas <- data.frame("fecha" = datos_efectivo %>% dplyr::select(fecha), 
                          "y_real" = datos_efectivo$efectivo, 
                          "y_pronostico" = dlm_1$ft %>% unlist(), "CI_inf" = dlm_1$CI_inf %>% unlist(),
                          "CI_sup" = dlm_1$CI_sup %>% unlist()) %>% 
  mutate(fecha = as.numeric(fecha))


ggplot(data = df_graficas, aes(x = fecha)) +
  geom_point(aes(y = y_real, shape = "Observaciones"), size = 2) +
  geom_line(aes(y = y_pronostico, color = 'Pronósticos'), size = 1) +
  geom_line(aes(y = CI_inf), color = "blue", alpha = 0.3) +
  geom_line(aes(y = CI_sup), color = "blue", alpha = 0.3) + 
  geom_ribbon(aes(ymax = CI_sup, ymin = CI_inf, fill = 'Intervalo al 95%'), alpha = 0.3) +
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




df_params <- data.frame(reduce(dlm_1$at, cbind) %>% t(), fecha = df_graficas$fecha)  %>% 
  rename(Intercepto = X1, PIB = X2, TIIE = X3, Q4 = X4) %>% 
  pivot_longer(names_to = "parametro", values_to = "valor", 
               cols = c(Intercepto, PIB, TIIE, Q4))

ggplot(df_params, aes(x=fecha, y = valor)) +
  geom_line() +
  facet_wrap(~parametro, nrow = 4, scales = "free") +
  theme_bw()  +
  ylab("Valor") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        strip.text = element_text(size=20)) 



################################################################################
## Modelo con varianza desconocida
##
################################################################################


dlm_2 <- actualizacion_dlm_V_desc(y = datos_efectivo$efectivo, variables_F = datos_F, 
                           m0 = m0, C0 = C0, G = Gt, W = Wt, S0 = as.numeric(Vt), n0=1,
                           lista_interv = list())


df_graficas2 <- data.frame("fecha" = datos_efectivo %>% dplyr::select(fecha), 
                          "y_real" = datos_efectivo$efectivo, 
                          "y_pronostico" = dlm_2$ft %>% unlist(), "CI_inf" = dlm_2$CI_inf %>% unlist(),
                          "CI_sup" = dlm_2$CI_sup %>% unlist()) %>% 
  mutate(fecha = as.numeric(fecha))


ggplot(data = df_graficas2, aes(x = fecha)) +
  geom_point(aes(y = y_real, shape = "Observaciones"), size = 2) +
  geom_line(aes(y = y_pronostico, color = 'Pronósticos'), size = 1) +
  geom_line(aes(y = CI_inf), color = "blue", alpha = 0.3) +
  geom_line(aes(y = CI_sup), color = "blue", alpha = 0.3) + 
  geom_ribbon(aes(ymax = CI_sup, ymin = CI_inf, fill = 'Intervalo al 95%'), alpha = 0.3) +
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




df_params2 <- data.frame(reduce(dlm_2$mt, cbind) %>% t(), fecha = df_graficas$fecha)  %>% 
  rename(Intercepto = X1, PIB = X2, TIIE = X3, Q4 = X4) %>% 
  pivot_longer(names_to = "parametro", values_to = "valor", 
               cols = c(Intercepto, PIB, TIIE, Q4))

ggplot(df_params2, aes(x=fecha, y = valor)) +
  geom_line() +
  facet_wrap(~parametro, nrow = 4, scales = "free") +
  theme_bw()  +
  ylab("Valor") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        strip.text = element_text(size=20)) 


################################################################################
## Modelo con MARSS
##
################################################################################


## number of periods of data
TT <- length(datos_efectivo$efectivo)
## get response variable: efectivo
dat <- matrix(datos_efectivo$efectivo, nrow = 1)

## get predictor variable
pib <- matrix(datos_pib$pib, nrow=1)
tiie <- matrix(datos_tiie$tiie, nrow=1)
q4 <- matrix(datos_estacionalidad$Q4, nrow = 1)

## number of regr params (slope + intercept)
m <- 4

## for process eqn
G <- read_rds('cache/outputs_modelos/G.rds')
U <- matrix(0, nrow = m, ncol = 1) ## 2x1; both elements = 0
W <- read_rds('cache/outputs_modelos/W.rds')




## for observation eqn
F <- array(NA, c(1, m, TT)) ## NxMxT; empty for now #Es Ft transpuesta
F[1, 1, ] <- rep(1, TT) ## Nx1; 1's for intercept
F[1, 2, ] <- pib ## Nx1; predictor variable
F[1, 3, ] <- tiie ## Nx1; predictor variable
F[1, 4, ] <- q4 ## Nx1; predictor variable
A <- matrix(0) ## 1x1; scalar = 0
V <- read_rds('cache/outputs_modelos/V.rds')

## only need starting values for regr parameters
## list of model matrices & vectors
mod_list <- list(B = G, U = U, Q = W, Z = F, A = A, R = V, 
                 x0 = read_rds('cache/outputs_modelos/m0.rds'),
                 V0 = read_rds('cache/outputs_modelos/C0.rds'))

## fit univariate DLM
dlm_3 <- MARSS(dat, model = mod_list, method = 'BFGS')

dlm_3$states
dlm_3$states.se
dlm_3$ytT

glance(dlm_3)
MARSSkfas(dlm_3)

## get list of Kalman filter output
kf_out <- MARSSkfss(dlm_3)
## forecasts of regr parameters; 2xT matrix
theta_priori <- kf_out$xtt1
## ts of E(forecasts)
one_step_forecast <- vector()
for (t in 1:TT) {
  #browser()
  one_step_forecast[t] <- F[, , t] %*% theta_priori[, t, drop = FALSE]
}

## variance of regr prior parameters; 1x2xT array
R <- kf_out$Vtt1
## obs variance; 1x1 matrix
V_est <- coef(dlm_3, type = "matrix")$R
## ts of Var(forecasts)
Q <- vector()
for (t in 1:TT) {
  tF <- matrix(F[, , t], m, 1) ## transpose of F
  Q[t] <- F[, , t] %*% R[, , t] %*% tF + V_est
}



df_result3 <- data.frame('fecha' = as.numeric(datos_efectivo$fecha), 'y_real' = datos_efectivo$efectivo, 
                        'y_pronostico' = one_step_forecast, 
                        'CI_inf' = qnorm(0.025, mean = one_step_forecast, sd = sqrt(Q)),
                        'CI_sup' = qnorm(0.975, mean = one_step_forecast, sd = sqrt(Q)))



ggplot(data = df_result3, aes(x = fecha)) +
  geom_point(aes(y = y_real, shape = "Observaciones"), size = 2) +
  geom_line(aes(y = y_pronostico, color = 'Pronósticos'), size = 1) +
  geom_line(aes(y = CI_inf), color = "blue", alpha = 0.3) +
  geom_line(aes(y = CI_sup), color = "blue", alpha = 0.3) + 
  geom_ribbon(aes(ymax = CI_sup, ymin = CI_inf, fill = 'Intervalo al 95%'), alpha = 0.3) +
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



df_params3 <- data.frame('fecha' = as.numeric(datos_efectivo$fecha), 'intercept' = theta_priori[1,], 
                        'pib' = theta_priori[2,], 'tiie' = theta_priori[3,], 
                        'Q4' = theta_priori[4,]) %>% 
  pivot_longer(cols = c(intercept:Q4), names_to = 'parametro', values_to = 'valor')


ggplot(df_params3, aes(x=fecha, y = valor)) +
  geom_line() +
  facet_wrap(~parametro, nrow = 4, scales = "free") +
  theme_bw()  +
  ylab("Valor") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        strip.text = element_text(size=20)) 


