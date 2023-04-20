
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
  mutate(pib = log(pib))



ggplot(datos_pib, aes(fecha, pib)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("PIB")


# Datos efectivo --------------------------------------------------------------

datos_efectivo <- read_rds('cache/variables/last/efectivo_last.rds') %>% 
  mutate(efectivo = log(efectivo))

ggplot(datos_efectivo, aes(fecha, efectivo)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("Efectivo")



# Estacionalidad ------------------------------------------------------------

datos_estacionalidad <- datos_efectivo %>% 
  mutate(Q4 = ifelse(fecha - floor(fecha) == 0.75,1,0))


## number of periods of data
TT <- length(datos_efectivo$efectivo)


## get predictor variable
pib <- matrix(datos_pib$pib, nrow=1)
q4 <- matrix(datos_estacionalidad$Q4, nrow = 1)

## number of regr params (slope + intercept)
m <- 3

## for process eqn
G <- matrix(list(0), m, m)
G[1,1] <- 'G.1'
G[2,2] <- 'G.2'
G[3,3] <- 1
W <- matrix(list(0), m, m) ## 2x2; all 0 for now #Es Wt
#diag(W) <- c("w.1", "w.2", 'w.3','w.4') ## 2x2; diag = (q1,q2)
W[1,1] <- 'W.1'
W[2,2] <- 'W.2'
W[3,3] <- 0


## for observation eqn
F <- array(NA, c(1, m, TT)) ## NxMxT; empty for now #Es Ft transpuesta
F[1, 1, ] <- rep(1, TT) ## Nx1; 1's for intercept
F[1, 2, ] <- pib ## Nx1; predictor variable
F[1, 3, ] <- q4 ## Nx1; predictor variable


y_hist <- matrix(datos_efectivo$efectivo, nrow = 1)

source('src/funciones/funciones_sin_tiie.R')

Vt <- matrix("v")

C0 <- read_rds('cache/outputs_modelos/12_periodos/sin tiie/C0.rds')

m0 <- read_rds('cache/outputs_modelos/12_periodos/sin tiie/m0.rds')

datos_F <- datos_pib %>% 
  filter(fecha >= 2004) %>% 
  left_join(datos_estacionalidad %>% 
              filter(fecha >= 2004)) %>% 
  mutate(intercept = 1) %>% 
  dplyr::select(5,2,4)

datos_efectivo_y <- datos_efectivo %>% 
  filter(fecha >= 2004)

list_interv <- list("t_int" = list(23), "at_int" = list(matrix(c(4.3085240,
                                                                 0.2674386,
                                                                 0,
                                                                 0.1267197), nrow=4)), 
                    "Rt_int" = list(matrix(c(9.260235e-04, -4.131049e-05, 0,    0,
                                             -4.131049e-05,  5.294323e-06, 0,    0,
                                             0, 0,  0,    0,
                                             0.000000e+00,  0.000000e+00,  0.000000e+00,    0
                    ), nrow=4)))

dlm_1 <- actualizacion_dlm_estima_G_W(y = datos_efectivo_y$efectivo, variables_F = datos_F,
                                      y_hist = y_hist, variables_F_hist = F, inicio = 12,
                                      m0 = m0, C0 = C0, G_forma = G, W_forma = W, V_forma = Vt, 
                                      lista_interv = list())


df_graficas <- data.frame("fecha" = datos_efectivo_y %>% dplyr::select(fecha), 
                          "y_real" = datos_efectivo_y$efectivo, 
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
  rename(Intercepto = X1, PIB = X2,  Q4 = X3) %>% 
  pivot_longer(names_to = "parametro", values_to = "valor", 
               cols = c(Intercepto, PIB,  Q4))

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
## Variance learning
##
################################################################################

V <- read_rds('cache/outputs_modelos/12_periodos/sin tiie/V.rds')


dlm_2 <- actualizacion_dlm_V_desc_estima_G_W(y = datos_efectivo_y$efectivo, 
                                             variables_F = datos_F,
                                             y_hist = y_hist, variables_F_hist = F, inicio = 12,
                                             m0 = m0, C0 = C0, G_forma = G, W_forma = W, 
                                             lista_interv = list(), S0 = as.numeric( 0.00000051), n0 = 12)


df_graficas2 <- data.frame("fecha" = datos_efectivo_y %>% dplyr::select(fecha), 
                           "y_real" = datos_efectivo_y$efectivo, 
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




df_params2 <- data.frame(reduce(dlm_2$at, cbind) %>% t(), fecha = df_graficas2$fecha)  %>% 
  rename(Intercepto = X1, PIB = X2, Q4 = X3) %>% 
  pivot_longer(names_to = "parametro", values_to = "valor", 
               cols = c(Intercepto, PIB, Q4))

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

