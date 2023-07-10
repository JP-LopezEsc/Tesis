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
  filter(fecha > 2011.75) %>% 
  mutate(pib = log(pib))



ggplot(datos_pib, aes(fecha, pib)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("PIB")


# Datos INPC ----------------------------------------------------------------

datos_inpc <- read_rds('cache/variables/inpc_trim.rds') %>% 
  filter(fecha > 2011.75)



ggplot(datos_inpc, aes(fecha, inpc)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("INPC")



# Datos efectivo --------------------------------------------------------------

datos_efectivo <- read_rds('cache/variables/efectivo.rds') %>% 
  filter(fecha > 2011.75) %>% 
  mutate(efectivo = log(efectivo))

ggplot(datos_efectivo, aes(fecha, efectivo)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("Efectivo")



# Estacionalidad ------------------------------------------------------------

datos_estacionalidad <- datos_efectivo %>% 
  mutate(Q1 = 1) %>% 
  mutate(Q2=0) %>% 
  mutate(Q3=0) %>% 
  mutate(Q4=0)


source('src/funciones/funciones_dlm.R')

C0 <- read_rds('cache/outputs_modelos/inpc_trim/C0.rds')

m0 <- read_rds('cache/outputs_modelos/inpc_trim/m0.rds')

G <- read_rds('cache/outputs_modelos/inpc_trim/G.rds')

W <- read_rds('cache/outputs_modelos/inpc_trim/W.rds')

V <- as.numeric(read_rds('cache/outputs_modelos/inpc_trim/V.rds'))

datos_F <- datos_pib %>% 
  left_join(datos_estacionalidad) %>% 
  left_join(datos_inpc) %>% 
  mutate(intercept = 1) %>% 
  dplyr::select(9,2, 8, 4:7)



dlm_1 <- actualizacion_dlm(y = datos_efectivo$efectivo, variables_F = datos_F,
                           m0 = m0, C0 = C0, G = G, W = W, V = V, 
                           lista_interv = list())


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


resids <- df_graficas %>% 
  mutate(resids = y_real - y_pronostico) %>% 
  dplyr::select(resids)

acf(resids)

mean(resids$resids)

var(df_graficas$y_pronostico)

mean(resids$resids^2)

SumResSquared <- sum(resids$resids^2)
TotalSumSquares <- sum((df_graficas$y_real - mean(df_graficas$y_real))^2)
RSquared <- 1 - (SumResSquared/TotalSumSquares)
RSquared

################################################################################
## Variance learning
##
################################################################################

S0 <- V

datos_F <- datos_pib %>% 
  left_join(datos_estacionalidad) %>% 
  left_join(datos_inpc) %>% 
  mutate(intercept = 1) %>% 
  dplyr::select(9,2, 8, 4:7)



dlm_2 <- actualizacion_dlm_V_desc(y = datos_efectivo$efectivo, variables_F = datos_F,
                                  m0 = m0, C0 = C0, G = G, W = W, S0 = S0, n0 = 44,
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




df_params2 <- data.frame(reduce(dlm_2$at, cbind) %>% t(), fecha = df_graficas2$fecha)  %>% 
  rename(Intercepto = X1, PIB = X2,  INPC = X3, Q1 = X4) %>% 
  pivot_longer(names_to = "parametro", values_to = "valor", 
               cols = c(Intercepto, PIB, INPC,  Q1))

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

df_St <- data.frame(dlm_2$St %>% unlist(), fecha = df_graficas2$fecha) %>% 
  dplyr::rename(St = 1)

ggplot(df_St, aes(x=fecha, y = St)) +
  geom_line(size = 1) +
  theme_bw()  +
  ylab("St") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.text = element_text(size=20)) 


resids2 <- df_graficas2 %>% 
  mutate(resids = y_real - y_pronostico) %>% 
  dplyr::select(resids)

acf(resids2)

mean(resids2$resids)

var(df_graficas2$y_pronostico)

mean(resids2$resids^2)

SumResSquared2 <- sum(resids2$resids^2)
TotalSumSquares2 <- sum((df_graficas2$y_real - mean(df_graficas2$y_real))^2)
RSquared2 <- 1 - (SumResSquared2/TotalSumSquares2)
RSquared2



# Pronósticos ---------------------------------------------------------------

k <- 2

prons2 <- pronosticos_k_pasos(datos_F, k, modelo = dlm_2)

df_prons2 <- data.frame("fecha" = datos_efectivo$fecha[k:nrow(datos_F)], 
                        "y_real" = datos_efectivo$efectivo[k:nrow(datos_F)], 
                        "y_pronostico" = prons2$ft_k %>% unlist(), "CI_inf" = prons2$CI_inf %>% unlist(),
                        "CI_sup" = prons2$CI_sup %>% unlist()) %>% 
  mutate(fecha = as.numeric(fecha))


ggplot(data = df_prons2, aes(x = fecha)) +
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


residsk <- df_prons2 %>% 
  mutate(resids = y_real - y_pronostico) %>% 
  dplyr::select(resids)


mean(residsk$resids)

var(df_prons2$y_pronostico)

mean(residsk$resids^2)

SumResSquaredk <- sum(residsk$resids^2)
TotalSumSquaresk <- sum((df_prons2$y_real - mean(df_prons2$y_real))^2)
RSquaredk <- 1 - (SumResSquaredk/TotalSumSquaresk)
RSquaredk


################################################################################
## Intervencion
##
################################################################################

a_34 <-dlm_2$at[[34]]
R_34 <-dlm_2$Rt[[34]]
a_34
R_34

a_34_int <- a_34
a_34_int[1,] <- 0.1
a_34_int

R_34_int <- R_34
R_34_int[1,1] <- R_34[1,1]*2
R_34_int

list_interv <- list("t_int" = list(34),
                    "at_int" = list(a_34_int),
                    "Rt_int" = list(R_34_int))

dlm_interv <- actualizacion_dlm_V_desc(y = datos_efectivo$efectivo, variables_F = datos_F,
                                       m0 = m0, C0 = C0, G = G, W = W, S0 = S0, n0 = 44,
                                       lista_interv = list_interv)

df_interv <- data.frame("fecha" = datos_efectivo %>% dplyr::select(fecha), 
                           "y_real" = datos_efectivo$efectivo, 
                           "y_pronostico" = dlm_interv$ft %>% unlist(), 
                            "CI_inf" = dlm_interv$CI_inf %>% unlist(),
                           "CI_sup" = dlm_interv$CI_sup %>% unlist()) %>% 
  mutate(fecha = as.numeric(fecha))


ggplot(data = df_interv, aes(x = fecha)) +
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


df_params_interv <- data.frame(reduce(dlm_interv$at, cbind) %>% t(), 
                               fecha = df_interv$fecha)  %>% 
  rename(Intercepto = X1, PIB = X2,  INPC = X3, Q1 = X4) %>% 
  pivot_longer(names_to = "parametro", values_to = "valor", 
               cols = c(Intercepto, PIB, INPC,  Q1))

ggplot(df_params_interv, aes(x=fecha, y = valor)) +
  geom_line() +
  facet_wrap(~parametro, nrow = 4, scales = "free") +
  theme_bw()  +
  ylab("Valor") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        strip.text = element_text(size=20)) 

df_St_interv <- data.frame(dlm_interv$St %>% unlist(), fecha = df_interv$fecha) %>% 
  dplyr::rename(St = 1)

ggplot(df_St_interv, aes(x=fecha, y = St)) +
  geom_line(size = 1) +
  theme_bw()  +
  ylab("St") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.text = element_text(size=20)) 


resids_interv <- df_interv %>% 
  mutate(resids = y_real - y_pronostico) %>% 
  dplyr::select(resids)

acf(resids_interv)

mean(resids_interv$resids)

var(df_interv$y_pronostico)

mean(resids_interv$resids^2)



# Pronosticos K pasos -------------------------------------------------------

k_int <- 2

prons2_int <- pronosticos_k_pasos(datos_F, k_int, modelo = dlm_interv, 
                                  lista_interv = list_interv,
                                  per_anticip_interv =1)

df_prons2_int <- data.frame("fecha" = datos_efectivo$fecha[k_int:nrow(datos_F)], 
                        "y_real" = datos_efectivo$efectivo[k_int:nrow(datos_F)], 
                        "y_pronostico" = prons2_int$ft_k %>% unlist(), 
                        "CI_inf" = prons2_int$CI_inf %>% unlist(),
                        "CI_sup" = prons2_int$CI_sup %>% unlist()) %>% 
  mutate(fecha = as.numeric(fecha))


ggplot(data = df_prons2_int, aes(x = fecha)) +
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


residsk_int <- df_prons2_int %>% 
  mutate(resids = y_real - y_pronostico) %>% 
  dplyr::select(resids)


mean(residsk_int$resids)

var(df_prons2_int$y_pronostico)

mean(residsk_int$resids^2)




