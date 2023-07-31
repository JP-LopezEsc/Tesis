library(tidyverse)
library(janitor)
library(lubridate)
library('MARSS')
library('broom')
library(ppcor)
Sys.setlocale(locale = "es_ES.UTF-8")

source('src/funciones/funciones_dlm.R')

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


# Valores iniciales DLM -----------------------------------------------------


C0 <- read_rds('cache/output_modelo_historico/C0.rds')

m0 <- read_rds('cache/output_modelo_historico/m0.rds')

G <- read_rds('cache/output_modelo_historico/G.rds')

W <- read_rds('cache/output_modelo_historico/W.rds')

S0 <- as.numeric(read_rds('cache/output_modelo_historico/V.rds'))


# Variables explicativas ----------------------------------------------------


datos_F <- datos_pib %>% 
  left_join(datos_estacionalidad) %>% 
  left_join(datos_inpc) %>% 
  mutate(intercept = 1) %>% 
  dplyr::select(9,2, 8, 4:7)


################################################################################
## DLM varianza desconocida
##
################################################################################


modelo_dlm <- actualizacion_dlm_V_desc(y = datos_efectivo$efectivo, 
                                       variables_F = datos_F,
                                       m0 = m0, 
                                       C0 = C0, 
                                       G = G, 
                                       W = W, 
                                       S0 = S0, 
                                       n0 = 44,
                                       lista_interv = list())


df_dlm_efectivo <- data.frame("fecha" = datos_efectivo %>% dplyr::select(fecha), 
                           "y_real" = datos_efectivo$efectivo, 
                           "y_pronostico" = modelo_dlm$ft %>% unlist(), 
                           "CI_inf" = modelo_dlm$CI_inf %>% unlist(),
                           "CI_sup" = modelo_dlm$CI_sup %>% unlist()) %>% 
  mutate(fecha = as.numeric(fecha))


ggplot(data = df_dlm_efectivo, aes(x = fecha)) +
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




df_dlm_params <- data.frame(reduce(modelo_dlm$mt, cbind) %>% t(), 
                            fecha =  datos_efectivo %>% dplyr::select(fecha))  %>% 
  rename(Intercepto = X1, PIB = X2,  INPC = X3, Q1 = X4) %>% 
  pivot_longer(names_to = "parametro", values_to = "valor", 
               cols = c(Intercepto, PIB, INPC,  Q1))

ggplot(df_dlm_params, aes(x=fecha, y = valor)) +
  geom_line() +
  facet_wrap(~parametro, nrow = 4, scales = "free") +
  theme_bw()  +
  ylab("Valor") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        strip.text = element_text(size=20)) 

df_St <- data.frame(modelo_dlm$St %>% unlist(), 
                    fecha =  datos_efectivo %>% dplyr::select(fecha)) %>% 
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


resids_dlm <- df_dlm_efectivo %>% 
  mutate(resids = y_real - y_pronostico) %>% 
  dplyr::select(resids)

acf(resids_dlm)

mean(resids_dlm$resids)

var(df_dlm_efectivo$y_pronostico)

mean(resids_dlm$resids^2)

SumResSquared <- sum(resids_dlm$resids^2)
TotalSumSquares <- sum((df_dlm_efectivo$y_real - mean(df_dlm_efectivo$y_real))^2)
RSquared <- 1 - (SumResSquared/TotalSumSquares)
RSquared



# Pronósticos ---------------------------------------------------------------

# El VECM hace sus propios pronósticos del PIB y de la inflación. Para que sea justa
# la comparación, utilizaré pronósticos de encuestas que hace Banxico para cada
# periodo para obtener los pronósticos del dlm. Estos datos están disponibles en 
# https://www.banxico.org.mx/publicaciones-y-prensa/encuestas-sobre-las-expectativas-de-los-especialis/encuestas-expectativas-del-se.html
# Esos pronósticos de Banxico se guardaron en el archivo 'cache/variables/prons_banxico.rds'

prons_F_banxico <- read_rds('cache/variables/prons_banxico.rds')
  
k <- 8

prons_dlm <- pronosticos_k_pasos(prons_F = prons_F_banxico, k, modelo = modelo_dlm)

df_prons_dlm <- data.frame("fecha" = datos_efectivo$fecha[k:nrow(datos_F)], 
                        "y_real" = datos_efectivo$efectivo[k:nrow(datos_F)], 
                        "y_pronostico" = prons_dlm$ft_k %>% unlist(), 
                        "CI_inf" = prons_dlm$CI_inf %>% unlist(),
                        "CI_sup" = prons_dlm$CI_sup %>% unlist()) %>% 
  mutate(fecha = as.numeric(fecha))


ggplot(data = df_prons_dlm, aes(x = fecha)) +
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


residsk_dlm <- df_prons_dlm %>% 
  mutate(resids = y_real - y_pronostico) %>% 
  dplyr::select(resids)


mean(residsk_dlm$resids)

var(df_prons_dlm$y_pronostico)

mean(residsk_dlm$resids^2)
