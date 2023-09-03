library(tidyverse)
library(janitor)
library(lubridate)
library('MARSS')
library('broom')
library(LaplacesDemon)
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



# Analisis cambio por pandemia ----------------------------------------------

# En el informe trimestral de 2020 Q1 de Banxico ya se había detectado el
# incremento de la demanda de dinero por la incertidumbre de la pandemia.
# Usando datos hasta 2020 Q1 se propone una intervención para 2020 Q2

df_dif_q4_q1 <- datos_efectivo %>% 
  mutate(dif = efectivo - lag(efectivo)) %>% 
    filter(fecha - round(fecha) == 0) %>% 
  drop_na()


ggplot(df_dif_q4_q1, aes(x = fecha, y = dif)) +
  geom_col() +
  theme_bw()

dif_q4_q1_2019 <- df_dif_q4_q1 %>% 
  filter(fecha == 2019) %>% 
  dplyr::select(dif) %>% 
  as.numeric()

dif_q4_q1_2020 <- df_dif_q4_q1 %>% 
  filter(fecha == 2020) %>% 
  dplyr::select(dif) %>% 
  as.numeric()

# Agregar el siguiente valor al intercepto en la intervencion

dif_q4_q1_2020 - dif_q4_q1_2019

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


################################################################################
## Intervencion
##
################################################################################

a_34 <-modelo_dlm$at[[34]]
R_34 <-modelo_dlm$Rt[[34]]
a_34
R_34

a_34_int <- a_34
a_34_int[1,] <- a_34[1,] + dif_q4_q1_2020 - dif_q4_q1_2019
a_34_int

R_34_int <- R_34
R_34_int[1,1] <- R_34[1,1]*10
R_34_int

qst(c(0.025,0.975), nu = 33, mu = a_34[1,], sigma = sqrt(R_34[1,1]))
qst(c(0.025,0.975), nu=33, mu = a_34_int[1,], sigma = sqrt(R_34_int[1,1]))

list_interv <- list("t_int" = list(34),
                    "at_int" = list(a_34_int),
                    "Rt_int" = list(R_34_int))

modelo_dlm_interv <- actualizacion_dlm_V_desc(y = datos_efectivo$efectivo, 
                                              variables_F = datos_F,
                                              m0 = m0, 
                                              C0 = C0, 
                                              G = G, 
                                              W = W, 
                                              S0 = S0, 
                                              n0 = 44,
                                              lista_interv = list_interv)

# Guarda modelo
# modelo_dlm_interv %>% write_rds('cache/modelos/modelo_dlm_interv.rds')

df_dlm_interv <- data.frame("fecha" = datos_efectivo %>% dplyr::select(fecha), 
                            "y_real" = datos_efectivo$efectivo, 
                            "y_pronostico" = modelo_dlm_interv$ft %>% unlist(), 
                            "CI_inf" = modelo_dlm_interv$CI_inf %>% unlist(),
                            "CI_sup" = modelo_dlm_interv$CI_sup %>% unlist()) %>% 
  mutate(fecha = as.numeric(fecha))


ggplot(data = df_dlm_interv, aes(x = fecha)) +
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


df_interv_params <- data.frame(reduce(modelo_dlm_interv$at, cbind) %>% t(), 
                               fecha = datos_efectivo %>% dplyr::select(fecha))  %>% 
  rename(Intercepto = X1, PIB = X2,  INPC = X3, Q1 = X4) %>% 
  pivot_longer(names_to = "parametro", values_to = "valor", 
               cols = c(Intercepto, PIB, INPC,  Q1))

ggplot(df_interv_params, aes(x=fecha, y = valor)) +
  geom_line() +
  facet_wrap(~parametro, nrow = 4, scales = "free") +
  theme_bw()  +
  ylab("Valor") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        strip.text = element_text(size=20)) 

df_St_interv <- data.frame(modelo_dlm_interv$St %>% unlist(), 
                           fecha = datos_efectivo %>% dplyr::select(fecha)) %>% 
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


resids_interv <- df_dlm_interv %>% 
  mutate(resids = y_real - y_pronostico) %>% 
  dplyr::select(resids)

acf(resids_interv)

mean(resids_interv$resids)

var(df_dlm_interv$y_pronostico)

mean(resids_interv$resids^2)



# Pronosticos K pasos -------------------------------------------------------

# El VECM hace sus propios pronósticos del PIB y de la inflación. Para que sea justa
# la comparación, utilizaré pronósticos de encuestas que hace Banxico para cada
# periodo para obtener los pronósticos del dlm. Estos datos están disponibles en 
# https://www.banxico.org.mx/publicaciones-y-prensa/encuestas-sobre-las-expectativas-de-los-especialis/encuestas-expectativas-del-se.html
# Esos pronósticos de Banxico se guardaron en el archivo 'cache/variables/prons_banxico.rds'

prons_F_banxico <- read_rds('cache/variables/prons_banxico.rds')[1:44]


k_int <- 8

# Como el efecto de la pandemia solo se conociió un periodo antes de que
# ocurriera, entonces  per_anticip_interv = 1
prons_dlm_interv <- pronosticos_k_pasos(prons_F = prons_F_banxico, 
                                        k_int, 
                                        modelo = modelo_dlm_interv, 
                                        lista_interv = list_interv,
                                        per_anticip_interv =1)

df_prons_dlm_interv <- data.frame("fecha" = datos_efectivo$fecha[k_int:nrow(datos_F)], 
                                  "y_real" = datos_efectivo$efectivo[k_int:nrow(datos_F)], 
                                  "y_pronostico" = prons_dlm_interv$ft_k %>% unlist(), 
                                  "CI_inf" = prons_dlm_interv$CI_inf %>% unlist(),
                                  "CI_sup" = prons_dlm_interv$CI_sup %>% unlist()) %>% 
  mutate(fecha = as.numeric(fecha))


ggplot(data = df_prons_dlm_interv, aes(x = fecha)) +
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


residsk_interv <- df_prons_dlm_interv %>% 
  mutate(resids = y_real - y_pronostico) %>% 
  dplyr::select(resids)


mean(residsk_interv$resids)

var(df_prons_dlm_interv$y_pronostico)

mean(residsk_interv$resids^2)




