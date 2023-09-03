
library(tidyverse)
library(zoo)
library(latex2exp)

datos_efectivo <- read_rds('cache/variables/efectivo.rds') %>% 
  filter(fecha > 2011.75) 

# DLM -----------------------------------------------------------------------


df_dlm_suav <- read_rds('cache/resultados/dlm/dlm_suavizamiento.rds')

ggplot(data = df_dlm_suav, aes(x = fecha)) +
  geom_point(aes(y = y_real, shape = "Observaciones"), size = 2) +
  geom_line(aes(y = y_suav, color = 'Suavizamiento'), size = 1) +
  geom_line(aes(y = CI_inf), color = "blue", alpha = 0.3) +
  geom_line(aes(y = CI_sup), color = "blue", alpha = 0.3) + 
  geom_ribbon(aes(ymax = CI_sup, ymin = CI_inf, fill = 'Intervalo al 95%'), alpha = 0.3) +
  theme_bw() +
  scale_colour_manual(
    name = "", values = c("Intervalo al 95%" = "transparent",
                          "Suavizamiento" = "black")) +
  scale_fill_manual(
    name = "",  values = c("Intervalo al 95%" = "blue",
                           "Suavizamiento" = "transparent")) +
  theme(legend.position = "bottom") +
  labs(shape = "") +
  ylab('Circulación') +
  xlab('Fecha') +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.text = element_text(size=20)) +
  scale_x_yearqtr(format="%YT%q", n=5)

ggsave('graphs/modelos/dlm/dlm_suavizado.png',  width = 11.7, height = 6)


dlm_suav <- read_rds('cache/modelos/modelo_dlm_suavizado.rds')

df_dlm_params <- data.frame(reduce(dlm_suav$at_k_filt, cbind) %>% t(), 
                        fecha = datos_efectivo %>% dplyr::select(fecha))  %>% 
  rename(Intercepto = X1, PIB = X2,  Inflación = X3, `Trimestre en turno` = X4) %>% 
  pivot_longer(names_to = "parametro", values_to = "valor", 
               cols = c(Intercepto, PIB, Inflación, `Trimestre en turno`)) %>% 
  mutate(across(parametro, factor, levels=c("Intercepto","PIB","Inflación", "Trimestre en turno")))

ggplot(df_dlm_params, aes(x=fecha, y = valor)) +
  geom_line() +
  facet_wrap(~parametro, nrow = 4, scales = "free") +
  theme_bw()  +
  ylab(TeX("Valores de $\\textbf{a}_{44}(-k)$")) +
  xlab("Fecha") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        strip.text = element_text(size=20)) +
  scale_x_yearqtr(format="%YT%q", n=5)


ggsave('graphs/modelos/dlm/dlm_params_suavizado.png',  width = 11.7, height = 10)




# DLM interv ----------------------------------------------------------------

df_dlm_interv_suav <- read_rds('cache/resultados/dlm_interv/dlm_interv_suavizamiento.rds')

ggplot(data = df_dlm_interv_suav, aes(x = fecha)) +
  geom_point(aes(y = y_real, shape = "Observaciones"), size = 2) +
  geom_line(aes(y = y_suav, color = 'Suavizamiento'), size = 1) +
  geom_line(aes(y = CI_inf), color = "blue", alpha = 0.3) +
  geom_line(aes(y = CI_sup), color = "blue", alpha = 0.3) + 
  geom_ribbon(aes(ymax = CI_sup, ymin = CI_inf, fill = 'Intervalo al 95%'), alpha = 0.3) +
  theme_bw() +
  scale_colour_manual(
    name = "", values = c("Intervalo al 95%" = "transparent",
                          "Suavizamiento" = "black")) +
  scale_fill_manual(
    name = "",  values = c("Intervalo al 95%" = "blue",
                           "Suavizamiento" = "transparent")) +
  theme(legend.position = "bottom") +
  labs(shape = "") +
  ylab('Circulación') +
  xlab('Fecha') +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.text = element_text(size=20)) +
  scale_x_yearqtr(format="%YT%q", n=5)

ggsave('graphs/modelos/dlm_interv/dlm_interv_suavizado.png',  width = 11.7, height = 6)


dlm_interv_suav <- read_rds('cache/modelos/modelo_dlm_interv_suavizado.rds')

df_dlm_interv_params <- data.frame(reduce(dlm_interv_suav$at_k_filt, cbind) %>% t(), 
                            fecha = datos_efectivo %>% dplyr::select(fecha))  %>% 
  rename(Intercepto = X1, PIB = X2,  Inflación = X3, `Trimestre en turno` = X4) %>% 
  pivot_longer(names_to = "parametro", values_to = "valor", 
               cols = c(Intercepto, PIB, Inflación, `Trimestre en turno`)) %>% 
  mutate(across(parametro, factor, levels=c("Intercepto","PIB","Inflación", "Trimestre en turno")))

ggplot(df_dlm_interv_params, aes(x=fecha, y = valor)) +
  geom_line() +
  facet_wrap(~parametro, nrow = 4, scales = "free") +
  theme_bw()  +
  ylab(TeX("Valores de $\\textbf{a}_{44}(-k)$")) +
  xlab("Fecha") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        strip.text = element_text(size=20)) +
  scale_x_yearqtr(format="%YT%q", n=5)


ggsave('graphs/modelos/dlm_interv/dlm_interv_params_suavizado.png',  width = 11.7, height = 10)
 