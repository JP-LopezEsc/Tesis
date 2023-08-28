################################################################################
## Crea y guarda gráficas en 'graphs/modelos/dlm'
##
################################################################################

library(tidyverse)
library(zoo)

datos_efectivo <- read_rds('cache/variables/efectivo.rds') %>% 
  filter(fecha > 2011.75) 


modelo_dlm <- read_rds('cache/modelos/modelo_dlm.rds')

df_params <- data.frame(reduce(modelo_dlm$mt, cbind) %>% t(), 
                        fecha = datos_efectivo %>% dplyr::select(fecha))  %>% 
  rename(Intercepto = X1, PIB = X2,  Inflación = X3, `Trimestre en turno` = X4) %>% 
  pivot_longer(names_to = "parametro", values_to = "valor", 
               cols = c(Intercepto, PIB, Inflación, `Trimestre en turno`)) %>% 
  mutate(across(parametro, factor, levels=c("Intercepto","PIB","Inflación", "Trimestre en turno")))

ggplot(df_params, aes(x=fecha, y = valor)) +
  geom_line() +
  facet_wrap(~parametro, nrow = 4, scales = "free") +
  theme_bw()  +
  ylab("Valor esperado") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        strip.text = element_text(size=20)) +
  scale_x_yearqtr(format="%YT%q", n=5)

ggsave(filename = "graphs/modelos/dlm/dlm_params.png", width = 11.7, height = 10)


df_St <- data.frame(modelo_dlm$St %>% unlist(), 
                    fecha = datos_efectivo %>% dplyr::select(fecha)) %>% 
  dplyr::rename(St = 1)

ggplot(df_St, aes(x=fecha, y = St)) +
  geom_line(size = 1) +
  theme_bw()  +
  ylab("St") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.text = element_text(size=20)) +
  scale_x_yearqtr(format="%YT%q", n=5)


ggsave(filename = "graphs/modelos/dlm/dlm_St.png", width = 11.7, height = 6)



# Pronósticos ---------------------------------------------------------------

for(k in 1:8){
  
  df_prons_dlm <- read_rds(paste0('cache/resultados/dlm/dlm_prons_',k,'_pasos.rds'))
  
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
          legend.text = element_text(size=20)) +
    scale_x_yearqtr(format="%YT%q", n=5)
  
  ggsave(paste0('graphs/modelos/dlm/pronosticos/dlm_prons_',k,'_pasos.png'))
  
}
