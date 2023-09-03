################################################################################
## Crea y guarda gráficas en 'graphs/modelos/dlm_interv'
##
################################################################################

library(tidyverse)
library(zoo)
library(latex2exp)

modelo_dlm_interv <- read_rds('cache/modelos/modelo_dlm_interv.rds')

datos_efectivo <- read_rds('cache/variables/efectivo.rds') %>% 
  filter(fecha > 2011.75) %>% 
  mutate(efectivo = log(efectivo))


df_interv_params <- data.frame(reduce(modelo_dlm_interv$mt, cbind) %>% t(), 
                               fecha = datos_efectivo %>% dplyr::select(fecha))  %>% 
  rename(Intercepto = X1, PIB = X2,  Inflación = X3, `Trimestre en turno` = X4) %>% 
  pivot_longer(names_to = "parametro", values_to = "valor", 
               cols = c(Intercepto, PIB, Inflación, `Trimestre en turno`)) %>% 
  mutate(across(parametro, factor, levels=c("Intercepto","PIB","Inflación", "Trimestre en turno")))

ggplot(df_interv_params, aes(x=fecha, y = valor)) +
  geom_line() +
  facet_wrap(~parametro, nrow = 4, scales = "free") +
  theme_bw()  +
  ylab(TeX("Valores de $\\textbf{m}_t$")) +
  xlab("Fecha") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        strip.text = element_text(size=20)) +
  scale_x_yearqtr(format="%YT%q", n=5)

ggsave(filename = "graphs/modelos/dlm_interv/dlm_interv_params.png", width = 11.7, height = 10)

df_St_interv <- data.frame(modelo_dlm_interv$St %>% unlist(), 
                           fecha = datos_efectivo %>% dplyr::select(fecha)) %>% 
  dplyr::rename(St = 1)

ggplot(df_St_interv, aes(x=fecha, y = St)) +
  geom_line(size = 1) +
  theme_bw()  +
  ylab(TeX("$S_t$")) +
  xlab("Fecha") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.text = element_text(size=20)) +
  ylim(c(2e-06, 3.5e-06)) +
  scale_x_yearqtr(format="%YT%q", n=5)

ggsave(filename = "graphs/modelos/dlm_interv/dlm_interv_St.png", width = 11.7, height = 6)


# Pronósticos ---------------------------------------------------------------

for(k in 1:8){
  
  df_prons_dlm_interv <- read_rds(paste0('cache/resultados/dlm_interv/dlm_interv_prons_',k,'_pasos.rds'))
  
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
          legend.text = element_text(size=20)) +
    scale_x_yearqtr(format="%YT%q", n=5)
  
  ggsave(paste0('graphs/modelos/dlm_interv/pronosticos/dlm_interv_prons_',k,'_pasos.png'))
  
}
