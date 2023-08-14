################################################################################
## Crea y guarda gráficas en 'graphs/modelos/vecm'
##
################################################################################

library(tidyverse)
library(zoo)


for(k in 1:8){
  
  df_prons_vecm <- read_rds(paste0('cache/resultados/vecm/vecm_prons_',k,'_pasos.rds'))
  
  ggplot(data = df_prons_vecm, aes(x = fecha)) +
    geom_point(aes(y = y_real, shape = "Observaciones"), size = 2) +
    geom_line(aes(y = y_pronostico, color = 'Pronósticos'), size = 1) +
    geom_line(aes(y = CI_inf), color = "forestgreen", alpha = 0.3) +
    geom_line(aes(y = CI_sup), color = "forestgreen", alpha = 0.3) + 
    geom_ribbon(aes(ymax = CI_sup, ymin = CI_inf, fill = 'Intervalo al 95%'), alpha = 0.3) +
    theme_bw() +
    scale_colour_manual(
      name = "", values = c("Intervalo al 95%" = "transparent",
                            "Pronósticos" = "forestgreen")) +
    scale_fill_manual(
      name = "",  values = c("Intervalo al 95%" = "forestgreen",
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
  
  ggsave(paste0('graphs/modelos/vecm/vecm_prons_',k,'_pasos.png'))
  
}
