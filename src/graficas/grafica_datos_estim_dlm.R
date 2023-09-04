
################################################################################
## Muestra datos usados para estimación y para DLM
##
################################################################################

library(tidyverse)
library(zoo)



datos_efectivo <- read_rds('cache/variables/efectivo.rds') %>% 
  mutate(estimacion = ifelse(fecha < 2012, efectivo, NA)) %>% 
  mutate(dlm = ifelse(fecha >= 2012, efectivo, NA))


ggplot(datos_efectivo, aes(x=fecha)) +
  geom_line(aes(y=estimacion, color = 'Estimación'), size = 1) +
  geom_line(aes(y=dlm, color = 'DLM'), size = 1) +
  theme_bw() +
  scale_colour_manual(
    name = "", values = c("Estimación" = "red",
                          "DLM" = 'blue')) +
  theme(legend.position = "bottom") +
  labs(shape = "") +
  ylab('Circulación') +
  xlab('Fecha') +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.text = element_text(size=20)) +
  scale_x_yearqtr(format="%YT%q", n=5)

ggsave('graphs/otros/datos_estim_dlm.pdf', width = 11.7, height = 6)  
