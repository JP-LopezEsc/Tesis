################################################################################
## Título: Dinero durante COVID-19. Introducción

## Descripción: Se grafica el comportamiento del dinero en la pandemia

## Fecha: 20-04-2022
################################################################################

library(tidyverse)
Sys.setlocale(locale = "es_ES.UTF-8")
################################################################################
## Serie de tiempo del dinero
##
################################################################################

datos_dinero <- readxl::read_excel("datos/billetes_monedas_publico.xlsx", 
                                   range = "A18:B277") %>% 
  rename(fecha = 1, dinero = 2) %>% 
  mutate(fecha = as.Date(fecha), dinero = dinero/1000000) %>% 
  filter(fecha > '2009-06-01')
  

ggplot(datos_dinero, aes(fecha, dinero)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y", 
               limits = c(datos_dinero$fecha[1], 
                          datos_dinero$fecha[nrow(datos_dinero)]),
               expand = c(0.02,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22)) +
  xlab("Año") +
  ylab("Circulación")

ggsave(filename = "graphs/introduccion/1.1_dinero.png", width = 11.7, height = 6)
