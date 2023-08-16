
library(ggplot2)
library(zoo)
library(gridExtra)
source('src/funciones/funciones_dlm.R')

datos_efectivo <- read_rds('cache/variables/efectivo.rds')

dlm_interv <- read_rds('cache/modelos/modelo_dlm_interv.rds')

#Pronosticos recopilados por Banxico en 2022 Q4
prons_F <- read_rds('cache/variables/prons_banxico.rds')[[45]]

modelo_prons <- pronosticos(variables_F = prons_F, per_futuros = 8, at_0 = dlm_interv$at[[44]],
            Rt_0 = dlm_interv$Rt[[44]], G = dlm_interv$G, W = dlm_interv$W, 
            St = dlm_interv$St[[44]], nt = dlm_interv$nT)


df_prons <- data.frame("fecha" = datos_efectivo$fecha[81:88] + 2, 
                            'efectivo' = NA,
                            "efectivo_prons" = exp(modelo_prons$ft_k %>% unlist()), 
                            "CI_inf" = exp(modelo_prons$CI_inf %>% unlist()),
                            "CI_sup" = exp(modelo_prons$CI_sup %>% unlist())) %>% 
  mutate(fecha = as.numeric(fecha))

df_completo <- datos_efectivo %>% 
  mutate(efectivo_prons=NA, CI_inf = NA, CI_sup = NA, fecha = as.numeric(fecha)) %>% 
  rbind(df_prons) %>% 
  filter(fecha >=2015)

df_completo

ggplot(data = df_completo, aes(x = fecha)) +
  geom_line(aes(y = efectivo, color = "Histórico"), size = 1) +
  geom_line(aes(y = efectivo_prons, color = 'Pronósticos'), size = 1, linetype = 'dashed') +
  geom_line(aes(y = CI_inf), color = "blue", alpha = 0.3) +
  geom_line(aes(y = CI_sup), color = "blue", alpha = 0.3) + 
  geom_ribbon(aes(ymax = CI_sup, ymin = CI_inf, fill = 'Intervalo al 95%'), alpha = 0.3) +
  theme_bw() +
  scale_colour_manual(
    name = "", values = c("Intervalo al 95%" = "transparent",
                          "Pronósticos" = "blue",
                          "Histórico" = 'black')) +
  scale_fill_manual(
    name = "",  values = c("Intervalo al 95%" = "blue",
                           "Pronósticos" = "transparent",
                           "Histórico" = 'transparent')) +
  theme(legend.position = "bottom") +
  labs(shape = "") +
  ylab('Circulación') +
  xlab('Fecha') +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.text = element_text(size=20)) +
  scale_x_yearqtr(format="%YT%q", n=5)

ggsave('graphs/modelos/dlm_interv/prons_futuros.png', width = 11.7, height = 6)  

# Para mayor comodidad, guardar pdf con pronosticos en carpeta docs

df_prons2 <- df_prons
df_prons2$fecha <- as.yearqtr(df_prons2$fecha, format = '%Y-Q%q')

pdf("docs/prons_futuros.pdf", height=11, width=10)
grid.table(df_prons2  %>% 
             dplyr::select(-efectivo) %>% 
             mutate_if(is.numeric, ~sprintf("%.2f",.)))
dev.off()
