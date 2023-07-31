################################################################################
## Título: DLM M0 vs PIB, INPC trim, estacionalidad. 

## Se toman datos de 2001 Q1 a 2011 Q4 para estimar valores iniciales para
## correr el DLM en el periodo 2012 Q1 a 2022 Q4. Se usa el paquete MARSS

################################################################################

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
  filter(fecha <= 2011.75) %>% 
  mutate(pib = log(pib))



ggplot(datos_pib, aes(fecha, pib)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("PIB")


datos_inpc <- read_rds('cache/variables/inpc_trim.rds') %>% 
  filter(fecha <= 2011.75) 



ggplot(datos_inpc, aes(fecha, inpc)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("INPC")


datos_efectivo <- read_rds('cache/variables/efectivo.rds') %>% 
  filter(fecha <= 2011.75) %>% 
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


################################################################################
## Modelo MARSS. 2001 Q1 - 2011 Q4
##
################################################################################

## número de periodos
TT <- length(datos_efectivo$efectivo)
## Variable de respuesta: Efectivo
y <- matrix(datos_efectivo$efectivo, nrow = 1)

## Variables explicativas
pib <- matrix(datos_pib$pib, nrow=1)
inpc <- matrix(datos_inpc$inpc, nrow=1)
q1 <- matrix(datos_estacionalidad$Q1, nrow = 1)
q2 <- matrix(datos_estacionalidad$Q2, nrow = 1)
q3 <- matrix(datos_estacionalidad$Q3, nrow = 1)
q4 <- matrix(datos_estacionalidad$Q4, nrow = 1)
## número de parámetros + intercepto
m <- 7

## G es la matriz de evolución
## Solo se estimarán los valores asociados al intercept, PIB y INPC.
# Los de la estacionalidad son 1
G <- matrix(list(0), m, m)
G[1,1] <- 'G.1'
G[2,2] <- 'G.2'
G[3,3] <- 'G.3'
G[4,5] <- 1
G[5,6] <- 1
G[6,7] <-1
G[7,4] <- 1


# U es parte de MARSS, pero no se necesita en el DLM, es 0
U <- matrix(0, nrow = m, ncol = 1)

# W es la varianza de evolución. Se estiman los del intercepto, PIB, INPC y el
# trimestre en turno para la estacionalidad
W <- matrix(list(0), m, m) ## 2x2; all 0 for now #Es Wt
W[1,1] <- 'W.1'
W[2,2] <- 'W.2'
W[3,3] <- 'W.3'
W[4,4] <- 'W.4'


## Se juntan todas las variables explicativas en F
F <- array(NA, c(1, m, TT)) 
F[1, 1, ] <- rep(1, TT) ## intercept
F[1, 2, ] <- pib 
F[1, 3, ] <- inpc 
F[1, 4, ] <- q1 
F[1, 5, ] <- q2 
F[1, 6, ] <- q3 
F[1, 7, ] <- q4 

# A es parte de MARSS, pero no se necesita en el DLM, es 0
A <- matrix(0) ## 1x1; scalar = 0

# V es la varianza de observación
V <- matrix("v") 


## valores iniciales para la estimación
val_inicial <- list(x0 = matrix(c(0, 0, 0,0,0,0,0), nrow = m))

## lista de matrices y vectores definidos anteriormente
matriz_list <- list(B = G, U = U, Q = W, Z = F, A = A, R = V)

## Ajusta DLM
dlm_marss <- MARSS(y, inits = val_inicial, model = matriz_list, method = 'BFGS')

dlm_marss$states


## Salidas del filtro de Kalman
kf_salidas <- MARSSkfss(dlm_marss)

## Errores de pronostico
residuales <- kf_salidas$Innov

## Q-Q plot de residuos
qqnorm(t(residuales), main = "", pch = 16, col = "blue")
qqline(t(residuales))

## p-value para t-test de H0: E(residuales) = 0
t.test(t(residuales), mu = 0)$p.value
#>0.05, H0 cannot be rejected

mean(residuales)

## ACF de residuales
acf(t(residuales), lag.max = 36)


plot(residuales[1,], type = 'l')
abline(h=0)


################################################################################
## Modelo con mi funcion
## Se usan los valores estimados con MARSS y se aplica mi función de dlm
################################################################################

source('src/funciones/funciones_dlm.R')

Vt <- coef(dlm_marss, type = "matrix")$R

Gt <- matrix(c(dlm_marss$par$B[1], 0, 0, 0,0,0,0,
               0, dlm_marss$par$B[2], 0, 0,0,0,0,
               0, 0, dlm_marss$par$B[3],0,0,0,0,
               0,0,0,0,0,0,1,
               0,0,0,1,0,0,0,
               0,0,0,0,1,0,0,
               0,0,0,0,0,1,0), nrow=7)

Wt <- matrix(c(dlm_marss$par$Q[1], 0, 0,0,0,0,0,
               0, dlm_marss$par$Q[2], 0,0,0,0,0,
               0, 0, dlm_marss$par$Q[3],0,0,0,0,
               0,0,0,dlm_marss$par$Q[4],0,0,0,
               0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,
               0,0,0,0,0,0,0), nrow=7)

# Por default MARSS define el equivalente de C0 como una matriz de 0s, entonces trata
# m0 como valores estimados y no como variable aleatoria. Para periodos posteriores
# Ct tendrá valores distintos a 0.

C0 <- matrix(0, nrow = 7, ncol = 7)


m0 <- c(dlm_marss$par$x0[1], 
        dlm_marss$par$x0[2], 
        dlm_marss$par$x0[3],
        dlm_marss$par$x0[4], 
        dlm_marss$par$x0[5], 
        dlm_marss$par$x0[6], 
        dlm_marss$par$x0[7])

datos_F <- datos_pib %>% 
  left_join(datos_estacionalidad) %>% 
  left_join(datos_inpc) %>% 
  mutate(intercept = 1) %>% 
  dplyr::select(9,2, 8, 4:7)



dlm_2 <- actualizacion_dlm(y = datos_efectivo$efectivo, variables_F = datos_F, 
                           m0 = m0, C0 = C0, G = Gt, W = Wt, V = Vt, lista_interv = list())


df_grafica <- data.frame("fecha" = datos_efectivo %>% dplyr::select(fecha), 
                          "y_real" = datos_efectivo$efectivo, 
                          "y_pronostico" = dlm_2$ft %>% unlist(), "CI_inf" = dlm_2$CI_inf %>% unlist(),
                          "CI_sup" = dlm_2$CI_sup %>% unlist()) %>% 
  mutate(fecha = as.numeric(fecha))


ggplot(data = df_grafica, aes(x = fecha)) +
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




df_params2 <- data.frame(reduce(dlm_2$mt, cbind) %>% t(), fecha = df_grafica$fecha)  %>% 
  rename(Intercepto = X1, PIB = X2, INPC = X3, Q1 = X4) %>% 
  pivot_longer(names_to = "parametro", values_to = "valor", 
               cols = c(Intercepto, PIB, INPC, Q1))

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



#  Se guardan parámetros inicales del periodo 2012 Q1 - 2022 Q4

#DLM MARSS
kf_salidas$xtt[,44]

kf_salidas$Vtt[,,44]

kf_salidas$xtt

#DLM 2

dlm_2$mt[[44]] %>% 
  write_rds('cache/output_modelo_historico/m0.rds')

dlm_2$Ct[[44]] %>% 
  write_rds('cache/output_modelo_historico/C0.rds')

Vt %>% 
  write_rds('cache/output_modelo_historico/V.rds')

Gt %>% 
  write_rds('cache/output_modelo_historico/G.rds')
# 
Wt %>% 
  write_rds('cache/output_modelo_historico/W.rds')

