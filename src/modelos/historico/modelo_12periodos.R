################################################################################
## Título: DLM M0 vs PIB, TIIE con dummy para Q4. 12 periodos

## Fecha: 24-02-2023
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
  filter(fecha >=2006.0, fecha <= 2008.75) %>% 
  mutate(pib = log(pib))



ggplot(datos_pib, aes(fecha, pib)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("PIB")



# Datos TIIE ----------------------------------------------------------

datos_tiie <- read_rds('cache/variables/last/tiie_last.rds') %>% 
  filter(fecha >=2006.0, fecha <= 2008.75) %>% 
  mutate(tiie = log(tiie))



ggplot(datos_tiie, aes(fecha, tiie)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("TIIE")


# Datos efectivo --------------------------------------------------------------

datos_efectivo <- read_rds('cache/variables/last/efectivo_last.rds') %>% 
  filter(fecha >=2006.0, fecha <= 2008.75) %>% 
  mutate(efectivo = log(efectivo))

ggplot(datos_efectivo, aes(fecha, efectivo)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("Efectivo")

cor(datos_efectivo$efectivo, datos_pib$pib)
cor(datos_efectivo$efectivo, datos_tiie$tiie)

pcor(data.frame(datos_efectivo$efectivo, datos_pib$pib, datos_tiie$tiie))


# Estacionalidad ------------------------------------------------------------

datos_estacionalidad <- datos_efectivo %>% 
  mutate(Q4 = ifelse(fecha - floor(fecha) == 0.75,1,0))


# datos_estacionalidad <- datos_efectivo %>% 
#   mutate(Q1 = ifelse(fecha - floor(fecha) == 0,1,0)) %>% 
#   mutate(Q2 = ifelse(fecha - floor(fecha) == 0.25,1,0)) %>% 
#   mutate(Q3 = ifelse(fecha - floor(fecha) == 0.5,1,0)) %>% 
#   mutate(Q4 = ifelse(fecha - floor(fecha) == 0.75,1,0))

################################################################################
## Modelo MARSS. 2006 Q1 - 2013 Q2
##
################################################################################

## number of periods of data
TT <- length(datos_efectivo$efectivo)
## get response variable: efectivo
dat <- matrix(datos_efectivo$efectivo, nrow = 1)

## get predictor variable
pib <- matrix(datos_pib$pib, nrow=1)
tiie <- matrix(datos_tiie$tiie, nrow=1)
#q1 <- matrix(datos_estacionalidad$Q1, nrow = 1)
#q2 <- matrix(datos_estacionalidad$Q2, nrow = 1)
#q3 <- matrix(datos_estacionalidad$Q3, nrow = 1)
q4 <- matrix(datos_estacionalidad$Q4, nrow = 1)
#inpc <- matrix(datos_inpc$inpc, nrow=1)
#fix <- matrix(datos_fix$fix, nrow = 1)
## number of regr params (slope + intercept)
m <- 4

## for process eqn
G <- matrix(list(0), m, m)
G[1,1] <- 'G.1'
G[2,2] <- 'G.2'
G[3,3] <- 'G.3'
G[4,4] <- 1
U <- matrix(0, nrow = m, ncol = 1) ## 2x1; both elements = 0
W <- matrix(list(0), m, m) ## 2x2; all 0 for now #Es Wt
#diag(W) <- c("w.1", "w.2", 'w.3','w.4') ## 2x2; diag = (q1,q2)
W[1,1] <- 'W.1'
W[2,2] <- 'W.2'
W[3,3] <- 'W.3'
W[4,4] <- 0


## for observation eqn
F <- array(NA, c(1, m, TT)) ## NxMxT; empty for now #Es Ft transpuesta
F[1, 1, ] <- rep(1, TT) ## Nx1; 1's for intercept
F[1, 2, ] <- pib ## Nx1; predictor variable
F[1, 3, ] <- tiie ## Nx1; predictor variable
#F[1, 4, ] <- q1 ## Nx1; predictor variable
#F[1, 5, ] <- q2 ## Nx1; predictor variable
#F[1, 6, ] <- q3 ## Nx1; predictor variable
F[1, 4, ] <- q4 ## Nx1; predictor variable
#F[1, 4, ] <- inpc ## Nx1; predictor variable
#F[1, 5, ] <- fix ## Nx1; predictor variable
A <- matrix(0) ## 1x1; scalar = 0
V <- matrix("v") ## 1x1; scalar = r # Es Vt
C0 <- matrix(list(0), m, m)
diag(C0) <- c('C0.1', 'C0.2', 'C0.3', 'C0.4')

## only need starting values for regr parameters
inits_list <- list(x0 = matrix(c(0, 0, 0, 0), nrow = m))
## list of model matrices & vectors
mod_list <- list(B = G, U = U, Q = W, Z = F, A = A, R = V)

## fit univariate DLM
dlm_1 <- MARSS(dat, inits = inits_list, model = mod_list, method = 'BFGS')

dlm_1$states
dlm_1$states.se
dlm_1$ytT

glance(dlm_1)
MARSSkfas(dlm_1)

## get list of Kalman filter output
kf_out <- MARSSkfss(dlm_1)
## forecasts of regr parameters; 2xT matrix
theta_priori <- kf_out$xtt1
## ts of E(forecasts)
one_step_forecast <- vector()
for (t in 1:TT) {
  #browser()
  one_step_forecast[t] <- F[, , t] %*% theta_priori[, t, drop = FALSE]
}

## variance of regr prior parameters; 1x2xT array
R <- kf_out$Vtt1
## obs variance; 1x1 matrix
V_est <- coef(dlm_1, type = "matrix")$R
## ts of Var(forecasts)
Q <- vector()
for (t in 1:TT) {
  tF <- matrix(F[, , t], m, 1) ## transpose of F
  Q[t] <- F[, , t] %*% R[, , t] %*% tF + V_est
}



df_result <- data.frame('fecha' = as.numeric(datos_efectivo$fecha), 'y_real' = datos_efectivo$efectivo, 
                        'y_pronostico' = one_step_forecast, 
                        'CI_inf' = qnorm(0.025, mean = one_step_forecast, sd = sqrt(Q)),
                        'CI_sup' = qnorm(0.975, mean = one_step_forecast, sd = sqrt(Q)))



ggplot(data = df_result, aes(x = fecha)) +
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



df_params <- data.frame('fecha' = as.numeric(datos_efectivo$fecha), 'intercept' = theta_priori[1,], 
                        'pib' = theta_priori[2,], 'tiie' = theta_priori[3,], 
                        'Q4' = theta_priori[4,]) %>% 
  pivot_longer(cols = c(intercept:Q4), names_to = 'parametro', values_to = 'valor')


ggplot(df_params, aes(x=fecha, y = valor)) +
  geom_line() +
  facet_wrap(~parametro, nrow = 4, scales = "free") +
  theme_bw()  +
  ylab("Valor") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        strip.text = element_text(size=20)) 




## forecast errors
innov <- kf_out$Innov

## Q-Q plot of innovations
qqnorm(t(innov), main = "", pch = 16, col = "blue")

## add y=x line for easier interpretation
qqline(t(innov))

## p-value for t-test of H0: E(innov) = 0
t.test(t(innov), mu = 0)$p.value
#>0.05, H0 cannot be rejected

mean(innov)

## plot ACF of innovations
acf(t(innov), lag.max = 36)

#Los errores anuales tienen una correlación alta!

plot(innov[1,], type = 'l')
abline(h=0)


################################################################################
## Modelo con mi funcion
##
################################################################################
source('src/funciones/funciones_dlm.R')

Vt <- V_est

Gt <- matrix(c(dlm_1$par$B[1], 0, 0, 0,
               0, dlm_1$par$B[2], 0, 0,
               0, 0, dlm_1$par$B[3], 0,
               0, 0, 0, 1), nrow=4)

Wt <- matrix(c(dlm_1$par$Q[1], 0, 0, 0,
               0, dlm_1$par$Q[2], 0, 0,
               0, 0, dlm_1$par$Q[3], 0,
               0, 0, 0, 0), nrow=4)

C0 <- solve(Gt) %*% (R[,,1] - Wt) %*% solve(t(Gt))

m0 <- c(dlm_1$par$x0[1], dlm_1$par$x0[2], dlm_1$par$x0[3], dlm_1$par$x0[4])

datos_F <- datos_pib %>% 
  left_join(datos_tiie) %>% 
  left_join(datos_estacionalidad) %>% 
  mutate(intercept = 1) %>% 
  dplyr::select(6,2,3,5)



dlm_2 <- actualizacion_dlm(y = datos_efectivo$efectivo, variables_F = datos_F, 
                           m0 = m0, C0 = C0, G = Gt, W = Wt, V = Vt, lista_interv = list())


df_graficas <- data.frame("fecha" = datos_efectivo %>% dplyr::select(fecha), 
                          "y_real" = datos_efectivo$efectivo, 
                          "y_pronostico" = dlm_2$ft %>% unlist(), "CI_inf" = dlm_2$CI_inf %>% unlist(),
                          "CI_sup" = dlm_2$CI_sup %>% unlist()) %>% 
  mutate(fecha = as.numeric(fecha))


ggplot(data = df_graficas, aes(x = fecha)) +
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




df_params2 <- data.frame(reduce(dlm_2$mt, cbind) %>% t(), fecha = df_graficas$fecha)  %>% 
  rename(Intercepto = X1, PIB = X2, TIIE = X3, Q4 = X4) %>% 
  pivot_longer(names_to = "parametro", values_to = "valor", 
               cols = c(Intercepto, PIB, TIIE, Q4))

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



# Parametros iniciales para 2006 Q1-2008 Q4 ---------------------------------------

#DLM 1
kf_out$xtt[,12]

kf_out$Vtt[,,12]

kf_out$xtt

#DLM 2

dlm_2$mt[[12]] %>% 
  write_rds('cache/outputs_modelos/12_periodos/m0.rds')

dlm_2$Ct[[12]] %>% 
  write_rds('cache/outputs_modelos/12_periodos/C0.rds')

Vt %>% 
  write_rds('cache/outputs_modelos/12_periodos/V.rds')

dlm_1$par$x0 %>% 
  write_rds('cache/outputs_modelos/12_periodos/m0_hist.rds')

dlm_2$mt %>% 
  write_rds('cache/outputs_modelos/12_periodos/m1_m12.rds')

dlm_2$Ct %>% 
  write_rds('cache/outputs_modelos/12_periodos/C1_C12.rds')

# Gt %>% 
#   write_rds('cache/outputs_modelos/G.rds')
# 
# Wt %>% 
#   write_rds('cache/outputs_modelos/W.rds')

