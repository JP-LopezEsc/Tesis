################################################################################
## Título: DLM M0 vs cetes_182d

## Fecha: 24-02-2023
################################################################################

library(tidyverse)
library(janitor)
library(lubridate)
library('MARSS')
library('broom')
Sys.setlocale(locale = "es_ES.UTF-8")

################################################################################
## Datos
##
################################################################################

# Datos Cetes 182d ----------------------------------------------------------

datos_cetes_182d <- read_rds('datos/variables/cetes_182d.rds')

ggplot(datos_cetes_182d, aes(fecha, cetes_182d)) +
  geom_line() +
  geom_vline(xintercept = as.Date('2020-03-01'), color = 'red')+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y", 
               limits = c(datos_cetes_182d$fecha[1], 
                          datos_cetes_182d$fecha[nrow(datos_cetes_182d)]),
               expand = c(0.02,0)) +
  theme_classic() +
  xlab("Año") +
  ylab("Rendimiento")

# Datos efectivo --------------------------------------------------------------

datos_efectivo <- read_rds('datos/variables/efectivo.rds')

#En teoría la correlación tendría que ser negativa
cor(datos_efectivo$efectivo, datos_cetes_182d$cetes_182d)


ggplot(data = datos_efectivo, 
       mapping = aes(x = month(fecha, label = TRUE), 
                     y = efectivo))+
  geom_point()+
  geom_line(aes(group = factor(year(fecha)),
                color = factor(year(fecha))))+
  scale_color_discrete(name = "Year")+
  xlab(label = "Month")

################################################################################
## Modelo 1: Actividad
##
################################################################################

## number of periods of data
TT <- length(datos_efectivo$efectivo)
## get response variable: efectivo
dat <- matrix(datos_efectivo$efectivo, nrow = 1)

## get predictor variable
Cetes_182d <- matrix(datos_cetes_182d$cetes_182d, nrow=1)
## number of regr params (slope + intercept)
m <- dim(Cetes_182d)[1] + 1

## for process eqn
G <- diag(m) ## 2x2; Identity #Es Gt
U <- matrix(0, nrow = m, ncol = 1) ## 2x1; both elements = 0
W <- matrix(list(0), m, m) ## 2x2; all 0 for now #Es Wt
diag(W) <- c("w.1", "w.2") ## 2x2; diag = (q1,q2)

## for observation eqn
F <- array(NA, c(1, m, TT)) ## NxMxT; empty for now #Es Ft transpuesta
F[1, 1, ] <- rep(1, TT) ## Nx1; 1's for intercept
F[1, 2, ] <- Cetes_182d ## Nx1; predictor variable
A <- matrix(0) ## 1x1; scalar = 0
V <- matrix("v") ## 1x1; scalar = r # Es Vt

## only need starting values for regr parameters
inits_list <- list(x0 = matrix(c(0, -5), nrow = m))
## list of model matrices & vectors
mod_list <- list(B = G, U = U, Q = W, Z = F, A = A, R = V)

## fit univariate DLM
dlm_1 <- MARSS(dat, inits = inits_list, model = mod_list, control = list(maxit=1000))

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



df_result <- data.frame('fecha' = datos_efectivo$fecha, 'efectivo' = datos_efectivo$efectivo, 
                        'one_step_Forcast' = one_step_forecast, 'varianza', V_est)


ggplot(data = df_result, aes(x = fecha)) +
  geom_line(aes(y = efectivo, color = 'Efectivo'), size = 1) +
  geom_line(aes(y = one_step_forecast, color = 'Pronósticos'), size = 1) +
  theme_bw() +
  theme(legend.position = "bottom") +
  ylab('Circulación') +
  xlab('Fecha') +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.text = element_text(size=20)) 


df_params <- data.frame('fecha' = datos_efectivo$fecha, 'intercept' = theta_priori[1,], 
                        'cetes_182d' = theta_priori[2,]) %>% 
  pivot_longer(cols = c(intercept, cetes_182d), names_to = 'parametro', values_to = 'valor')


ggplot(df_params, aes(x=fecha, y = valor)) +
  geom_line() +
  facet_wrap(~parametro, nrow = 3, scales = "free") +
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
#p<0.05, H0 is rejected

mean(innov)

## plot ACF of innovations
acf(t(innov), lag.max = 10)
