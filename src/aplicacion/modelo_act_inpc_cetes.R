################################################################################
## Título: DLM M0 vs actividad, inpc y cetes 182d

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
  filter(fecha <= 2010.75) %>% 
  mutate(pib = log(pib))




# Datos Cetes 28d ----------------------------------------------------------

datos_cetes_28d <- read_rds('cache/variables/cetes_28d_trim.rds') %>% 
  filter(fecha <= 2010.75) %>% 
  mutate(cetes_28d = log(cetes_28d))


# Datos INPC ----------------------------------------------------------

datos_inpc <- read_rds('cache/variables/inpc_trim.rds') %>% 
  filter(fecha <= 2010.75) %>% 
  mutate(inpc = log(inpc))



# FIX -----------------------------------------------------------------------

datos_fix <- read_rds('cache/variables/fix_trim.rds') %>% 
  filter(fecha <= 2010.75) %>% 
  mutate(fix = log(fix))


# Datos efectivo --------------------------------------------------------------

datos_efectivo <- read_rds('cache/variables/efectivo_trim.rds') %>% 
  filter(fecha <= 2010.75) %>% 
  mutate(efectivo = log(efectivo))


cor(datos_efectivo$efectivo, datos_pib$pib)
cor(datos_efectivo$efectivo, datos_cetes_28d$cetes_28d)
cor(datos_efectivo$efectivo, datos_fix$fix)
cor(datos_efectivo$efectivo, datos_inpc$inpc)

pcor(data.frame(datos_efectivo$efectivo, datos_pib$pib, datos_cetes_28d$cetes_28d, 
                datos_fix$fix, datos_inpc$inpc))

pcor(data.frame(datos_efectivo$efectivo, datos_pib$pib, datos_cetes_28d$cetes_28d))

pcor(data.frame(datos_efectivo$efectivo, datos_pib$pib, datos_fix$fix))

pcor(data.frame(datos_efectivo$efectivo, datos_pib$pib, datos_inpc$inpc))

################################################################################
## Modelo MARSS
##
################################################################################

## number of periods of data
TT <- length(datos_efectivo$efectivo)
## get response variable: efectivo
dat <- matrix(datos_efectivo$efectivo, nrow = 1)

## get predictor variable
pib <- matrix(datos_pib$pib, nrow=1)
cetes_28d <- matrix(datos_cetes_28d$cetes_28d, nrow=1)
inpc <- matrix(datos_inpc$inpc, nrow=1)
#fix <- matrix(datos_fix$fix, nrow = 1)
## number of regr params (slope + intercept)
m <- 4

## for process eqn
G <- diag(m) ## 2x2; Identity #Es Gt
G <- matrix(c(0.99,0,0,0,
              0,1,0,0,
              0,0,1,0,
              0,0,0,1), nrow=4)
G <- matrix(list(0), m, m)
diag(G) <- c('G.1', 'G.2', 'G.3', 'G.4')
U <- matrix(0, nrow = m, ncol = 1) ## 2x1; both elements = 0
W <- matrix(list(0), m, m) ## 2x2; all 0 for now #Es Wt
diag(W) <- c("w.1", "w.2", 'w.3', 'w.4') ## 2x2; diag = (q1,q2)

## for observation eqn
F <- array(NA, c(1, m, TT)) ## NxMxT; empty for now #Es Ft transpuesta
F[1, 1, ] <- rep(1, TT) ## Nx1; 1's for intercept
F[1, 2, ] <- pib ## Nx1; predictor variable
F[1, 3, ] <- cetes_28d ## Nx1; predictor variable
F[1, 4, ] <- inpc ## Nx1; predictor variable
#F[1, 5, ] <- fix ## Nx1; predictor variable
A <- matrix(0) ## 1x1; scalar = 0
V <- matrix("v") ## 1x1; scalar = r # Es Vt

## only need starting values for regr parameters
inits_list <- list(x0 = matrix(c(0, 0, -0.5, 0), nrow = m))
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
                        'pib' = theta_priori[2,], 'cetes_28d' = theta_priori[3,],
                        'inpc' = theta_priori[4,]) %>% 
  pivot_longer(cols = c(intercept:inpc), names_to = 'parametro', values_to = 'valor')


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
#>0.05, H0 cannot be rejected

mean(innov)

## plot ACF of innovations
acf(t(innov), lag.max = 36)

#Los errores anuales tienen una correlación alta!

plot(innov[1,], type = 'l')
abline(h=0)
