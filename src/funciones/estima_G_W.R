
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
  filter(fecha >=2006.0) %>% 
  mutate(pib = log(pib))



ggplot(datos_pib, aes(fecha, pib)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("PIB")



# Datos TIIE ----------------------------------------------------------

datos_tiie <- read_rds('cache/variables/last/tiie_last.rds') %>% 
  filter(fecha >=2006.0) %>% 
  mutate(tiie = log(tiie))



ggplot(datos_tiie, aes(fecha, tiie)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("TIIE")


# Datos efectivo --------------------------------------------------------------

datos_efectivo <- read_rds('cache/variables/last/efectivo_last.rds') %>% 
  filter(fecha >=2006.0) %>% 
  mutate(efectivo = log(efectivo))

ggplot(datos_efectivo, aes(fecha, efectivo)) +
  geom_line() +
  theme_classic() +
  xlab("Año") +
  ylab("Efectivo")



# Estacionalidad ------------------------------------------------------------

datos_estacionalidad <- datos_efectivo %>% 
  mutate(Q4 = ifelse(fecha - floor(fecha) == 0.75,1,0))


## number of periods of data
TT <- length(datos_efectivo$efectivo)


## get predictor variable
pib <- matrix(datos_pib$pib, nrow=1)
tiie <- matrix(datos_tiie$tiie, nrow=1)
q4 <- matrix(datos_estacionalidad$Q4, nrow = 1)

## number of regr params (slope + intercept)
m <- 4

## for process eqn
G <- matrix(list(0), m, m)
diag(G) <- c('G.1', 'G.2', 'G.3', 'G.4')
W <- matrix(list(0), m, m) ## 2x2; all 0 for now #Es Wt
diag(W) <- c("w.1", "w.2", 'w.3','w.4') ## 2x2; diag = (q1,q2)



## for observation eqn
F <- array(NA, c(1, m, TT)) ## NxMxT; empty for now #Es Ft transpuesta
F[1, 1, ] <- rep(1, TT) ## Nx1; 1's for intercept
F[1, 2, ] <- pib ## Nx1; predictor variable
F[1, 3, ] <- tiie ## Nx1; predictor variable
F[1, 4, ] <- q4 ## Nx1; predictor variable
V <- matrix("v") ## 1x1; scalar = r # Es Vt


estimacion_MARSS <- function(y, F, inicio, G, W, V){
  
  G_estimadas <- list()
  W_estimadas <- list()
  
  U <- matrix(0, nrow = m, ncol = 1) ## 2x1; both elements = 0
  A <- matrix(0) ## 1x1; scalar = 0
  
  for(i in inicio:length(y)){
    y_iter <- matrix(y[1:i], nrow = 1)
    mod_list <- list(B = G, U = U, Q = W, Z = array(F[1,,1:i], c(1,m,length(F[1,1,1:i]))), 
                     A = A, R = V, x0 = read_rds('cache/outputs_modelos/m0.rds'))
    dlm <- MARSS(y_iter, model = mod_list, method = 'BFGS')
    
    G_salida <-  matrix(list(0), m, m)
    diag(G_salida) <- dlm$par$B
    
    W_salida <-  matrix(list(0), m, m)
    diag(W_salida) <- dlm$par$Q
    
    G_estimadas[[i]] <- G_salida
    W_estimadas[[i]] <- W_salida
  }

  return(list('G_estimadas' = G_estimadas, 'W_estimadas' = W_estimadas))
  
}


dim(array(F[1,,1:25], c(1,m,length(F[1,1,1:25]))))
array(F[1,,1:25], c(1,m,length(F[1,1,1:25])))
F

## get response variable: efectivo
dat <- matrix(datos_efectivo$efectivo[1:25], nrow = 1)

U <- matrix(0, nrow = m, ncol = 1) ## 2x1; both elements = 0
A <- matrix(0) ## 1x1; scalar = 0

mod_list <- list(B = G, U = U, Q = W, Z = array(F[1,,1:25], c(1,m,length(F[1,1,1:25]))), 
                 A = A, R = V, x0 = read_rds('cache/outputs_modelos/m0.rds'))
dlm <- MARSS(dat, model = mod_list, method = 'BFGS')

G_prueba <-  matrix(list(0), m, m)
diag(G_prueba) <- dlm$par$B

dat[,1:4]

estimaciones <- estimacion_MARSS(datos_efectivo$efectivo, F,15, G, W, matrix(1.98e-05))

estimaciones$G_estimadas


################################################################################
## Funcion para solo un periodo
##
################################################################################

dat <- matrix(datos_efectivo$efectivo, nrow = 1)

estimacion_MARSS <- function(y, F, inicio, periodos_extra, G, W, V){
  
  U <- matrix(0, nrow = m, ncol = 1) ## 2x1; both elements = 0
  A <- matrix(0) ## 1x1; scalar = 0
  
  y_iter <- matrix(y[1:inicio + periodos_extra], nrow = 1)
  mod_list <- list(B = G, U = U, Q = W, 
                   Z = array(F[1,,1:inicio + periodos_extra], 
                             c(1,m,length(F[1,1,1:inicio + periodos_extra]))), 
                   A = A, R = V, x0 = read_rds('cache/outputs_modelos/m0_hist.rds'))
  
  dlm <- MARSS(y_iter, model = mod_list, method = 'BFGS')
  
  G_salida <-  matrix(list(0), m, m)
  diag(G_salida) <- dlm$par$B
  mode(G_salida) <- 'numeric'
  
  W_salida <-  matrix(list(0), m, m)
  diag(W_salida) <- dlm$par$Q
  mode(W_salida) <- 'numeric'
  
  
  
  return(list('G_estimada' = G_salida, 'W_estimada' = W_salida))
  
}


estimacion_MARSS(dat, F, 32, 0, G, W, matrix(1.98e-05))
