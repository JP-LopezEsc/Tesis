library(LaplacesDemon)


estimacion_MARSS <- function(y, F, inicio, periodos_extra, G, W, V, mt, Ct){
 
  U <- matrix(0, nrow = m, ncol = 1) ## 2x1; both elements = 0
  A <- matrix(0) ## 1x1; scalar = 0
  
  y_iter <- matrix(y[periodos_extra:(inicio + periodos_extra-1)], nrow = 1)
  mod_list <- list(B = G, U = U, Q = W, 
                   Z = array(F[1,,periodos_extra:(inicio + periodos_extra-1)], 
                             c(1,m,length(F[1,1,periodos_extra:(inicio + periodos_extra-1)]))), 
                   A = A, R = V, x0 = mt, V0 = Ct)
  
  dlm <- MARSS(y_iter, model = mod_list, method = 'BFGS')
  
  #G_salida <-  matrix(list(0), m, m)
  #diag(G_salida) <- dlm$par$B
  G_salida <- matrix(c(dlm$par$B[1], 0, 0,
                       0, dlm$par$B[2], 0,
                       0,0,1), nrow=3)
  mode(G_salida) <- 'numeric'
  
  # W_salida <-  matrix(list(0), m, m)
  # diag(W_salida) <- dlm$par$Q
  W_salida <- matrix(c(dlm$par$Q[1], 0, 0,
                       0, dlm$par$Q[2], 0,
                       0,0,0), nrow=3)
  mode(W_salida) <- 'numeric'
  
  V_salida <-  coef(dlm, type = "matrix")$R
  mode(V_salida) <- 'numeric'
  
  return(list('G_estimada' = G_salida, 'W_estimada' = W_salida, 'V_estimada' = V_salida))
  
}


#Se asume que Gt, Vt y Wt son ctes conocidas para toda t.
actualizacion_dlm_estima_G_W <- function(y, variables_F, y_hist, variables_F_hist,
                                         inicio, m0, C0, G_forma, W_forma, V_forma, lista_interv){
  
  error_varianza <- function(x) {
    if(any (diag(x)<0)) stop ('Elementos del parametro de escala o varianzas deben ser positivos')
  }

  mt_hist <- read_rds('cache/outputs_modelos/12_periodos/sin tiie/m1_m12.rds')
  Ct_hist <- read_rds('cache/outputs_modelos/12_periodos/sin tiie/C1_C12.rds')
  mt_menos_1 <- m0
  Ct_menos_1 <- C0
  lista_at <- list()
  lista_Rt <- list()
  lista_ft <- list()
  lista_Qt <- list()
  lista_mt <- list()
  lista_Ct <- list()
  lista_CI <- list()
  lista_CI_inf <- list()
  lista_CI_sup <- list()
  interv <- F
  for(t in 1:length(y)){
  
    mt_inicio <- matrix(unlist(mt_hist[length(mt_hist)-inicio+1]), nrow=3)
    Ct_inicio <- matrix(unlist(Ct_hist[length(Ct_hist)-inicio+1]), nrow=3)
    
    estima_MARSS <- estimacion_MARSS(y_hist, variables_F_hist, inicio, t, G_forma,
                                     W_forma, V_forma, mt_inicio, Ct_inicio)
    G <- estima_MARSS$G_estimada
    W <- estima_MARSS$W_estimada
    V <- estima_MARSS$V_estimada
    
    if(t %in% lista_interv$t_int){
      at <- lista_interv$at_int[[match(t,list_interv$t_int)]]
      Rt <- lista_interv$Rt_int[[match(t,list_interv$t_int)]]
      interv <- T
    } else {
      at <- G %*% mt_menos_1
      Rt <- G %*% Ct_menos_1 %*% t(G) + W
    }
    
    Ft <- as.numeric(variables_F[t,])
    ft <- t(Ft) %*% at
    Qt <- t(Ft) %*% Rt %*% Ft + V
    CI <- c(qnorm(0.025, mean = ft, sd = sqrt(Qt)), qnorm(0.975, mean = ft, 
                                                          sd = sqrt(Qt)))
    CI_inf <- CI[1]
    CI_sup <- CI[2]
    Yt <- y[t]
    At <- Rt %*% Ft %*% solve(Qt)
    et <- Yt-ft
    mt <- at + At %*% et
    Ct <- (Rt - At %*% Qt %*% t(At))
    
    error_varianza(Rt)
    error_varianza(Ct)
    
    lista_at[[t]] <- at
    lista_Rt[[t]] <- Rt
    lista_ft[[t]] <- ft
    lista_Qt[[t]] <- Qt
    lista_CI[[t]] <- CI
    lista_CI_inf[[t]] <- CI_inf
    lista_CI_sup[[t]] <- CI_sup
    lista_mt[[t]] <- mt
    lista_Ct[[t]] <- Ct
    mt_menos_1 <- mt
    Ct_menos_1 <- Ct
    
    mt_hist[[inicio + t]] <- mt
    Ct_hist[[inicio + t]] <- Ct
    
    interv <- F
  }
  return(list("at" = lista_at,  "Rt" = lista_Rt,  "ft" = lista_ft, 
              "Qt" = lista_Qt, "CI" = lista_CI, "CI_inf" = lista_CI_inf, 
              "CI_sup" = lista_CI_sup, "mt" = lista_mt, "Ct" = lista_Ct))
}





actualizacion_dlm_V_desc_estima_G_W <- function(y, variables_F, y_hist, variables_F_hist,
                                                inicio, m0, C0, G_forma, W_forma, S0, n0,
                                                lista_interv){
  
  error_varianza <- function(x) {
    if(any (diag(x)<0)) stop ('Elementos del parametro de escala o varianzas deben ser positivos')
  }
  
  mt_hist <- read_rds('cache/outputs_modelos/12_periodos/sin tiie/m1_m12.rds')
  Ct_hist <- read_rds('cache/outputs_modelos/12_periodos/sin tiie/C1_C12.rds')
  mt_menos_1 <- m0
  Ct_menos_1 <- C0
  St_menos_1 <- S0
  nt_menos_1 <- n0
  lista_at <- list()
  lista_Rt <- list()
  lista_ft <- list()
  lista_Qt <- list()
  lista_mt <- list()
  lista_Ct <- list()
  lista_CI <- list()
  lista_CI_inf <- list()
  lista_CI_sup <- list()
  lista_St <- list()
  interv <- F
  for(t in 1:length(y)){
   
    mt_inicio <- matrix(unlist(mt_hist[length(mt_hist)-inicio+1]), nrow=3)
    Ct_inicio <- matrix(unlist(Ct_hist[length(Ct_hist)-inicio+1]), nrow=3)
    
    estima_MARSS <- estimacion_MARSS(y_hist, variables_F_hist, inicio, t, G_forma,
                                     W_forma, V = matrix(St_menos_1), mt_inicio, Ct_inicio)
    G <- estima_MARSS$G_estimada
    W <- estima_MARSS$W_estimada
    
    
    if(t %in% lista_interv$t_int){
      at <- lista_interv$at_int[[match(t,list_interv$t_int)]]
      Rt <- lista_interv$Rt_int[[match(t,list_interv$t_int)]]
      interv <- T
    } else {
      at <- G %*% mt_menos_1
      Rt <- G %*% Ct_menos_1 %*% t(G) + W
    }
    
    Ft <- as.numeric(variables_F[t,])
    ft <- t(Ft) %*% at
    Qt <- t(Ft) %*% Rt %*% Ft + St_menos_1
    CI <- c(qst(0.025, nu = nt_menos_1, mu = ft, sigma = sqrt(Qt)),
            qst(0.975, nu = nt_menos_1, mu = ft, sigma = sqrt(Qt)))
    CI_inf <- CI[1]
    CI_sup <- CI[2]
    Yt <- y[t]
    At <- Rt %*% Ft %*% solve(Qt)
    et <- Yt-ft
    mt <- at + At %*% et
    nt <- nt_menos_1 + 1
    St <- as.numeric(St_menos_1 + (St_menos_1/nt) * (et^2 %*% solve(Qt) - 1))
    Ct <- (St/St_menos_1)*(Rt - At %*% Qt %*% t(At))
    
    error_varianza(Rt)
    error_varianza(St)
    error_varianza(Ct)
    
    lista_at[[t]] <- at
    lista_Rt[[t]] <- Rt
    lista_ft[[t]] <- ft
    lista_Qt[[t]] <- Qt
    lista_CI[[t]] <- CI
    lista_CI_inf[[t]] <- CI_inf
    lista_CI_sup[[t]] <- CI_sup
    lista_mt[[t]] <- mt
    lista_Ct[[t]] <- Ct
    lista_St[[t]] <- St
    mt_menos_1 <- mt
    Ct_menos_1 <- Ct
    nt_menos_1 <- nt
    St_menos_1 <- St
    
    mt_hist[[inicio + t]] <- mt
    Ct_hist[[inicio + t]] <- Ct
    
    interv <- F
  }
  return(list("at" = lista_at,  "Rt" = lista_Rt,  "ft" = lista_ft, 
              "Qt" = lista_Qt, "CI" = lista_CI, "CI_inf" = lista_CI_inf, 
              "CI_sup" = lista_CI_sup, "mt" = lista_mt, "Ct" = lista_Ct,
              "St" = lista_St))
}



################################################################################
## Usando los valores iniciales
##
################################################################################



estimacion_MARSS2 <- function(y, F, inicio, periodos_extra, G, W, V, mt, Ct){
  
  U <- matrix(0, nrow = m, ncol = 1) ## 2x1; both elements = 0
  A <- matrix(0) ## 1x1; scalar = 0
  
  y_iter <- matrix(y[(periodos_extra+1):(inicio + periodos_extra)], nrow = 1)
  mod_list <- list(B = G, U = U, Q = W, 
                   Z = array(F[1,,(periodos_extra+1):(inicio + periodos_extra)], 
                             c(1,m,length(F[1,1,(periodos_extra+1):(inicio + periodos_extra)]))), 
                   A = A, R = V, x0 = mt, V0 = Ct)
  
  dlm <- MARSS(y_iter, model = mod_list, method = 'BFGS')
  
  #G_salida <-  matrix(list(0), m, m)
  #diag(G_salida) <- dlm$par$B
  G_salida <- matrix(c(dlm$par$B[1], 0, 0, 
                       0, dlm$par$B[2], 0, 
                       0,0,1), nrow=3)
  mode(G_salida) <- 'numeric'
  
  # W_salida <-  matrix(list(0), m, m)
  # diag(W_salida) <- dlm$par$Q
  W_salida <- matrix(c(dlm$par$Q[1], 0, 0, 
                       0, dlm$par$Q[2], 0, 
                       0,0,0), nrow=3)
  mode(W_salida) <- 'numeric'
  
  V_salida <-  coef(dlm, type = "matrix")$R
  mode(V_salida) <- 'numeric'
  
  return(list('G_estimada' = G_salida, 'W_estimada' = W_salida, 'V_estimada' = V_salida))
  
}



actualizacion_dlm_V_desc_estima_G_W_v2 <- function(y, variables_F, y_hist, variables_F_hist,
                                                inicio, m0, C0, G_forma, W_forma, S0, n0,
                                                lista_interv){
  
  error_varianza <- function(x) {
    if(any (diag(x)<0)) stop ('Elementos del parametro de escala o varianzas deben ser positivos')
  }
  
  mt_hist <- read_rds('cache/outputs_modelos/12_periodos/sin tiie/m1_m12.rds')
  Ct_hist <- read_rds('cache/outputs_modelos/12_periodos/sin tiie/C1_C12.rds')
  mt_menos_1 <- m0
  Ct_menos_1 <- C0
  St_menos_1 <- S0
  nt_menos_1 <- n0
  lista_at <- list()
  lista_Rt <- list()
  lista_ft <- list()
  lista_Qt <- list()
  lista_mt <- list()
  lista_Ct <- list()
  lista_CI <- list()
  lista_CI_inf <- list()
  lista_CI_sup <- list()
  lista_St <- list()
  interv <- F
  G <- read_rds('cache/outputs_modelos/12_periodos/sin tiie/G.rds')
  W <- read_rds('cache/outputs_modelos/12_periodos/sin tiie/W.rds')
  
  for(t in 1:length(y)){
  
   
    if(t %in% lista_interv$t_int){
      at <- lista_interv$at_int[[match(t,list_interv$t_int)]]
      Rt <- lista_interv$Rt_int[[match(t,list_interv$t_int)]]
      interv <- T
    } else {
      at <- G %*% mt_menos_1
      Rt <- G %*% Ct_menos_1 %*% t(G) + W
    }
    
    Ft <- as.numeric(variables_F[t,])
    ft <- t(Ft) %*% at
    Qt <- t(Ft) %*% Rt %*% Ft + St_menos_1
    CI <- c(qst(0.025, nu = nt_menos_1, mu = ft, sigma = sqrt(Qt)),
            qst(0.975, nu = nt_menos_1, mu = ft, sigma = sqrt(Qt)))
    CI_inf <- CI[1]
    CI_sup <- CI[2]
    Yt <- y[t]
    At <- Rt %*% Ft %*% solve(Qt)
    et <- Yt-ft
    mt <- at + At %*% et
    nt <- nt_menos_1 + 1
    St <- as.numeric(St_menos_1 + (St_menos_1/nt) * (et^2 %*% solve(Qt) - 1))
    Ct <- (St/St_menos_1)*(Rt - At %*% Qt %*% t(At))
    
    error_varianza(Rt)
    error_varianza(St)
    error_varianza(Ct)
    
    lista_at[[t]] <- at
    lista_Rt[[t]] <- Rt
    lista_ft[[t]] <- ft
    lista_Qt[[t]] <- Qt
    lista_CI[[t]] <- CI
    lista_CI_inf[[t]] <- CI_inf
    lista_CI_sup[[t]] <- CI_sup
    lista_mt[[t]] <- mt
    lista_Ct[[t]] <- Ct
    lista_St[[t]] <- St
    mt_menos_1 <- mt
    Ct_menos_1 <- Ct
    nt_menos_1 <- nt
    St_menos_1 <- St
    
    mt_hist[[inicio + t]] <- mt
    Ct_hist[[inicio + t]] <- Ct
    
    interv <- F
    
    mt_inicio <- matrix(unlist(mt_hist[length(mt_hist)-inicio+1]), nrow=3)
    Ct_inicio <- matrix(unlist(Ct_hist[length(Ct_hist)-inicio+1]), nrow=3)
    
    estima_MARSS <- estimacion_MARSS2(y_hist, variables_F_hist, inicio, t, G_forma,
                                     W_forma, V = matrix(St_menos_1), mt_inicio, Ct_inicio)
    
    G <- estima_MARSS$G_estimada
    W <- estima_MARSS$W_estimada
  }
  return(list("at" = lista_at,  "Rt" = lista_Rt,  "ft" = lista_ft, 
              "Qt" = lista_Qt, "CI" = lista_CI, "CI_inf" = lista_CI_inf, 
              "CI_sup" = lista_CI_sup, "mt" = lista_mt, "Ct" = lista_Ct,
              "St" = lista_St))
}

