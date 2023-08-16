library(LaplacesDemon)


#Se asume que Gt y Wt son ctes conocidas para toda t.
actualizacion_dlm_V_desc <- function(y, variables_F, m0, C0, G, W, S0, n0, lista_interv){
  
  error_varianza <- function(x) {
    if(any (diag(x)<0)) stop ('Elementos del parametro de escala o varianzas deben ser positivos')
  }
  
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
    
    interv <- F
  }
  return(list("at" = lista_at,  "Rt" = lista_Rt,  "ft" = lista_ft, 
              "Qt" = lista_Qt, "CI" = lista_CI, "CI_inf" = lista_CI_inf, 
              "CI_sup" = lista_CI_sup, "mt" = lista_mt, "Ct" = lista_Ct,
              "St" = lista_St, 'm0' = m0, 'C0' = C0,
              'G' = G, 'W' = W, 'S0' = S0, 'lista_interv' = lista_interv,
              'n0' = n0, 'nT' = nt, 'y' = y, 'variables_F' = variables_F))
}




#Se asume que Gt, Vt y Wt son ctes conocidas para toda t.
actualizacion_dlm <- function(y, variables_F, m0, C0, G, W, V, lista_interv){

  error_varianza <- function(x) {
    if(any (diag(x)<0)) stop ('Elementos del parametro de escala o varianzas deben ser positivos')
  }
  
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
    
    interv <- F
  }
  return(list("at" = lista_at,  "Rt" = lista_Rt,  "ft" = lista_ft, 
              "Qt" = lista_Qt, "CI" = lista_CI, "CI_inf" = lista_CI_inf, 
              "CI_sup" = lista_CI_sup, "mt" = lista_mt, "Ct" = lista_Ct,
              'm0' = m0, 'C0' = C0))
}


#Los datos deben empezar en el primer periodo futuro (no observado)
#Para unas distribuciones iniciales dadas, calcula las distribuciones de pronostico
#hasta 'per_futuros' pasos
pronosticos <- function(variables_F, per_futuros, at_0, Rt_0, G, W, St, nt,
                        t_iter = NULL, lista_interv = NULL, 
                        per_anticip_interv = NULL){
  #browser() 
  at_k<- at_0
  Rt_k <- Rt_0
  lista_at_k <- list()
  lista_Rt_k <- list()
  lista_ft_k <- list()
  lista_Qt_k <- list()
  lista_CI_inf <- list()
  lista_CI_sup <- list()
  for(k in 1:per_futuros){
    
    if(isTRUE((t_iter + k - 1) %in% lista_interv$t_int & k <= per_anticip_interv)){
      at_k <- lista_interv$at_int[[match(t_iter + k - 1, list_interv$t_int)]]
      Rt_k <- lista_interv$Rt_int[[match(t_iter + k - 1, list_interv$t_int)]]
      interv <- T
    } else {
      at_k <- G %*% at_k
      Rt_k <- G %*% Rt_k %*% t(G) + W
    }

    Ft <- as.numeric(variables_F[k,])
    ft_k <- t(Ft) %*% at_k
    Qt_k <- t(Ft) %*% Rt_k %*% Ft + St
    CI <- c(qst(0.025, nu = nt, mu = ft_k, sigma = sqrt(Qt_k)),
            qst(0.975, nu = nt, mu = ft_k, sigma = sqrt(Qt_k)))
    CI_inf <- CI[1]
    CI_sup <- CI[2]
    lista_at_k[[k]] <- at_k
    lista_Rt_k[[k]] <- Rt_k
    lista_ft_k[[k]] <- ft_k
    lista_Qt_k[[k]] <- Qt_k
    lista_CI_inf[[k]] <- CI_inf
    lista_CI_sup[[k]] <- CI_sup
    
  }
  return(list("at_k" = lista_at_k, "Rt_k" = lista_Rt_k, "ft_k" = lista_ft_k, 
              "Qt_k" = lista_Qt_k, "CI_inf" = lista_CI_inf, "CI_sup" = lista_CI_sup))
}

# Para todos los periodos aplica la funcion de pronosticos
pronosticos_k_pasos <- function(prons_F, k, modelo, 
                                lista_interv = NULL, per_anticip_interv = NULL){
  
  lista_ft_k <- list()
  lista_Qt_k <- list()
  lista_CI_inf <- list()
  lista_CI_sup <- list()
  
  G <- modelo$G
  W <- modelo$W
  #La iteracion t contiene info hasta D_{t-1}
  for (t in 1:(length(prons_F) - k + 1)){
    
    if(t==1){
      at_0 <- modelo$m0
      Rt_0 <- modelo$C0
      St <- modelo$S0
      nt <- modelo$n0
    } else {
      at_0 <- modelo$mt[[t-1]]
      Rt_0 <- modelo$Ct[[t-1]]
      St <- modelo$St[[t-1]]
      nt <- nt + 1
    }
    variables_F = prons_F[[t]]
    prons_t <- pronosticos(variables_F = variables_F,
                           per_futuros = k,
                           at_0 = at_0, Rt_0 = Rt_0,
                           G = G, W = W, St = St, nt = nt, t_iter = t,
                           lista_interv = lista_interv, 
                           per_anticip_interv = per_anticip_interv
                           )
    
    lista_ft_k[[t]] <- prons_t$ft_k[[k]]
    lista_Qt_k[[t]] <- prons_t$Qt_k[[k]]
    lista_CI_inf[[t]] <- prons_t$CI_inf[[k]]
    lista_CI_sup[[t]] <- prons_t$CI_sup[[k]]
  }
  
  return(list("ft_k" = lista_ft_k, 
              "Qt_k" = lista_Qt_k, "CI_inf" = lista_CI_inf, "CI_sup" = lista_CI_sup))
}


suavizamiento_V_desc <- function(modelo){

  error_varianza <- function(x) {
    if(any (diag(x)<0)) stop ('Elementos del parametro de escala o varianzas deben ser positivos')
  }
 
  lista_ft_k_filt <- list()
  lista_at_k_filt <- list()
  lista_Rt_k_filt <- list()
  lista_resp_med_esc <- list()
  lista_CI_inf <- list()
  lista_CI_sup <- list()
  
  G <- modelo$G
  lista_interv <- modelo$lista_interv
  nt <-modelo$nT
  
  #Se calculan las distribuciones filtradas para k = T
  
  at_k_filt_mas_1 <- modelo$mt[[length(modelo$y)]]
  Rt_k_filt_mas_1 <- modelo$Ct[[length(modelo$y)]]
  St <- modelo$St[[length(modelo$y)]]
  
  Ft <- as.numeric(modelo$variables_F[length(modelo$y),]) 
  lista_ft_k_filt[[length(modelo$y)]] <- t(Ft) %*% at_k_filt_mas_1
  lista_at_k_filt[[length(modelo$y)]] <- at_k_filt_mas_1
  lista_Rt_k_filt[[length(modelo$y)]] <- Rt_k_filt_mas_1
  lista_resp_med_esc[[length(modelo$y)]] <- t(Ft) %*% Rt_k_filt_mas_1 %*% Ft
  lista_CI_inf[[length(modelo$y)]] <- qst(0.025, nu = nt, mu = lista_ft_k_filt[[length(modelo$y)]],
                                         sigma = sqrt(lista_resp_med_esc[[length(modelo$y)]]))
  lista_CI_sup[[length(modelo$y)]] <- qst(0.975, nu = nt, mu = lista_ft_k_filt[[length(modelo$y)]],
                                         sigma = sqrt(lista_resp_med_esc[[length(modelo$y)]]))
  interv <- F
  
  error_varianza(Rt_k_filt_mas_1)
  error_varianza(St)
  
  for(i in length(modelo$y):2){
    #Se calculan las distribuciones filtradas para i-1. De lo mas reciente a lo mas viejo
    
    if(i %in% lista_interv$t_int){
      at_int <- lista_interv$at_int[[match(i,lista_interv$t_int)]]
      Rt_int <- lista_interv$Rt_int[[match(i,lista_interv$t_int)]]
      Rt<- G  %*% modelo$Ct[[i-1]] %*% t(G)
      Ut <- t(chol(Rt_int))
      Zt <- t(chol(Rt))
      Kt <- Ut %*% solve(Zt)
      Gt_int <- Kt %*% G
      interv <- T
    }
    
    Ct_k <- modelo$Ct[[i-1]]
    Rt_k_mas_1 <- modelo$Rt[[i]]
    mt_k <- modelo$mt[[i-1]]
    at_k_mas_1 <- modelo$at[[i]]
    St_k_mas_1 <- modelo$St[[i]]
    St_k <- modelo$St[[i-1]]
    
    Bt_k <- Ct_k %*% t(`if`(interv,Gt_int, G)) %*% solve(Rt_k_mas_1)
    at_k_filt <- mt_k + Bt_k %*% (at_k_filt_mas_1 - at_k_mas_1)
    Rt_k_filt <- Ct_k + Bt_k %*% (Rt_k_filt_mas_1 - Rt_k_mas_1) %*% t(Bt_k)
    
    Ft_k <- as.numeric(modelo$variables_F[i-1,])
    ft_k_filt <- t(Ft_k) %*% at_k_filt
    resp_med_esc <- (St/St_k)*(t(Ft_k) %*% Rt_k_filt %*% Ft_k)
    CI <- c(qst(0.025, nu = nt, mu = ft_k_filt, sigma = sqrt(resp_med_esc)),
            qst(0.975, nu = nt, mu = ft_k_filt, sigma = sqrt(resp_med_esc)))
    
    error_varianza(Rt_k_mas_1)
    error_varianza(Ct_k)
    error_varianza(St_k_mas_1)
    error_varianza(St_k)
    error_varianza(Rt_k_filt)
    
    lista_ft_k_filt[[i-1]] <- ft_k_filt
    lista_at_k_filt[[i-1]] <- at_k_filt
    lista_Rt_k_filt[[i-1]] <- Rt_k_filt
    lista_resp_med_esc[[i-1]] <- resp_med_esc
    lista_CI_inf[[i-1]] <- CI[1]
    lista_CI_sup[[i-1]] <- CI[2]
    
    at_k_filt_mas_1 <- at_k_filt
    Rt_k_filt_mas_1 <- Rt_k_filt
    interv <- F
  }
  
  return(list("ft_k_filt" = lista_ft_k_filt, "at_k_filt" = lista_at_k_filt,
              "Rt_k_filt" = lista_Rt_k_filt, "resp_media_esc" = lista_resp_med_esc, 
              "CI_inf" = lista_CI_inf, "CI_sup" = lista_CI_sup))
}



