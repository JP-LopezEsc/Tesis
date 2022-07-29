library('MARSS')

## load Nile flow data
data(Nile, package = "datasets")
## define model list
mod_list <- list(B = "identity", U = "zero", Q = matrix("q"),
                 Z = "identity", A = matrix("a"), R = matrix("r"))
## fit the model with MARSS
fit <- MARSS(matrix(Nile, nrow = 1), mod_list)





## load the data
data(SalmonSurvCUI, package = "MARSS")
## get time indices
years <- SalmonSurvCUI[, 1]
## number of years of data
TT <- length(years)
## get response variable: logit(survival)
dat <- matrix(SalmonSurvCUI[, 2], nrow = 1)

## get predictor variable
CUI <- SalmonSurvCUI[, 3]
## z-score the CUI
CUI_z <- matrix((CUI - mean(CUI))/sqrt(var(CUI)), nrow = 1)
## number of regr params (slope + intercept)
m <- dim(CUI_z)[1] + 1

## for process eqn
B <- diag(m) ## 2x2; Identity
U <- matrix(0, nrow = m, ncol = 1) ## 2x1; both elements = 0
Q <- matrix(list(0), m, m) ## 2x2; all 0 for now
diag(Q) <- c("q.alpha", "q.beta") ## 2x2; diag = (q1,q2)

## for observation eqn
Z <- array(NA, c(1, m, TT)) ## NxMxT; empty for now
Z[1, 1, ] <- rep(1, TT) ## Nx1; 1's for intercept
Z[1, 2, ] <- CUI_z ## Nx1; predictor variable
A <- matrix(0) ## 1x1; scalar = 0
R <- matrix("r") ## 1x1; scalar = r

## only need starting values for regr parameters
inits_list <- list(x0 = matrix(c(0, 0), nrow = m))
## list of model matrices & vectors
mod_list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R)

## fit univariate DLM
dlm_1 <- MARSS(dat, inits = inits_list, model = mod_list)

dlm_1$states
dlm_1$states.se
dlm_1$ytT



## get list of Kalman filter output
kf_out <- MARSSkfss(dlm_1)
## forecasts of regr parameters; 2xT matrix
eta <- kf_out$xtt1
## ts of E(forecasts)
fore_mean <- vector()
for (t in 1:TT) {
  browser()
  fore_mean[t] <- Z[, , t] %*% eta[, t, drop = FALSE]
}

## variance of regr parameters; 1x2xT array
Phi <- kf_out$Vtt1
## obs variance; 1x1 matrix
R_est <- coef(dlm_1, type = "matrix")$R
## ts of Var(forecasts)
fore_var <- vector()
for (t in 1:TT) {
  tZ <- matrix(Z[, , t], m, 1) ## transpose of Z
  fore_var[t] <- Z[, , t] %*% Phi[, , t] %*% tZ + R_est
}
