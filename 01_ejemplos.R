################################################################################
## Título: Ejemplos libro

## Descripción: Se ven los datos y ejemplos del libro Bayesian Statistics for MKT

## Fecha: 14/02/2022
################################################################################

library(bayesm)
source("00_rhierBinLogit.R")

# Multinomial logit example -----------------------------------------------

data("margarine")

margarine$demos
margarine$choicePrice




# Ejemplo con código ------------------------------------------------------

data(bank)
choiceAtt=bank$choiceAtt
Z=bank$demo

## center demo data so that mean of random effects
## distribution can be interpreted as the average respondents
Z[,1]=rep(1,nrow(Z))
Z[,2]=Z[,2]-mean(Z[,2])
Z[,3]=Z[,3]-mean(Z[,3])
Z[,4]=Z[,4]-mean(Z[,4])

#Se acomodan los datos en una lista. Cada unidad i tiene su lista dentro de esa lista
Z=as.matrix(Z)
hh=levels(factor(choiceAtt$id))
nhh=length(hh)
lgtdata=NULL
for (i in 1:nhh) {
  y=choiceAtt[choiceAtt[,1]==hh[i],2]
  nobs=length(y)
  X=as.matrix(choiceAtt[choiceAtt[,1]==hh[i],c(3:16)])
  lgtdata[[i]]=list(y=y,X=X)
}
p = c(1)
Data=list(lgtdata=lgtdata,Z=Z, p=p)
Data$lgtdata[[124]]

#Se definen los parámetros de MCMC
Mcmc=list(R=20000,sbeta=0.2,keep=20)
#Modifiqué ligeramente la función del paquete del libro.
out=rhierBinLogit_JP(Data=Data,Mcmc=Mcmc)

# out=rhierMnlRwMixture(Data = Data, Mcmc = Mcmc)

str(out)


index=4*c(0:13)+1
matplot(out$Deltadraw[,index],type="l",xlab="Iterations/20",
          ylab=" ",main="Average Respondent Part-Worths")


index=c(0:13)*15+1
matplot(out$Vbetadraw[,index],type="l",xlab="Iterations/20", ylab=" ",main="V-beta Draws")


plot(out$llike,type="l",xlab="Iterations/20",ylab=" ", main="Posterior Log Likelihood")

plot(out$reject,type="l",xlab="Iterations/20",ylab=" ", main="Rejection Rate of Metropolis-Hastings Algorithm")
  