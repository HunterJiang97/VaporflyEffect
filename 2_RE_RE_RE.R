## JAGS Model 1: Runner:RE Course:RE Gender: RE Shoe:Linear
# 1.1 R: Gaussian C: Gaussian Gender: Gaussian
# 1.2 R: Gaussian C: Double-e Gender: Gaussian 
# 1.3 R: Double-e C: Gaussian Gender: Gaussian
# 1.4 R: Double-e C: Double-e Gender: Gaussian
##

## Clear up
rm(list=ls())
setwd("C:/Users/User1/Desktop/WorkFlow/ST540Midterm/Simulation/")
library(rjags)

## Resource
Shoe.data <- readRDS(file = "1_data.RDS")
indi <- readRDS(file = "1_indi.RDS")
trainfun <- readRDS(file = "1_jags.RDS")
checkfun <- readRDS(file = "1_func.RDS")
X <- as.matrix(readRDS(file = "1_dat1.RDS"))
Y <- readRDS(file = "1_daty.RDS")


## JAGS Model 1
Model1 <- list()
  
## JAGS Model: 1.1 Runner:RE Course:RE Shoe:Linear
Model1[[1]] <- textConnection("model{

   ## Likelihood
    for(ii in 1:n){
      y[ii] ~ dnorm(alpha1[f1[ii]] + alpha2[f2[ii]] + alpha3[f3[ii]] + inprod(x[ii,], beta[]), taue)
    }


   ## Priors
   # Random effects
    for(ii in 1:nf1){
      alpha1[ii] ~ dnorm(mu,tau)
    }
    for(ii in 1:nf2){
      alpha2[ii] ~ dnorm(mu,tau)
    }
    for(ii in 1:nf3){
      alpha3[ii] ~ dnorm(mu,tau)
    }
    
    for(jj in 1:p){
      beta[jj] ~ dnorm(0, 0.001)
    }
    mu   ~ dnorm(0,0.001)
    taue ~ dgamma(0.1,0.1)
    tau ~ dgamma(0.1,0.1)
}")

## JAGS Model: 1.1 Runner:RE Course:RE Shoe:Linear
Model1[[2]] <- textConnection("model{

   ## Likelihood
    for(ii in 1:n){
      y[ii] ~ dnorm(alpha1[f1[ii]] + alpha2[f2[ii]] + alpha3[f3[ii]] + inprod(x[ii,], beta[]), taue)
    }


   ## Priors
   # Random effects
    for(ii in 1:nf1){
      alpha1[ii] ~ dnorm(mu,tau)
    }
    for(ii in 1:nf2){
      alpha2[ii] ~ ddexp(mu,tau)
    }
    for(ii in 1:nf3){
      alpha3[ii] ~ dnorm(mu,tau)
    }
    
    for(jj in 1:p){
      beta[jj] ~ dnorm(0, 0.001)
    }
    mu   ~ dnorm(0,0.001)
    taue ~ dgamma(0.1,0.1)
    tau ~ dgamma(0.1,0.1)
}")

## JAGS Model: 1.1 Runner:RE Course:RE Shoe:Linear
Model1[[3]] <- textConnection("model{

   ## Likelihood
    for(ii in 1:n){
      y[ii] ~ dnorm(alpha1[f1[ii]] + alpha2[f2[ii]] + alpha3[f3[ii]] + inprod(x[ii,], beta[]), taue)
    }


   ## Priors
   # Random effects
    for(ii in 1:nf1){
      alpha1[ii] ~ ddexp(mu,tau)
    }
    for(ii in 1:nf2){
      alpha2[ii] ~ dnorm(mu,tau)
    }
    for(ii in 1:nf3){
      alpha3[ii] ~ dnorm(mu,tau)
    }
    
    for(jj in 1:p){
      beta[jj] ~ dnorm(0, 0.001)
    }
    mu   ~ dnorm(0,0.001)
    taue ~ dgamma(0.1,0.1)
    tau ~ dgamma(0.1,0.1)
}")

## JAGS Model: 1.1 Runner:RE Course:RE Shoe:Linear
Model1[[4]] <- textConnection("model{

   ## Likelihood
    for(ii in 1:n){
      y[ii] ~ dnorm(alpha1[f1[ii]] + alpha2[f2[ii]] + alpha3[f3[ii]] + inprod(x[ii,], beta[]), taue)
    }

   ## Priors
   # Random effects
    for(ii in 1:nf1){
      alpha1[ii] ~ ddexp(mu,tau)
    }
    for(ii in 1:nf2){
      alpha2[ii] ~ ddexp(mu,tau)
    }
    for(ii in 1:nf3){
      alpha3[ii] ~ dnorm(mu,tau)
    }
    
    for(jj in 1:p){
      beta[jj] ~ dnorm(0, 0.001)
    }
    mu   ~ dnorm(0,0.001)
    taue ~ dgamma(0.1,0.1)
    tau ~ dgamma(0.1,0.1)
}")




## Train & Test
Results <- matrix(NA, ncol = 5, nrow = 4)
var <- c("alpha1", "alpha2","alpha3","beta")
Train.dat <- list(y = Shoe.data$y[indi], x = Shoe.data$x[indi,], f1=Shoe.data$f1[indi], f2=Shoe.data$f2[indi], 
                  f3=Shoe.data$f3[indi], n=length(indi), nf1=Shoe.data$nf1, nf2=Shoe.data$nf2, nf3=2, p=Shoe.data$p)

for (ii in 1:4){
    model.jags <- jags.model(Model1[[ii]], data = Train.dat, n.chains=2, quiet=TRUE)
    update(model.jags, 50000, progress.bar="none")
    sample.jags <- coda.samples(model.jags, variable.names = var, thin = 100, n.iter = 100000, progress.bar="none")
    Results[ii,] <- checkfun(sample.jags, model.jags, X[-indi,], Y[-indi])
}

Results
saveRDS(Results, file = "2_RERE.RDS")
