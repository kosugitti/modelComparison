library(runjags)
library(ggmcmc)
library(rstan)
options(mc.cores = parallel::detectCores())

# 世界一簡単なベイズ推定
N <- 100
mu <- 50
sig <- 10
X <- rnorm(N,mu,sig)

dataSet <- list(N=N,X=X)
model.stan <- stan_model("practice1.stan")
fit.stan <- sampling(model.stan,dataSet)
fit.stan

# JAGS
runJagsOut <- run.jags(method="parallel",
                       model="practice1jags.txt",
                       monitor=c("mu","sigma"),
                       data=dataSet,
                       n.chains=4,
                       adapt=1000,
                       burnin=1000,
                       sample=10000,
                       summarise=TRUE)
runJagsOut


# 世界で二番目に簡単なベイズ推定
N <- 1000
beta1 <- 0.8
beta0 <- 15
sig <- 10
X <- runif(N,0,100)
Yhat <- beta1 * X + beta0
Y <- Yhat + rnorm(N,0,sig)

dataSet <- list(N=N,X=X,Y=Y)
model.stan2 <- stan_model("practice2.stan")
fit.stan2 <- sampling(model.stan2,dataSet)
fit.stan2

# JAGS
runJagsOut2 <- run.jags(method="parallel",
                       model="practice2jags.txt",
                       monitor=c("beta0","beta1","sigma"),
                       data=dataSet,
                       n.chains=4,
                       adapt=1000,
                       burnin=1000,
                       sample=10000,
                       summarise=TRUE)
runJagsOut2

