# 環境をまっさらに
rm(list=ls())
# ライブラリを読み込む
library(rjags)
library(runjags)
library(ggmcmc)
library(rstan)
options(mc.cores = parallel::detectCores())
# set.seed
set.seed(20170907)
# set sample size
N1 <- 50
N2 <- 30
# 真値theta
theta1 <- 0.4
theta2 <- 0.6
# 乱数発生
y1 <- rbinom(N1,1,theta1)
y2 <- rbinom(N2,1,theta2)

# Stanの場合
model2.stan <- stan_model("jags2.stan",model_name="jags mimic ex2")
datastan <- list(Ntotal=N1+N2,Y=c(y1,y2),s=c(rep(1,N1),rep(2,N2)),Nsubj=2)
# サンプリングする（含ウォームアップ）
fit <- sampling(model2.stan,datastan)
print(fit)

# JAGSの場合
datajags <- list(Ntotal=N1+N2,Y=c(y1,y2),s=c(rep(1,N1),rep(2,N2)),Nsubj=2)


# 乱数発生機を作る
model2.jags <- jags.model(file="jags2.txt",data=datajags,
                          n.chains=3,inits = list(theta=c(mean(y1),mean(y2))))
# ウォームアップする
update(model2.jags,n.iter=500)
# サンプリングする
samples <- coda.samples(model2.jags,variable.names=c("theta"),n.iter=3334)
summary(samples)
samples.matrix <- as.matrix(samples)
diff <- samples.matrix[,1]-samples.matrix[,2]
summary(diff)


Nsubj <- max(datajags$s)
s <- datajags$s
Y <- datajags$Y

initsList = function() {
  thetaInit = rep(0,Nsubj)
  for ( i in 1:Nsubj ) { 
    yThisSubj <- Y[s==i]
    resampledY = sample( yThisSubj , replace=TRUE )  # resample
    thetaInit[i] = sum(resampledY)/length(resampledY) 
  }
  thetaInit = 0.001+0.998*thetaInit # keep away from 0,1
  return( list( theta=thetaInit ) )
}


try( runjags.options( inits.warning=FALSE , rng.warning=FALSE ) )

runJagsOut <- run.jags(method="parallel",
                       model="jags2.txt",
                       monitor=c("theta"),
                       data=datajags,
                       inits = initsList,
                       n.chains=4,
                       adapt=1000,
                       burnin=1000,
                       sample=10000,
                       summarise=TRUE)


plot(runJagsOut,layout=c(2,2))

codaSamples <- as.mcmc.list(runJagsOut)
ggS <- ggs(codaSamples)
ggs_density(ggS)
