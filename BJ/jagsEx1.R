# 環境をまっさらに
rm(list=ls())
# ライブラリを読み込む
library(rjags)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# set.seed
set.seed(20170907)
# set sample size
N <- 50
# 真値theta
theta <- 0.4
# 乱数発生
y <- rbinom(N,1,theta)
y


# 初期値を与えてやる
thetaInit <- sum(y)/length(y)
initsList <- list(theta=thetaInit)

# 初期値を与える関数
initsList <- function(){
  resampledY <- sample(y,replace=TRUE)
  thetaInit <- sum(resampledY)/length(resampledY)
  thetaInit <- 0.001 + 0.998*thetaInit
  return(list(theta=thetaInit))
}
# 挙動の確認
initsList()

# 実行してみよう
datajags <- list(Ntotal=length(y),y=y)
# 乱数発生機を作る
model1.jags <- jags.model(file="jags1.txt",data=datajags,
                          inits=initsList,n.chains=3,n.adapt=500)
# ウォームアップする
update(model1.jags,n.iter=500)
# サンプリングする
samples <- coda.samples(model1.jags,variable.names=c("theta"),n.iter=3334)
summary(samples)

plot(samples)

library(ggmcmc)
ggS <- ggs(samples)

ggs_histogram(ggS)
ggs_density(ggS)
ggs_traceplot(ggS)
ggs_running(ggS)
ggs_compare_partial(ggS)
ggs_autocorrelation(ggS)
ggs_geweke(ggS)

#library(coda)
codamenu()

coda::geweke.diag(samples)
coda::gelman.diag(samples)
coda::raftery.diag(samples)
coda::heidel.diag(samples)

HPDinterval(samples)
acfplot(samples)

# Stanの場合
# 乱数発生機を作る
model1.stan <- stan_model("jags1.stan",model_name="jags mimic ex1")
datastan <- list(N=length(y),Y=y)
# サンプリングする（含ウォームアップ）
fit <- sampling(model1.stan,datastan)
print(fit,pars="theta")
