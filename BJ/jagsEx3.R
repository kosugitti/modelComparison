# 環境をまっさらに
rm(list=ls())
# ライブラリを読み込む
library(runjags)
library(ggmcmc)
library(coda)
library(ggplot2)
library(tidyr)
library(dplyr)
library(rstan)
options(mc.cores = parallel::detectCores())
# 表6回，裏3回
coins <- c(rep(0,9-6),rep(1,6))
dataSet <- list(y=coins,N=9)

fit.jags3 <- run.jags(method="parallel",
                model="jags3.txt",
                monitor=c("m","theta","omega","kappa"),
                data=dataSet,
                n.chains=4,
                adapt=1000,
                burnin=1000,
                sample=10000,
                summarise=TRUE)
fit.df <- as.data.frame(as.matrix(fit.jags3$mcmc))
fit.df$m <- as.factor(fit.df$m)
summary(fit.df)
# モデルの確信度比較
g <- ggplot(fit.df,aes(x=m,fill=m)) + geom_bar()
g

# 各モデルの推定値
mean(fit.df[fit.df$m==1,]$theta)
mean(fit.df[fit.df$m==2,]$theta)
median(fit.df[fit.df$m==1,]$theta)
median(fit.df[fit.df$m==2,]$theta)
g <- ggplot(fit.df,aes(x=theta,fill=m))+geom_histogram(binwidth = 0.02)+facet_wrap(~m)
g


# より一般的なモデル
fit.jags3b <- run.jags(method="parallel",
                model="jags3b.txt",
                monitor=c("m","theta1","theta2"),
                data=dataSet,
                n.chains=4,
                adapt=1000,
                burnin=1000,
                sample=10000,
                summarise=TRUE)
summary(fit)
fit.df <- as.data.frame(as.matrix(fit.jags3b$mcmc))
fit.df$m <- as.factor(fit.df$m)
summary(fit.df)
ggS <- ggs(as.mcmc.list(fit.jags3b))
ggs_autocorrelation(ggS)


fit.df %>% tidyr::gather(key,val,-m) %>% ggplot(aes(x=val,fill=key)) +
  geom_density(alpha=0.5) + facet_wrap(~m)
fit.df %>% dplyr::group_by(m) %>% summarize(median1=median(theta1),mean1=mean(theta1),
                                            median2=median(theta2),mean2=mean(theta2))

# データセットが変わりますので注意
coins <- c(rep(0,30-17),rep(1,17))
dataSet <- list(y=coins,N=30)
# 1st Trial 擬似事前分布の場所を探す(s1)
fit.jags3bs1 <- run.jags(method="parallel",
                model="jags3bs1.txt",
                monitor=c("m","theta1","theta2"),
                data=dataSet,
                n.chains=4,
                adapt=1000,
                burnin=1000,
                sample=10000,
                summarise=TRUE)
fit.df <- as.data.frame(as.matrix(fit.jags3bs1$mcmc))
fit.df %>% dplyr::group_by(m) %>% 
  summarize(median1=median(theta1),median2=median(theta2))

fit.df %>% tidyr::gather(key,val,-m) %>% ggplot(aes(x=val,fill=key)) +
  geom_density(alpha=0.5) + facet_wrap(~m+key)


# 2nd Trial(s2)
fit.jags3bs2 <- run.jags(method="parallel",
                   model="jags3bs2.txt",
                   monitor=c("m","theta1","theta2"),
                   data=dataSet,
                   n.chains=4,
                   adapt=1000,
                   burnin=1000,
                   sample=10000,
                   summarise=TRUE)
fit.df <- as.data.frame(as.matrix(fit.jags3bs2$mcmc))
fit.df %>% dplyr::group_by(m) %>% 
  summarize(median1=median(theta1),median2=median(theta2))

fit.df %>% tidyr::gather(key,val,-m) %>% ggplot(aes(x=val,fill=key)) +
  geom_density(alpha=0.5) + facet_wrap(~m+key)

fit.df$m <- as.factor(fit.df$m)
g <- ggplot(fit.df,aes(x=m,fill=m)) + geom_bar()
g


## Stanではできなかった・・・(><)
jags3b.stan <- stan_model("jags3.stan")
coins <- c(rep(0,9-6),rep(1,6))
dataSet <- list(Y=coins,N=9)
fit <- sampling(jags3b.stan,data=dataSet)
fit


jags3b2.stan <- stan_model("jags3b2.stan")
coins <- c(rep(0,30-17),rep(1,17))
dataSet <- list(Y=coins,N=30)
fit2 <- sampling(jags3b2.stan,data=dataSet)
fit2
