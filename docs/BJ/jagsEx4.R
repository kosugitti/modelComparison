# 環境をまっさらに
rm(list=ls())
# ライブラリを読み込む
library(runjags)
library(ggmcmc)
library(coda)
library(ggplot2)
library(tidyr)
library(dplyr)

#------------------------------------------------------------------------------
# THE DATA.
# Randomly generated fictitious data.
# For each subject, specify the condition s/he was in,
# the number of trials s/he experienced, and the number correct.
npg = 20  # number of subjects per group
ntrl = 20 # number of trials per subject
CondOfSubj = c( rep(1,npg) , rep(2,npg) , rep(3,npg) , rep(4,npg) )
nTrlOfSubj = rep( ntrl , 4*npg )
set.seed(47405)
condMeans = c(.40,.50,.51,.52)
nCorrOfSubj = c( rbinom(npg,ntrl,condMeans[1]) , rbinom(npg,ntrl,condMeans[2]) ,
                 rbinom(npg,ntrl,condMeans[3]) , rbinom(npg,ntrl,condMeans[4]) )
nCond = length(unique(CondOfSubj))
nSubj = length(CondOfSubj)
# jitter the data to be as close as possible to desired condition means:
for ( cIdx in 1:nCond ) {
  nToAdd = round(condMeans[cIdx]*npg*ntrl)-sum(nCorrOfSubj[CondOfSubj==cIdx])
  if ( nToAdd > 0 ) {
    for ( i in 1:nToAdd ) {
      thisNcorr = ntrl
      while ( thisNcorr == ntrl ) {
        randSubjIdx = sample(which(CondOfSubj==cIdx),size=1)
        thisNcorr = nCorrOfSubj[randSubjIdx]
      }
      nCorrOfSubj[randSubjIdx] = nCorrOfSubj[randSubjIdx]+1
    }
  } 
  if ( nToAdd < 0 ) {
    for ( i in 1:abs(nToAdd) ) {
      thisNcorr = 0
      while ( thisNcorr == 0 ) {
        randSubjIdx = sample(which(CondOfSubj==cIdx),size=1)
        thisNcorr = nCorrOfSubj[randSubjIdx]
      }
      nCorrOfSubj[randSubjIdx] = nCorrOfSubj[randSubjIdx]-1
    }
  }
}


show( aggregate( nCorrOfSubj , by=list(CondOfSubj) , FUN=mean ) / ntrl )

# Package the data:
dataList = list(
  nCond = nCond ,
  nSubj = nSubj ,
  CondOfSubj = CondOfSubj ,
  nTrlOfSubj = nTrlOfSubj ,
  nCorrOfSubj = nCorrOfSubj
)

# ここを実行してデータセットの形を確認しておこう
dataList

#------------------------------------------------------------------------------
parameters = c("omega","kappa","omega0","theta","Idx")
adaptSteps = 1000            # Number of steps to "tune" the samplers.
burnInSteps = 5000           # Number of steps to "burn-in" the samplers.
nChains = 4                  # Number of chains to run.
numSavedSteps=12000          # Total number of steps in chains to save.
thinSteps=20                 # Number of steps to "thin" (1=keep every step).

fit.jags4 <- run.jags( method=c("parallel") ,
                        model="jags4.txt" , 
                        monitor=parameters , 
                        data=dataList ,  
                        #inits=initsList , 
                        n.chains=nChains ,
                        adapt=adaptSteps ,
                        burnin=burnInSteps , 
                        sample=ceiling(numSavedSteps/nChains) ,
                        thin=thinSteps ,
                        summarise=FALSE)
fit.df <- as.data.frame(as.matrix( fit.jags4$mcmc ))
fit.df$Idx <- as.factor(fit.df$Idx)
g <- ggplot(fit.df,aes(x=Idx)) + stat_count()
g

fit.jags4s <- run.jags( method=c("parallel") ,
                       model="jags4s.txt" , 
                       monitor=parameters , 
                       data=dataList ,  
                       #inits=initsList , 
                       n.chains=nChains ,
                       adapt=adaptSteps ,
                       burnin=burnInSteps , 
                       sample=ceiling(numSavedSteps/nChains) ,
                       thin=thinSteps ,
                       summarise=FALSE)
fit.df <- as.data.frame(as.matrix( fit.jags4s$mcmc ))
fit.df$Idx <- as.factor(fit.df$Idx)
g <- ggplot(fit.df,aes(x=Idx)) + stat_count()
g

