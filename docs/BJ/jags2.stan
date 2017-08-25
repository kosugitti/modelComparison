data{
  int<lower=0> Ntotal;
  int<lower=0> Nsubj;
  int<lower=0> Y[Ntotal];
  int<lower=0> s[Ntotal];
}

parameters{
  real<lower=0,upper=1> theta[Nsubj];
}

model{
  for( i in 1:Ntotal ){
    Y[i] ~ bernoulli(theta[s[i]]);
  }
  for( i in 1:Nsubj ){
    theta[i] ~ beta(2,2);
  }
}

generated quantities{
  real diff;
  diff = theta[1] - theta[2];
}
