data{
  int<lower=0> N;
  int<lower=0> Y[N];
}

parameters{
  real<lower=0,upper=1> theta;
}

model{
  for( i in 1:N ){
    Y[i] ~ bernoulli(theta);
  }
  theta ~ beta(1,1);
}
