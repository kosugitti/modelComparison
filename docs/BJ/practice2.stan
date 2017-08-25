data{
  int<lower=0> N;
  real X[N];
  real Y[N];
}

parameters{
  real beta0;
  real beta1;
  real<lower=0> sig;
}

model{
  for(i in 1:N){
    Y[i] ~ normal(beta0+beta1*X[i],sig);
  }
  beta0 ~ uniform(-100,100);
  beta1 ~ uniform(-100,100);
  sig ~ student_t(4,0,5);
}
