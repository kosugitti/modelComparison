data{
  int<lower=0> N;
  int Y[N];
}

parameters{
  real<lower=0,upper=1> m;
  real<lower=0,upper=1> theta1;
  real<lower=0,upper=1> theta2;
}

model{
  for(n in 1:N){
    //多分二つ使っちゃってるからダメ
    Y[n] ~ bernoulli(theta1);
    Y[n] ~ bernoulli(theta2);
  }

  target += log_sum_exp(
    bernoulli_lpmf(1|m) + beta_lpdf(theta1|0.25*(12-2)+1,(1-0.25)*(12-2)+1),
    bernoulli_lpmf(0|m) + beta_lpdf(theta2|0.75*(12-2)+1,(1-0.75)*(12-2)+1)
  );
  
}
