
data {
  int<lower=1> K;
  int<lower=1> T;
  int<lower=0> y[K, T];
}

parameters {
  matrix[K, T] z;
  vector[K] beta0_std;
  real<lower=0> sigma; 
  real<lower=0> sigmabeta0;
  real<lower=0,upper=1> gamma; 
}

transformed parameters{
  vector[K] beta0_raw = sigmabeta0 * beta0_std;
  vector[K] beta0 = beta0_raw - mean(beta0_raw);
  matrix[K, T] beta;
  
  beta[,1] = sigma * z[,1];
  
  for (t in 2:T) {
    beta[,t] = gamma * beta[,t-1] + sigma * z[,t];
  }
}

model {
  for (t in 1:T) {
    vector[K] theta = beta0 + beta[,t];
    y[,t] ~ multinomial(softmax(theta));
  }
  
  to_vector(z) ~ normal(0,1);
  sigma ~ normal(0,10);
  
  beta0_std ~ normal(0, 1);
  sigmabeta0 ~ normal(0,10);
  
  gamma ~ beta(1,1);
}

generated quantities{
  matrix[K, T] theta;
  for (t in 1:T) {
    vector[K] eta = beta0 + beta[,t];
    theta[,t] = softmax(eta);
  }
  
}


