//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int<lower=0> K;
  int<lower=0> T;
  int<lower=0> y[N,K,T];
  int<lower=0> n[N,T];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real beta[K,T];
  real beta0[K];
  real<lower=0> sigma; 
  real<lower=0> sigmabeta0;
  real<lower=0, upper=1> gamma; 
  // Standard deviation
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for (t in 1:T){
   for (i in 1:N){
    for (j in 1:K){
    y[i,j,t] ~ binomial(n[i,t], inv_logit(beta0[j] + beta[j,t]));
    } 
  }
}
   
    // Priors
for (t in 2:T){
  for (j in 1:K){
  beta[j,t] ~ normal(gamma*beta[j,t-1], sigma);       // Normal prior on coefficients
  }
}

for (j in 1:K){
  beta[j,1] ~ normal(0, sigma);       // Normal prior on coefficients
  beta0[j] ~ normal(0, sigmabeta0);
}



  sigma ~ normal(0, 10);
  sigmabeta0 ~ normal(0, 10);
  
  // Half-normal prior
  gamma ~ uniform(0, 1);






  
  
}




