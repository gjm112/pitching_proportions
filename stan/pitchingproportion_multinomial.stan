
data {
  int<lower=1> K;
  int<lower=1> T;
  int<lower=0> y[30,K,T];
  int<lower=0> n[30,T];
  int<lower=1> N;
  int<lower=1> all_ind[N];
  int<lower=1> ind_start[T];
  int<lower=1> ind_end[T];
}


parameters {
  matrix[K, T] z;
  vector[K] beta0_std;
  real<lower=0> sigma; 
  real<lower=0> sigmabeta0;
  real<lower=0,upper=1> gamma;  
}

transformed parameters {
  vector[K] beta0 = sigmabeta0 * beta0_std;
  beta0 = beta0 - mean(beta0);
  
  matrix[K, T] beta;
  
  beta[,1] = beta[,1] = sigma * z[,1];
  
  for (t in 2:T) {
    beta[,t] = gamma * beta[,t-1] + sigma * z[,t];
  }
}

model {
  
  for (t in 1:T) {
    vector[K] probs = softmax(beta0 + beta[,t]);
    
    for (i in ind_start[t]:ind_end[t]) {
      int team = all_ind[i];
      y[team, 1:K, t] ~ multinomial(probs);
    }
  }

// Priors

  to_vector(z) ~ normal(0, 1);
  beta0_std ~ normal(0, 2);
  sigma ~ normal(0, 0.5);
  sigmabeta0 ~ normal(0, 5);
  gamma_un ~ beta(1,1);
}

generated quantities {
  matrix[K, T] theta;
  for (t in 1:T) {
    theta[,t] = softmax(beta0 + beta[,t]);
  }
  
}



