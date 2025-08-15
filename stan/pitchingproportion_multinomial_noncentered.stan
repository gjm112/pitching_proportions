
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
  //matrix[K, T] beta;
  vector[K] beta0_raw;
  real<lower=0> sigma; 
  real<lower=0> sigmabeta0;
  real gamma_un; 
}

transformed parameters {
  matrix[K, T] beta;
  vector[K] beta0;
  matrix[K, T] theta;
  real<lower=0,upper=1> gamma = inv_logit(gamma_un);
  
  // First year
  for (j in 1:K) {
    beta[j,1] = sigma * z[j,1];
  }
  
  // Following years
  for ( t in 2:T) {
    for (j in 1:K) {
      beta[j,t] = gamma * beta[j, t-1] + sigma * z[j,t];
    }
  }
  
  beta0 = sigmabeta0 * beta0_raw;
  
  for (t in 1:T) {
    for (j in 1:K) {
      theta[j, t] = beta0[j] + beta[j, t];
    }
    theta[,t] = softmax(theta[,t]);
  }
}

model {
  
  for (t in 1:T) {
    for (i in ind_start[t]:ind_end[t]) {
      int team = all_ind[i];
      y[team, 1:K, t] ~ multinomial(theta[,t]);
    }
  }

// Priors
to_vector(z) ~ normal(0, 1);
beta0_raw ~ normal(0,1);

//for (t in 2:T) {
//  for (j in 1:K) {
//    beta[j,t] ~ normal(gamma*beta[j,t-1], sigma);
//  }
//}

//for (j in 1:K) {
//  beta[j,1] ~ normal(0, sigma);
//  beta0[j] ~ normal(0, sigmabeta0);
//}

// Hyperpriors
  sigma ~ normal(0, 2);
  sigmabeta0 ~ normal(0, 2);
  gamma_un ~ normal(0, 1);
}



