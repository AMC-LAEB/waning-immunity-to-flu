data {
  int <lower = 0> n; 
  int n_season; 
  vector[n] rel_size;
  vector[n] y_since_prev;
  int <lower = 0, upper = n_season> season[n];  
}

parameters {

  vector<lower = 0>[n_season] alpha;
  real beta; 
  real<lower = 0> sigma_a;
  real<lower = 0> sigma_y;
  real mu_a;
  
}

model {

  vector[n] y;
  
  y = alpha[season] + beta * y_since_prev;

  beta ~ normal(0, 1);
  sigma_y ~ normal(0, 1);
  sigma_a ~ normal(0, 1);
  alpha ~ normal(mu_a, sigma_a);
  mu_a ~ normal(0.5, 1);
  
  rel_size ~ normal(y, sigma_y);
}

generated quantities {
  vector[n] log_lik;
  for (i in 1:n){
    log_lik[i] = normal_lpdf(rel_size[i] | alpha[season[i]] + beta * y_since_prev[i], sigma_y);
  }
}
