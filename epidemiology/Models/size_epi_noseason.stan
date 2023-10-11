data {
  int <lower = 0> n; 
  int n_season;
  vector[n] rel_size;
  vector[n] y_since_prev;
  int <lower = 0, upper = n_season> season[n];  
}

parameters {

  real alpha;
  real beta; 
  real<lower = 0> sigma_y;
  
}

model {
  vector[n] y;
  
  y = alpha + beta * y_since_prev;
  beta ~ normal(0, 1);
  sigma_y ~ normal(0, 1);
  alpha ~ normal(0.5, 1);

  rel_size ~ normal(y, sigma_y);
}

generated quantities {
  vector[n] log_lik;
  for (i in 1:n){
    log_lik[i] = normal_lpdf(rel_size[i] | alpha + beta * y_since_prev[i], sigma_y);
  }
}
