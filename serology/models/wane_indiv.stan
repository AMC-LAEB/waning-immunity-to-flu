functions {
  real cdf_diff(real diff, real sigma){
    if(diff<0){
      return(normal_cdf(diff+1, 0, sigma)-normal_cdf(diff, 0, sigma));
    }
    else{
      return(normal_cdf(-diff, 0, sigma)-normal_cdf(-(diff+1), 0, sigma));
    }
  }

  real myprob(real meas, real mn_pred, real sigma){
    real diff=meas-mn_pred;
    if(meas==0){
      return(log(normal_cdf(1, mn_pred, sigma)));
    }	  
    else if(meas==8){
      return(log(normal_cdf(mn_pred-diff, mn_pred, sigma)));
    }
    else{
      return(log( cdf_diff(diff, sigma) ));
    }
  }
}

data {
  int <lower = 0> n_data; 
  int <lower = 0> n_strain;
  int <lower = 0> n_subject;
  
  vector[n_data] year;
  vector[n_data] LogTiter;
  int <lower = 0, upper = n_strain> Strain[n_data];
  int <lower = 0, upper = n_subject> Subject[n_data];
}

parameters {
  vector[n_subject] intercepts[n_strain];
  vector[n_subject] alpha[n_strain];
  vector[n_strain] alpha_mn;
  vector<lower = 0>[n_strain] alpha_sd;
  real<lower = 0> sigma;
}

model {
  vector[n_data] y;
  
  for(i in 1:n_strain){
    for(j in 1:n_subject){
      alpha[i][j] ~ normal(alpha_mn[i], alpha_sd[i]);
	}
  }
  
  alpha_mn ~ normal(0, 1);
  alpha_sd ~ normal(0, 1);
  sigma ~ normal(0, 1);
  for(i in 1:n_strain){
    intercepts[i] ~ normal(4, 4);
  }

  for(i in 1:n_data){
    y[i] = intercepts[Strain[i]][Subject[i]]+alpha[Strain[i]][Subject[i]]*year[i];
  }
  
  for(i in 1:n_data){
    target+=myprob(LogTiter[i], y[i], sigma);
  }
}
