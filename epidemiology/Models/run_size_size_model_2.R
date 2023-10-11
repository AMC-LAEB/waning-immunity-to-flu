epi = read.csv("Data/viro_data.csv")
ili = read.csv("Data/ili_data.csv")
byyear = merge(epi,ili,by=c("Season","Country"))
library(rstan)
library(loo)


data = byyear[!(is.na(as.numeric(byyear$H3_REL_LAG2))) & !(is.na(as.numeric(byyear$H3_REL))),]
d = list()
d$n = nrow(data)
d$n_season = length(unique(data$Season))
d$rel_size = as.numeric(data$H3_REL)
d$y_since_prev = as.numeric(data$H3_REL_LAG2)
d$season = data$Season - min(data$Season) + 1

size_size_model_2_h3 <- stan(
  file = "Models/size_epi_model.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,               
)

log_lik_size_size_model_2_h3 <- extract_log_lik(size_size_model_2_h3, merge_chains = FALSE)
r_eff_size_size_model_2_h3 <- relative_eff(exp(log_lik_size_size_model_2_h3))
loo_size_size_model_2_h3 <- loo(log_lik_size_size_model_2_h3, r_eff = r_eff_size_size_model_2_h3, cores = 4)

size_size_model_2_h3_noseason <- stan(
  file = "Models/size_epi_noseason.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,               
)

log_lik_size_size_model_2_h3_noseason <- extract_log_lik(size_size_model_2_h3_noseason, merge_chains = FALSE)
r_eff_size_size_model_2_h3_noseason <- relative_eff(exp(log_lik_size_size_model_2_h3_noseason))
loo_size_size_model_2_h3_noseason <- loo(log_lik_size_size_model_2_h3_noseason, r_eff = r_eff_size_size_model_2_h3_noseason, cores = 4)

h3_compare_size_size_2 <- loo_compare(loo_size_size_model_2_h3, loo_size_size_model_2_h3_noseason)


















data = byyear[!(is.na(as.numeric(byyear$H1pdm_REL_LAG2))) & !(is.na(as.numeric(byyear$H1pdm_REL))),]

d = list()
d$n = nrow(data)
d$n_season = length(unique(data$Season))
d$rel_size = as.numeric(data$H1pdm_REL)
d$y_since_prev = as.numeric(data$H1pdm_REL_LAG2)
d$season = data$Season - min(data$Season) + 1

size_size_model_2_h1pdm <- stan(
  file = "Models/size_epi_model.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,               
)

log_lik_size_size_model_2_h1pdm <- extract_log_lik(size_size_model_2_h1pdm, merge_chains = FALSE)
r_eff_size_size_model_2_h1pdm <- relative_eff(exp(log_lik_size_size_model_2_h1pdm))
loo_size_size_model_2_h1pdm <- loo(log_lik_size_size_model_2_h1pdm, r_eff = r_eff_size_size_model_2_h1pdm, cores = 4)

size_size_model_2_h1pdm_noseason <- stan(
  file = "Models/size_epi_noseason.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,               
)

log_lik_size_size_model_2_h1pdm_noseason <- extract_log_lik(size_size_model_2_h1pdm_noseason, merge_chains = FALSE)
r_eff_size_size_model_2_h1pdm_noseason <- relative_eff(exp(log_lik_size_size_model_2_h1pdm_noseason))
loo_size_size_model_2_h1pdm_noseason <- loo(log_lik_size_size_model_2_h1pdm_noseason, r_eff = r_eff_size_size_model_2_h1pdm_noseason, cores = 4)

h1pdm_compare_size_size_2 <- loo_compare(loo_size_size_model_2_h1pdm, loo_size_size_model_2_h1pdm_noseason)












data = byyear[!(is.na(as.numeric(byyear$B_REL_LAG2))) & !(is.na(as.numeric(byyear$B_REL))),]
d = list()
d$n = nrow(data)
d$n_season = length(unique(data$Season))
d$rel_size = as.numeric(data$B_REL)
d$y_since_prev = as.numeric(data$B_REL_LAG2)
d$season = data$Season - min(data$Season) + 1

library(rstan)
size_size_model_2_b <- stan(
  file = "Models/size_epi_model.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,               
)

log_lik_size_size_model_2_b <- extract_log_lik(size_size_model_2_b, merge_chains = FALSE)
r_eff_size_size_model_2_b <- relative_eff(exp(log_lik_size_size_model_2_b))
loo_size_size_model_2_b <- loo(log_lik_size_size_model_2_b, r_eff = r_eff_size_size_model_2_b, cores = 4)


size_size_model_2_b_noseason <- stan(
  file = "Models/size_epi_noseason.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,               
)


log_lik_size_size_model_2_b_noseason <- extract_log_lik(size_size_model_2_b_noseason, merge_chains = FALSE)
r_eff_size_size_model_2_b_noseason <- relative_eff(exp(log_lik_size_size_model_2_b_noseason))
loo_size_size_model_2_b_noseason <- loo(log_lik_size_size_model_2_b_noseason, r_eff = r_eff_size_size_model_2_b_noseason, cores = 4)


b_compare_size_size_2 <- loo_compare(loo_size_size_model_2_b, loo_size_size_model_2_b_noseason)







