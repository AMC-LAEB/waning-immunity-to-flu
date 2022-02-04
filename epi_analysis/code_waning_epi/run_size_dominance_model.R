library(rstan)
library(loo)


epi = read.csv("~/flu_waning/viro_data.csv")
ili = read.csv("~/flu_waning/ili_data.csv")
byyear = merge(epi,ili,by=c("Season","Country"))


data = byyear[!(is.na(as.numeric(byyear$Y_SINCE_LAST_H3))) & !(is.na(as.numeric(byyear$H3_REL))),]
d = list()
d$n = nrow(data)
d$n_season = length(unique(data$Season))
d$rel_size = as.numeric(data$H3_REL)
d$y_since_prev = as.numeric(data$Y_SINCE_LAST_H3) - 1
d$season = data$Season - min(data$Season) + 1


size_model_h3 <- stan(
  file = "~/flu_waning/size_epi_model.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,                  
)

log_lik_size_model_h3 <- extract_log_lik(size_model_h3, merge_chains = FALSE)
r_eff_size_model_h3 <- relative_eff(exp(log_lik_size_model_h3))
loo_size_model_h3 <- loo(log_lik_size_model_h3, r_eff = r_eff_size_model_h3, cores = 4)

size_model_h3_noseason <- stan(
  file = "~/flu_waning/size_epi_noseason.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,                  
)

log_lik_size_model_h3_noseason <- extract_log_lik(size_model_h3_noseason, merge_chains = FALSE)
r_eff_size_model_h3_noseason <- relative_eff(exp(log_lik_size_model_h3_noseason))
loo_size_model_h3_noseason <- loo(log_lik_size_model_h3_noseason, r_eff = r_eff_size_model_h3_noseason, cores = 4)


data = byyear[!(is.na(as.numeric(byyear$Y_SINCE_LAST_H1pdm))) & !(is.na(as.numeric(byyear$H3_REL))),]
d = list()
d$n = nrow(data)
d$n_season = length(unique(data$Season))
d$rel_size = as.numeric(data$H3_REL)
d$y_since_prev = as.numeric(data$Y_SINCE_LAST_H1pdm) - 1
d$season = data$Season - min(data$Season) + 1


size_model_h3_othersubtype <- stan(
  file = "~/flu_waning/size_epi_model.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,                  
)


size_model_h3_noseason_othersubtype <- stan(
  file = "~/flu_waning/size_epi_noseason.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,                  
)



h3_compare <- loo_compare(loo_size_model_h3, loo_size_model_h3_noseason)

















data = byyear[!(is.na(as.numeric(byyear$Y_SINCE_LAST_H1pdm))) & !(is.na(as.numeric(byyear$H1pdm_REL))),]
d = list()
d$n = nrow(data)
d$n_season = length(unique(data$Season))
d$rel_size = as.numeric(data$H1pdm_REL)
d$y_since_prev = as.numeric(data$Y_SINCE_LAST_H1pdm) - 1
d$season = data$Season - min(data$Season) + 1


size_model_h1pdm <- stan(
  file = "~/flu_waning/size_epi_model.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,                  
)

log_lik_size_model_h1pdm <- extract_log_lik(size_model_h1pdm, merge_chains = FALSE)
r_eff_size_model_h1pdm <- relative_eff(exp(log_lik_size_model_h1pdm))
loo_size_model_h1pdm <- loo(log_lik_size_model_h1pdm, r_eff = r_eff_size_model_h1pdm, cores = 4)

size_model_h1pdm_noseason <- stan(
  file = "~/flu_waning/size_epi_noseason.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,                  
)

log_lik_size_model_h1pdm_noseason <- extract_log_lik(size_model_h1pdm_noseason, merge_chains = FALSE)
r_eff_size_model_h1pdm_noseason <- relative_eff(exp(log_lik_size_model_h1pdm_noseason))
loo_size_model_h1pdm_noseason <- loo(log_lik_size_model_h1pdm_noseason, r_eff = r_eff_size_model_h1pdm_noseason, cores = 4)

data = byyear[!(is.na(as.numeric(byyear$Y_SINCE_LAST_H3))) & !(is.na(as.numeric(byyear$H1pdm_REL))),]
d = list()
d$n = nrow(data)
d$n_season = length(unique(data$Season))
d$rel_size = as.numeric(data$H1pdm_REL)
d$y_since_prev = as.numeric(data$Y_SINCE_LAST_H3) - 1
d$season = data$Season - min(data$Season) + 1

size_model_h1pdm_othersubtype <- stan(
  file = "~/flu_waning/size_epi_model.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,                  
)

size_model_h1pdm_noseason_othersubtype <- stan(
  file = "~/flu_waning/size_epi_noseason.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,                  
)


h1pdm_compare <- loo_compare(loo_size_model_h1pdm, loo_size_model_h1pdm_noseason)









data = byyear[!(is.na(as.numeric(byyear$Y_SINCE_LAST_B))) & !(is.na(as.numeric(byyear$B_REL))),]
d = list()
d$n = nrow(data)
d$n_season = length(unique(data$Season))
d$rel_size = as.numeric(data$B_REL)
d$y_since_prev = as.numeric(data$Y_SINCE_LAST_B) - 1
d$season = data$Season - min(data$Season) + 1

size_model_b <- stan(
  file = "~/flu_waning/size_epi_model.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,                  
)

log_lik_size_model_b <- extract_log_lik(size_model_b, merge_chains = FALSE)
r_eff_size_model_b <- relative_eff(exp(log_lik_size_model_b))
loo_size_model_b <- loo(log_lik_size_model_b, r_eff = r_eff_size_model_b, cores = 4)

size_model_b_noseason <- stan(
  file = "~/flu_waning/size_epi_noseason.stan",  
  data = d,    
  chains = 4,             
  warmup = 1000,          
  iter = 3000,                  
)

log_lik_size_model_b_noseason <- extract_log_lik(size_model_b_noseason, merge_chains = FALSE)
r_eff_size_model_b_noseason <- relative_eff(exp(log_lik_size_model_b_noseason))
loo_size_model_b_noseason <- loo(log_lik_size_model_b_noseason, r_eff = r_eff_size_model_b_noseason, cores = 4)


b_compare <- loo_compare(loo_size_model_b, loo_size_model_b_noseason)






