################################################################################
# This script draws samples from the joint posterior distribution of parameters
# err_meas (measurement error), m (waning rate) and c_i (the set of individual
# specific intercepts).
#
# Data:
# time series t=(t1, t2 ... tn), with measured values y*=(y1*, y2*, ... yn*) and 
# true values y=(y1, y2 ... yn). Repeated for N people and up to 2 replicates.
#
# Details of the likelihood function can be found in the SI
################################################################################

################################################################################
# Receive user input data set and number of iterations
################################################################################
args <- commandArgs(trailingOnly = TRUE)
cohort<-as.character(args[1])
batch<-as.character(args[2])
yrs<-as.character(args[3])
mcmc.size<-as.numeric(args[4])
burn<-as.numeric(args[5])
thread<-as.numeric(args[6]) # manually input thread name

fn<-paste0("processed_data/", cohort, "/dat_", yrs, "_", batch, ".RData")
load(fn)

data<-paste(cohort, yrs, batch, sep="_")

cat(paste0("\nData: ", data, "\nIterations: ", mcmc.size, "\n\n"))


################################################################################
# Load libraries and data
################################################################################

suppressPackageStartupMessages({
  library(tidyr)
  library(plyr)
  library(dplyr)
  library(reshape2)
})

#load(paste0(data, ".RData"))

################################################################################
# Measurement model ------------------------------------------------------------
################################################################################

# Area under Gaussian distribution, ~N(m, a), between x and x+diff. This is
# calculated using the difference between the cumulative function. When dealing
# with numbers close to zero, the lower tail is used to maximise numerical
# precision.
cdfdiff<-function(x, diff, m, a)
{
  if(x<m)
  {
    return(pnorm(x+diff, mean=m, sd=a, lower.tail = T)-pnorm(x, mean=m, sd=a, lower.tail = T))
  }
  return(pnorm(x, mean=m, sd=a, lower.tail = F)-pnorm(x+diff, mean=m, sd=a, lower.tail = F))
}

# Find log likelihood of one data point (meas) given the apparent true value is 
# normally distributed with mean (true) and a standard deviation (a). lo_lim and
# hi_lim are the min and max dilutions used in the experiment.
err<-function(true, meas, a, lo_lim, hi_lim)
{
  # If <lo_lim, apparent titer could be anything from -inf to lo_lim
  if(meas<lo_lim)
  {
    return(log(pnorm(lo_lim, mean=true, sd=a)) ) 
  }
  # If >hi_lim, titer could be anything from hi_lim to +inf
  if(meas>=hi_lim)
  {
    return(log(pnorm(hi_lim, mean=true, sd=a, lower.tail = F)))
  }
  # Else, find probability between meas and meas+1
  return(log(cdfdiff(meas, 1, true, a)))
}

# Log likelihood of measuring meas.vec, given true values tru.vec
prob_meas<-function(tru.vec, meas.vec, a, lo_lim, hi_lim)
{
  return(sum(mapply(FUN=err, true=tru.vec, meas=meas.vec, a=a, lo_lim=lo_lim, hi_lim=hi_lim)))
}

################################################################################
# MCMC functions ---------------------------------------------------------------
################################################################################

# Master object, mast, has components
#
# strain - strain specific data and parameters ---------------------------------
## dat              data
## N                number of participants
## nt               number of timepoints per participant
## grads            vector of grad MCMC values
## inters           matrix of intercept MCMC values (vector per patient)
## current.grad     current value of gradient in the MCMC routine
## current.inters   vector of current values of individual intercepts in MCMC
## jump.grad        step size for gradient
## jump.inter       step size for intercepts
## acc.grad         accepted grad jumps
## acc.inter        accepted inter jumps
## trial.grad       attempted grad jumps
## trial.inter      attempted inter jumps
#
# err - error parameters -------------------------------------------------------
## errs             vector of error MCMC values
## current.err      current value of error in MCMC routine
## jump.err         step size for error
## acc.err          accepted error jumps
## trial.err        attempted error jumps
#
# lik - log likelihood values --------------------------------------------------

# Propose parameter given current, cur, and random jump, rand.jump
propose.par <- function(cur, rand.jump, delta)
{
  return(cur + (0.5-rand.jump)*delta)
}

# Log likelihood of grad and err given data
log.lik<-function(grad, err, obj)
{
  dat<-obj$dat
  
  inters<-obj$current.inters
  # copy intercepts for each timepoint to allow vector addition below
  inters<-rep(inters, each=obj$nt)
  
  # calculate true titer values implied by grad and inters
  dat$true.vec<-grad*dat$t+inters
  dat<-na.omit(dat)
  
  # return log likelihood
  return(prob_meas(dat$true.vec, dat$y, err, 1, 10))
}

# Update individual intercepts via MH, iteration number i
update.inters<-function(mast, strain, i)
{
  obj<-mast$strain[[strain]]
  
  dat<-obj$dat
  N<-obj$N
  nt<-obj$nt
  grad<-obj$current.grad
  current.inters<-obj$current.inters
  jump.inter<-obj$jump.inter
  
  err<-mast$err$current.err
  
  # Only change intercepts for random sample of 10%
  ids<-sample(1:N, round(0.1*N))
  
  # For each individual, suggest new values of intercept and accept/reject with
  # MH step
  for(j in ids)
  {
    subdat<-na.omit(dat[(nt*j-(nt-1)):(nt*j),])
    
    t<-subdat$t
    meas.vec<-subdat$y
    
    inter<-current.inters[j]
    tru.vec<-grad*t+inter
    
    new.inter<-propose.par(inter, runif(1), jump.inter)
    tru.vec.new<-grad*t+new.inter
    
    lik<-prob_meas(tru.vec, meas.vec, err, 1, 10)
    lik.new<-prob_meas(tru.vec.new, meas.vec, err, 1, 10)
    
    # Determine if new value is accepted
    log.acc = lik.new-lik
    if (log(runif(1)) < log.acc)
    {
      current.inters[j] <- new.inter
      # Increment number of accepted steps for model optimization
      if(i>mast$burn)
      {
        obj$acc.inter<-obj$acc.inter+1
      }
    }
    # Increment number of trial steps for model optimization
    if(i>mast$burn)
    {
      obj$trial.inter<-obj$trial.inter+1
    }
  }
  # Update log of intercepts
  obj$inters[i,]<-current.inters
  # Update current intercepts
  obj$current.inters<-current.inters
  
  mast$strain[[strain]]<-obj
  
  return(mast)
}

# Update gradient via MH, iteration number i
update.grad<-function(mast, strain, i)
{
  obj<-mast$strain[[strain]]
  
  err<-mast$err$current.err
  grad<-obj$current.grad
  new.grad<-propose.par(grad, runif(1), obj$jump.grad)
  
  # Set some limits on explored parameter space
  if(new.grad<1 & new.grad>(-1))
  {
    lik<-log.lik(grad, err, obj)
    lik.new<-log.lik(new.grad, err, obj)
    
    # Determine if new value is accepted
    log.acc <- lik.new-lik
    if (log(runif(1)) < log.acc)
    {
      grad<-new.grad
      # Increment number of accepted steps for model optimization
      if(i>mast$burn)
      {
        obj$acc.grad<-obj$acc.grad+1
      }
    }
    # Increment number of trials for model optimization
    if(i>mast$burn)
    {
      obj$trial.grad<-obj$trial.grad+1
    }
  }
  # Update log of gradients
  obj$grads[i]<-grad
  
  # Update log of intercepts
  obj$current.grad<-grad
  
  mast$strain[[strain]]<-obj
  
  return(mast)
}

# Update error given all objects
update.err<-function(mast, i)
{
  objs<-mast$strain
  current.err<-mast$err$current.err
  jump.err<-mast$err$jump.err
  errs<-mast$err$errs
  
  new.err<-propose.par(current.err, runif(1), jump.err)
  
  # error must be positive
  if(new.err<10 & new.err>0)
  {
    lik<-0
    lik.new<-0
    for(strain in 1:length(objs))
    {
      dat<-objs[[strain]]
      lik<-lik+log.lik(dat$current.grad, current.err, dat)
      lik.new<-lik.new+log.lik(dat$current.grad, new.err, dat)
    }
    
    # Since this is the last step, take stock of the likelihood
    lik.vec<-mast$lik
    lik.vec[i]<-lik
    
    # The log acceptance ratio = the log likelihood ratio
    log.acc <- lik.new-lik
    # Metropolis step for alpha: determine whether the proposal is accepted
    if (log(runif(1)) < log.acc)
    {
      current.err<-new.err
      lik.vec[i]<-lik.new
      
      if(i>mast$burn)
      {
        mast$err$acc.err<-mast$err$acc.err+1
      }
    }
    if(i>mast$burn)
    {
      mast$err$trial.err<-mast$err$trial.err+1
    }
  }
  
  errs[i]<-current.err
  
  mast$err$current.err<-current.err
  mast$err$errs<-errs
  mast$lik<-lik.vec
  
  return(mast)
}

chain_hierarchical = function(dats, params)
{
  #################################################################
  # Step (a) (b): Reserve space for the MCMC samples and initialize
  #               Also build master object
  #################################################################
  
  # Get parameters
  jump.grad.sizes<-params$grad.jump
  jump.inter.sizes<-params$inter.jump
  jump.err<-params$err.jump
  mcmc.size<-params$mcmc.size
  burn<-params$burn
  
  # obj.strain[[i]] contains parameters for each strain, i ---------------------
  grad0<-runif(1,-0.5, 0.5)               # Random initial value of gradient
  
  obj.strain<-list()
  for(i in 1:length(dats))
  {
    obj.strain[[i]]<-list()
    
    # data ---------------------------------------------------------------------
    subdat<-dats[[i]]
    obj.strain[[i]]$dat<-subdat
    N<-nlevels(as.factor(subdat$id))
    obj.strain[[i]]$N<-N                             # number of people
    obj.strain[[i]]$nt<-nrow(subdat)/N               # number of timepoints per person
    
    # initialize accepted and trial --------------------------------------------
    obj.strain[[i]]$acc.inter<-0
    obj.strain[[i]]$trial.inter<-0
    
    obj.strain[[i]]$acc.grad<-0                         
    obj.strain[[i]]$trial.grad<-0  
    
    # intercepts ---------------------------------------------------------------
    inters<-matrix(0, nrow=mcmc.size, ncol=N)
    # random initial values=individual means + random noise
    i0<-subdat%>%group_by(id)%>%summarise(y=mean(y, na.rm=T))
    inters[1,]<-i0$y+runif(N, 0, 1)
    obj.strain[[i]]$inters<-inters
    obj.strain[[i]]$current.inters<-inters[1,]
    
    # gradients ----------------------------------------------------------------
    grads<-rep(0, mcmc.size)
    grads[1]<-grad0
    obj.strain[[i]]$grads<-grads
    obj.strain[[i]]$current.grad<-grads[1]
    
    # jump sizes ---------------------------------------------------------------
    obj.strain[[i]]$jump.inter<-jump.inter.sizes[i]
    obj.strain[[i]]$jump.grad<-jump.grad.sizes[i]
  }
  
  # errors object --------------------------------------------------------------
  err0<-runif(1,0.2, 1)                       # Random initial value of error
  
  obj.err<-list()
  
  # errors
  errs<-rep(0, mcmc.size)
  errs[1]<-err0
  obj.err$errs<-errs
  obj.err$current.err<-errs[1]
  
  # jump size
  obj.err$jump.err<-jump.err
  
  # initialize accepted and trials
  obj.err$acc.err<-0                         
  obj.err$trial.err<-0 
  
  # likelihood log -------------------------------------------------------------
  likelihood<-rep(0, mcmc.size)
  likelihood[1]<-NA
  
  # Master object --------------------------------------------------------------
  mast<-list()
  mast$strain<-obj.strain
  mast$err<-obj.err
  mast$lik<-likelihood
  # burn in iterations, acceptance rates calculated past this point
  mast$burn<-burn
  
  ###################
  # Draw MCMC samples
  ###################
  
  t0<-Sys.time()
  verb<-round(mcmc.size/10)         # user output every verb iterations
  
  for (i in 2:mcmc.size)
  {
    if(i%%verb==0)
    {
      cat(i, "\t")
      tv<-Sys.time()
      print(difftime(tv, t0))
      
      cat("\nAcceptance:\n\n")
      
      if(i>mast$burn)
      {
        for(strain in 1:length(mast$strain))
        {
          acc<-mast$strain[[strain]]$acc.grad
          tri<-mast$strain[[strain]]$trial.grad
          cat(paste0("strain #", strain, ": "))
          cat(paste0("grad: ", acc, "/", tri, "(", round(acc*100/tri), "%)\t"))
          acc<-mast$strain[[strain]]$acc.inter
          tri<-mast$strain[[strain]]$trial.inter
          cat(paste0("inters:", acc, "/", tri, "(", round(acc*100/tri), "%)\n"))
        }
        
        acc<-mast$err$acc.err
        tri<-mast$err$trial.err
        cat(paste0("\nerr: ", acc, "/", mast$err$trial.err, "(", round(acc*100/tri), "%)\n\n"))
      }
      else
      {
        cat("pre- burn in, wait for fitting performance...\n")
      }
    }
    
    for(strain in 1:length(mast$strain))
    {
      #########################################################
      # Step (c): Update the id-specific intercepts via MH step
      #########################################################
      mast<-update.inters(mast, strain, i)
      
      ##################################
      # Step (d) update grad via MH step
      ##################################
      mast<-update.grad(mast, strain, i)
    }
    
    ###################################
    # Step (e) update error via MH step
    ###################################
    mast<-update.err(mast, i)
  }
  
  # The output: the MCMC samples
  return(mast)
}

# MCMC parameters
params<-list()
params$mcmc.size<-mcmc.size               # no. iterations
params$grad.jump<-c(0.1, 0.15, 0.1, 0.1)  # step sizes (per strain) for gradient
params$inter.jump<-c(2, 3, 2, 2)          # step sizes (per strain) for inter
params$err.jump<-0.1                      # step size for error
params$burn<-burn

# Data for 20_21, step sizes need to be different to get 20-30% acceptance.
if(yrs=="20_21")
{
  params$grad.jump<-c(0.4, 0.6, 0.4, 0.4)
  params$inter.jump<-c(3.5, 5, 3.5, 3.5)
  params$err.jump<-0.13
}

# Call the main routine
t1<-Sys.time()
mcmc.sample <- chain_hierarchical(dats, params)
t2<-Sys.time()

print(difftime(t2, t1))

save(mcmc.sample, file=paste0("mcmcout_", data, "_reps_", mcmc.size, "_thread_no_", thread, ".RData"))