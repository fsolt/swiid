library(tidyverse)
library(stringr)
library(rstan)
library(beepr)

load("data/ineq.rda")

seed <- 324
iter <- 100
chains <- 4
cores <- chains

x <- ineq

# Format data for Stan
source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      R = max(x$rcode),
                      
                      W = max(x$wcode),
                      KW = max(rho_wd$kwcode),
                      
                      P = length(rho_wd$rho_wd),
                      kkp = rho_wd$kcode,      
                      rrp = rho_wd$rcode,
                      ttp	= rho_wd$tcode,
                      wdp = rho_wd$wcode,
                      kwp = rho_wd$kwcode,
                      WK = max(rho_wd$wkcode),
                      wkp = rho_wd$wkcode,
                      rho_wd = rho_wd$rho_wd,
                      rho_wd_se = rho_wd$rho_wd_se
)

# Stan
start <- proc.time()
out1 <- stan(file = "R/wd.stan",
             data = source_data,
             seed = seed,
             iter = iter,
             cores = cores,
             chains = chains,
             control = list(max_treedepth = 20,
                            adapt_delta = .8))
runtime <- proc.time() - start
runtime

lapply(get_sampler_params(out1, inc_warmup = FALSE),
       summary, digits = 2)

beep() # chime