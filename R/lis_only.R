library(rstan)
library(beepr)

seed <- 324
iter <- 1000
chains <- 4
cores <- chains

x <- ineq %>% 
  filter(series == "LIS disp sqrt") %>% 
  mutate(kcode = as.integer(factor(country, levels = unique(country))), # redo codes for filtered sample
         tcode = as.integer(year - min(year) + 1)) 

source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      N = length(x$gini_m),
                      N_b = length(x$gini_b[!is.na(x$gini_b)]),
                      kk = x$kcode,
                      tt = x$tcode,
                      gini_m = x$gini_m,
                      gini_m_se = x$gini_m_se,
                      gini_b = x$gini_b[!is.na(x$gini_b)],
                      gini_b_se = x$gini_b_se[!is.na(x$gini_b_se)]
)

start <- proc.time()
out1 <- stan(file = "R/lis_only.stan",
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

# Chime
beep()