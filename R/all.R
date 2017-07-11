library(tidyverse)
library(stringr)
library(rstan)
library(beepr)

load("data/ineq.rda")

seed <- 324
iter <- 1000
chains <- 4
cores <- chains

x <- ineq 



# Format data for Stan
source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      R = max(x$rcode),
                      S = max(x$scode),
                      WE = max(x$wecode),
                      KWE = max(rho_we$kwecode),
                      W = max(x$wcode),
                      KW = max(rho_wd$kwcode),
                      E = max(x$ecode),
                      KE = max(rho_es$kecode),
                      N = length(x$gini_m),
                      N_bl = length(x$gini_b[!is.na(x$gini_b)]),
                      N_obl = length(x$s_bl_obs[x$s_bl_obs>0]),
                      N_kbl = length(x$k_bl_obs[x$k_bl_obs>0]),
                      N_kk = ,
                      N_kr = ,
                      N_rk = ,
                      
                      kk = x$kcode,
                      tt = x$tcode,
                      rr = x$rcode,
                      ss = x$scode,
                      wen = x$wecode,
                      kwen = x$kwecode,
                      gini_m = x$gini_m,
                      gini_m_se = x$gini_m_se,
                      gini_b = x$gini_b[!is.na(x$gini_b)],
                      gini_b_se = x$gini_b_se[!is.na(x$gini_b_se)],
                      
                      M = length(rho_we$rho),
                      kkm = rho_we$kcode,      
                      rrm = rho_we$rcode,
                      ttm	= rho_we$tcode,
                      wem = rho_we$wecode,
                      kwem = rho_we$kwecode,
                      rho_we = rho_we$rho,
                      rho_we_se = rho_we$rho_se,
                      
                      P = length(rho_wd$rho_wd),
                      kkp = rho_wd$kcode,      
                      rrp = rho_wd$rcode,
                      ttp	= rho_wd$tcode,
                      wdp = rho_wd$wcode,
                      kwp = rho_wd$kwcode,
                      rho_wd = rho_wd$rho_wd,
                      rho_wd_se = rho_wd$rho_wd_se,
                      
                      Q = length(rho_es$rho_es),
                      kkq = rho_es$kcode,      
                      rrq = rho_es$rcode,
                      ttq	= rho_es$tcode,
                      esq = rho_es$ecode,
                      keq = rho_es$kecode,
                      rho_es = rho_es$rho_es,
                      rho_es_se = rho_es$rho_es_se
)

# Stan
start <- proc.time()
out1 <- stan(file = "R/all_lis.stan",
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

save(out1, file = str_c("data/all_", iter/1000, "k_",
                        str_replace(Sys.time(), " ", "_") %>% str_replace("2017-", ""), ".rda"))

beep() # chime


# Plots
plot_tscs(x, out1, save_pdf = "paper/figures/ts_all.pdf")
plot_tscs(x, out1)
