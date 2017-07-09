library(tidyverse)
library(stringr)
library(rstan)
library(beepr)

load("data/ineq.rda")

seed <- 324
iter <- 10
chains <- 1
cores <- chains
gt <- 3

xx <- ineq %>%  
  filter(k_bl_obs > gt) %>% 
  mutate(kcode = as.integer(factor(country, levels = unique(country))), # redo codes for filtered sample
         tcode = as.integer(year - min(year) + 1),
         rcode = as.integer(factor(region, levels = unique(region))),
         scode = as.integer(factor(series, levels = unique(series))))

x_countries <- unique(xx$country)

rho <- rho_obs %>% 
  filter(country %in% x_countries)

rho_obl <- rho %>% 
  distinct(country, wdes) %>% 
  mutate(rho_obl = TRUE)

x <- xx %>% 
  left_join(rho_obl, by = c("country", "wdes")) %>% 
  mutate(rho_obl = if_else(!is.na(gini_b), TRUE, rho_obl)) %>% 
  filter(!is.na(rho_obl))

# Format data for Stan
source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      R = max(x$rcode),
                      S = max(x$scode),
                      WE = max(x$wecode),
                      W = max(x$wcode),
                      E = max(x$ecode),
                      N = length(x$gini_m),
                      N_bl = length(x$gini_b[!is.na(x$gini_b)]),
                      N_obl = length(x$s_bl_obs[x$s_bl_obs>0]),
                      kk = x$kcode,
                      tt = x$tcode,
                      rr = x$rcode,
                      ss = x$scode,
                      wen = x$wecode,
                      gini_m = x$gini_m,
                      gini_m_se = x$gini_m_se,
                      gini_b = x$gini_b[!is.na(x$gini_b)],
                      gini_b_se = x$gini_b_se[!is.na(x$gini_b_se)],
                      
                      M = length(rho$rho),
                      kkm = rho$kcode,      
                      rrm = rho$rcode,
                      ttm	= rho$tcode,
                      wem = rho$wecode,
                      rho_we = rho$rho,
                      rho_we_se = rho$rho_se
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

save(out1, file = str_c("data/all_lis_gt", gt, iter/1000, "k_", 
                        str_replace(Sys.time(), " ", "_") %>% str_replace("2017-", ""), ".rda"))

beep() # chime


# Plots
plot_tscs(x, out1, save_pdf = "paper/figures/ts_all_lis.pdf")
plot_tscs(x, out1)
