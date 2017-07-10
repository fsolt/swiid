library(tidyverse)
library(stringr)
library(rstan)
library(beepr)

load("data/ineq.rda")

seed <- 324
iter <- 1000
chains <- 4
cores <- chains
gt <- 0

x <- ineq %>%  
  filter(k_bl_obs > gt) %>% 
  mutate(kcode = as.integer(factor(country, levels = unique(country))),  # redo codes for filtered sample
         tcode = as.integer(year - min(year) + 1),
         rcode = as.integer(factor(region, levels = unique(region))),
         scode = as.integer(factor(series, levels = unique(series))),
         wecode = as.integer(factor(wdes, levels = unique(wdes))),
         kwecode = as.integer(factor(100*kcode+wecode)),
         wcode = as.integer(factor(welfare_def) %>% forcats::fct_relevel(baseline_wd)),
         ecode = as.integer(factor(equiv_scale) %>% forcats::fct_relevel(baseline_es)))

x_countries <- unique(x$country)
x_wecodes <- x %>%
  select(wdes, wecode, wcode, ecode) %>% 
  distinct() 

rho_we <- rho_we %>% 
  filter(country %in% x_countries) %>% 
  select(-matches("code")) %>% 
  left_join(x %>% select("country", "year", "kcode", "tcode", "rcode") %>% distinct(),
            by = c("country", "year")) %>% 
  left_join(x_wecodes, by = "wdes") %>% 
  mutate(kwecode = as.integer(factor(100*kcode+wecode)))

# Format data for Stan
source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      R = max(x$rcode),
                      S = max(x$scode),
                      WE = max(x$wecode),
                      KWE = max(rho$kwecode),
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
                      rho_we_se = rho_we$rho_se
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

# save(out1, file = str_c("data/all_lis_gt", gt, iter/1000, "k_", 
                        # str_replace(Sys.time(), " ", "_") %>% str_replace("2017-", ""), ".rda"))

beep() # chime


# Plots
plot_tscs(x, out1, save_pdf = "paper/figures/ts_all_lis.pdf")
plot_tscs(x, out1)
