library(tidyverse)
library(stringr)
library(rstan)
library(beepr)

load("data/ineq.rda")

seed <- 324
iter <- 1000
chains <- 4
cores <- chains
gt <- 3

x <- ineq %>%  
  filter(k_bl_obs > gt) %>% 
  mutate(kcode = as.integer(factor(country, levels = unique(country))), # redo codes for filtered sample
         tcode = as.integer(year - min(year) + 1),
         rcode = as.integer(factor(region, levels = unique(region))),
         scode = as.integer(factor(series, levels = unique(series))))

# Format data for Stan
source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      R = max(x$rcode),
                      S = max(x$scode),
                      W = max(x$wcode),
                      E = max(x$ecode),
                      
                      N = length(x$gini_m),
                      N_b = length(x$gini_b[!is.na(x$gini_b)]),
                      kk = x$kcode,
                      tt = x$tcode,
                      kktt = (x$kcode-1)*max(x$tcode)+x$tcode,
                      ss = x$scode,
                      ktt = rep(1:max(x$tcode), times = max(x$kcode)),
                      ktk = rep(1:max(x$kcode), each = max(x$tcode)),
                      gini_m = x$gini_m,
                      gini_m_se = x$gini_m_se,
                      gini_b = x$gini_b[!is.na(x$gini_b)],
                      gini_b_se = x$gini_b_se[!is.na(x$gini_b_se)]
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

save(out1, file = str_c("data/all_gt", gt, iter/1000, "k_", 
                        str_replace(Sys.time(), " ", "_") %>% str_replace("2017-", ""), ".rda"))

beep() # chime


# Plots
plot_tscs(x, out1, save_pdf = "paper/figures/ts_all_lis.pdf")
plot_tscs(x, out1)
