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

x0 <- ineq2 %>%  
  filter(k_bl_obs > gt & s_bl_obs > 1) %>% 
  mutate(kcode = as.integer(factor(country, levels = unique(country))),
         scode = as.integer(factor(series, levels = unique(series)))) %>%  # redo codes for filtered sample
  group_by(country) %>% 
  mutate(tcode = as.integer(year - min(year) + 1)) %>% 
  ungroup()

kt <- x0 %>%  
  transmute(kcode = kcode,
            yrspan = (lastyr - firstyr) + 1) %>% 
  distinct(kcode, yrspan) %>% 
  slice(rep(1:n(), yrspan)) %>% 
  group_by(kcode) %>% 
  mutate(tcode = 1:n()) %>% 
  ungroup() %>% 
  mutate(ktcode = 1:n())

x <- x0 %>% 
  left_join(kt, by = c("kcode", "tcode"))

kn <- x %>% 
  group_by(kcode) %>% 
  summarize(kt1 = min(ktcode),
            yrspan = first(yrspan)) %>% 
  ungroup()

# Format data for Stan
source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      KT = nrow(kt),
                      S = max(x$scode),
                      N = length(x$gini_b),
                      N_b = length(x$gini_b[!is.na(x$gini_b)]),
                      kk = x$kcode,
                      tt = x$tcode,
                      kktt = x$ktcode,
                      ktt = kt$tcode,
                      ktk = kt$kcode,
                      kn = kn$yrspan,
                      kt1 = kn$kt1,
                      ss = x$scode,
                      gini_m = x$gini_m,
                      gini_m_se = x$gini_m_se,
                      gini_b = x$gini_b[!is.na(x$gini_b)],
                      gini_b_se = x$gini_b_se[!is.na(x$gini_b_se)]
)

# Stan
start <- proc.time()
out1 <- stan(file = "R/lis_plus_series.stan",
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

save(out1, file = str_c("data/series_gt", gt, iter/1000, "k_", 
                        str_replace(Sys.time(), " ", "_") %>% str_replace("2017-", ""), ".rda"))

beep() # chime


# Post-processing
source("R/plot_tscs.R")
plot_tscs(x, out1, save_pdf = "paper/figures/ts_lps.pdf")
plot_tscs(x, out1)
