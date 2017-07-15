library(tidyverse)
library(stringr)
library(rstan)
library(beepr)

load("data/ineq.rda")

seed <- 324
iter <- 2000
chains <- 4
cores <- chains
gt <- 4

x0 <- baseline %>% 
  filter(k_bl_obs > gt) %>% 
  mutate(kcode = as.integer(factor(country, levels = unique(country)))) %>%  # redo codes for filtered sample
  group_by(country) %>% 
    mutate(tcode = as.integer(year - min(year) + 1)) %>% 
  ungroup()

kt <- x0 %>%     
  group_by(kcode) %>%
  summarize(firstyr = min(year),
            lastyr = max(year),
            yrspan = (lastyr - firstyr) + 1) %>% 
  ungroup() %>%  
  slice(rep(1:n(), yrspan)) %>% 
  group_by(kcode) %>% 
  mutate(tcode = 1:n()) %>% 
  ungroup() %>% 
  mutate(ktcode = 1:n())

x <- x0 %>% 
  left_join(kt, by = c("kcode", "tcode"))

kn <- x %>% 
  group_by(kcode) %>% 
  summarize(kt1 = first(ktcode),
            yrspan = first(yrspan)) %>% 
  ungroup()


source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      KT = nrow(kt),
                      N = length(x$gini_b),
                      N_b = length(x$gini_b[!is.na(x$gini_b)]),
                      kk = x$kcode,
                      tt = x$tcode,
                      kktt = x$ktcode,
                      ktt = kt$tcode,
                      ktk = kt$kcode,
                      kn = kn$yrspan,
                      kt1 = kn$kt1,
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
                            adapt_delta = .9))
runtime <- proc.time() - start
runtime

lapply(get_sampler_params(out1, inc_warmup = FALSE),
       summary, digits = 2)

save(out1, file = str_c("data/lis_gt", gt, iter/1000, "k_", 
                        str_replace(Sys.time(), " ", "_") %>% str_replace("2017-", ""), ".rda"))

beep() # chime


# Post-processing
source("R/plot_tscs.R")
plot_tscs(x, out1, dims = c(7, 4), save_pdf = "paper/figures/ts_lis_only.pdf")
plot_tscs(x, out1)
