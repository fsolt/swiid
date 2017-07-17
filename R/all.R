library(tidyverse)
library(stringr)
library(rstan)
library(beepr)

load("data/ineq.rda")

seed <- 324
iter <- 10
chains <- 1
cores <- chains

kt <- ineq2 %>%  
  transmute(kcode = kcode,
            yrspan = (lastyr - firstyr) + 1) %>% 
  distinct(kcode, yrspan) %>% 
  slice(rep(1:n(), yrspan)) %>% 
  group_by(kcode) %>% 
  mutate(tcode = 1:n()) %>% 
  ungroup() %>% 
  mutate(ktcode = 1:n())

x <- ineq2 %>% 
  left_join(kt, by = c("kcode", "tcode"))

kn <- x %>% 
  group_by(kcode) %>% 
  summarize(kt1 = first(ktcode),
            yrspan = first(yrspan)) %>% 
  ungroup()

rho_wd1 <- rho_wd
rho_es1 <- rho_es

# Format data for Stan
source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      KT = nrow(kt),
                      R = max(x$rcode),
                      S = max(x$scode),
                      WE = max(x$wecode),
                      KWE = max(x$kwecode),
                      RWE = max(x$rwecode),

                      N = nrow(x),
                      N_bl = nrow(x %>% filter(bl)),
                      N_obl = nrow(x %>% filter(obl)),
                      N_kbl = nrow(x %>% filter(kbl)),
                      
                      kk = x$kcode,
                      tt = x$tcode,
                      kk = x$kcode,
                      tt = x$tcode,
                      kktt = x$ktcode,
                      ktt = kt$tcode,
                      ktk = kt$kcode,
                      kn = kn$yrspan,
                      kt1 = kn$kt1,
                      rr = x$rcode,
                      ss = x$scode,
                      wen = x$wecode,
                      kwen = x$kwecode,
                      rwen = x$rwecode,
                      
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
                      rwem = rho_we$rwecode,
                      rho_we = rho_we$rho,
                      rho_we_se = rho_we$rho_se
)

# Stan
start <- proc.time()
out1 <- stan(file = "R/all.stan",
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

save(out1, file = str_c("data/all_kt1_", iter/1000, "k_",
                        str_replace(Sys.time(), " ", "_") %>% str_replace("2017-", ""), ".rda"))

beep() # chime


# Plots
source("R/plot_tscs.R")
plot_tscs(x, out1, save_pdf = "paper/figures/ts_all.pdf")
plot_tscs(x, out1)
