library(tidyverse)
library(stringr)
library(rstan)
library(beepr)

load("data/ineq.rda")

seed <- 324
iter <- 500
chains <- 4
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
                      W = max(x$wcode),
                      KW = max(rho_wd$kwcode),
                      RW = max(rho_wd$rwcode),
                      E = max(x$ecode),
                      KE = max(rho_es$kecode),
                      RE = max(rho_es$recode),
                      N = nrow(x),
                      N_bl = nrow(x %>% filter(bl)),
                      N_obl = nrow(x %>% filter(obl)),
                      N_kbl = nrow(x %>% filter(kbl)),
                      N_kk = nrow(x %>% filter(kbl | (kw & ke))),
                      N_kr = nrow(x %>% filter(kbl | kw)),
                      N_rk = nrow(x %>% filter(kbl | kw | ke)),
                      
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
                      kwn = x$kwcode,
                      ken = x$kecode,
                      rwn = x$rwcode,
                      ren = x$recode,
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
                      
                      P = length(rho_wd1$rho_wd),
                      kkp = rho_wd1$kcode,      
                      rrp = rho_wd1$rcode,
                      ttp	= rho_wd1$tcode,
                      wdp = rho_wd1$wcode,
                      kwp = rho_wd1$kwcode,
                      rwp = rho_wd1$rwcode,
                      rho_wd = rho_wd1$rho_wd,
                      rho_wd_se = rho_wd1$rho_wd_se,
                      
                      Q = length(rho_es1$rho_es),
                      kkq = rho_es1$kcode,      
                      rrq = rho_es1$rcode,
                      ttq	= rho_es1$tcode,
                      esq = rho_es1$ecode,
                      keq = rho_es1$kecode,
                      req = rho_es1$recode,
                      rho_es = rho_es1$rho_es,
                      rho_es_se = rho_es1$rho_es_se
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
