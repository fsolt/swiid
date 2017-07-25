library(tidyverse)
library(stringr)
library(rstan)
library(beepr)

load("data/ineq.rda")

seed <- 324
iter <- 2000
chains <- 3
cores <- chains
adapt_delta <- .99

x0 <- ineq2_m %>%  
  filter(k_bl_obs == 0) %>%   # only non-baseline countries
  mutate(kcode = as.integer(factor(country, levels = unique(country))))  # redo codes for filtered sample

x0_wdes <- x0 %>%
  select(region, wdes) %>% 
  unite("r_weldef_eqsc", c("region", "wdes")) %>% 
  pull(r_weldef_eqsc) %>% 
  unique()

rho_we0 <- rho_we_m %>% 
  mutate(region = countrycode(country, "swiid.name", "swiid.region", custom_dict = cc_swiid)) %>% 
  unite("r_w_e", c("region", "wdes"), remove = FALSE) %>% 
  filter(r_w_e %in% x0_wdes) %>% 
  select(-matches("code"), -r_w_e)  

rwe2codes <- rho_we_m %>%
  filter(wcode == 1) %>%        # baseline_wd is always coded 1
  transmute(wdes2 = wdes,
            rwe2code = rwecode,
            rcode = rcode) %>% 
  distinct()

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
  left_join(kt, by = c("kcode", "tcode")) %>% 
  mutate(wdes2 = str_replace(wdes, ".*_", "market_")) %>% 
  left_join(rwe2codes, by = c("wdes2", "rcode")) %>% 
  mutate(rwe2code = if_else(is.na(rwe2code), 0L, rwe2code),
         kw = if_else(rwe2code==0, FALSE, kw)) %>% 
  arrange(desc(kw), desc(country_obs))
  
kn <- x %>% 
  group_by(kcode) %>% 
  summarize(kt1 = min(ktcode),
            yrspan = first(yrspan)) %>% 
  ungroup()

rho_we1 <- rho_we0 %>% 
  left_join(x %>% 
              select("region", "wdes", "rcode", "wecode", "rwecode") %>%
              distinct(),
            by = c("region", "wdes"))

rho_wd1 <- rho_wd_m %>% 
  filter(country %in% x$country) %>% 
  select(-matches("code")) %>% 
  left_join(x %>% 
              select("country", "year", "kwd", matches("code")) %>%
              distinct(),
            by = c("country", "year", "kwd")) 



# Format data for Stan
source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      KT = nrow(kt),
                      R = max(x$rcode),
                      WE = max(x$wecode),
                      KWE = max(x$kwecode),
                      RWE = max(x$rwecode),
                      W = max(x$wcode),
                      KW = max(rho_wd$kwcode),
                      RW = max(rho_wd$rwcode),
                      
                      N = nrow(x),
                      N_kr = nrow(x %>% filter(kw)),

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
                      rwen2 = x$rwe2code,
                      kwn = x$kwcode,
                      ken = x$kecode,
                      rwn = x$rwcode,
                      ren = x$recode,
                      
                      gini_m = x$gini_m,
                      gini_m_se = x$gini_m_se,
                      
                      M = length(rho_we1$rho),
                      rrm = rho_we1$rcode,
                      wem = rho_we1$wecode,
                      rwem = rho_we1$rwecode,
                      rho_we = rho_we1$rho,
                      rho_we_se = rho_we1$rho_se,
                      
                      P = length(rho_wd1$rho_wd),
                      kkp = rho_wd1$kcode,      
                      rrp = rho_wd1$rcode,
                      ttp	= rho_wd1$tcode,
                      wdp = rho_wd1$wcode,
                      kwp = rho_wd1$kwcode,
                      rwp = rho_wd1$rwcode,
                      rho_wd = rho_wd1$rho_wd,
                      rho_wd_se = rho_wd1$rho_wd_se
)

# Stan
start <- proc.time()
out1 <- stan(file = "R/all_not_lis.stan",
             data = source_data,
             seed = seed,
             iter = iter,
             cores = cores,
             chains = chains,
             control = list(max_treedepth = 20,
                            adapt_delta = adapt_delta))
runtime <- proc.time() - start
runtime

lapply(get_sampler_params(out1, inc_warmup = FALSE),
       summary, digits = 2)

save(x, out1, file = str_c("data/all_not_lis_mkt_", iter/1000, "k_",
                        str_replace(Sys.time(), " ", "_") %>% str_replace("2017-", ""), ".rda"))

beep() # chime


# Plots
source("R/plot_tscs.R")
plot_tscs(x, out1, save_pdf = "paper/figures/ts_all_not_lis_mkt.pdf")
plot_tscs(x, out1)
