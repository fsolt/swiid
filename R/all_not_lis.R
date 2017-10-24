library(tidyverse)
library(stringr)
library(rstan)
library(beepr)

load("data/ineq.rda")

seed <- 324
iter <- 3000
warmup <- iter - 1000
chains <- 3
cores <- chains
adapt_delta <- .9

baseline_series <- "LIS disp sqrt"
baseline_wd <- str_split(baseline_series, "\\s")[[1]] %>% nth(-2)
baseline_es <- str_split(baseline_series, "\\s")[[1]] %>% last()

x0 <- ineq2 %>%  
  filter(k_bl_obs == 0) %>%   # only non-baseline countries
  mutate(kcode2 = as.integer(factor(country, levels = unique(country))),
         tcode2 = year - min(year) + 1,
         rcode2 = as.integer(factor(region, levels = unique(region))),
         wecode2 = as.integer(factor(wdes, levels = unique(wdes))),
         kwecode2 = as.integer(factor(100*kcode+wecode)),
         wcode2 = as.integer(factor(welfare_def) %>% forcats::fct_relevel(baseline_wd)),
         ecode2 = as.integer(factor(equiv_scale) %>% forcats::fct_relevel(baseline_es)),
         wecode2 = as.integer(factor(paste(wcode, ecode))),
         kwecode2 = as.integer(factor(100*kcode+wecode)),
         rwecode2 = as.integer(factor(100*rcode+wecode)),
         kwcode2 = as.integer(factor(100*kcode+wcode)),
         rwcode2 = as.integer(factor(100*rcode+wcode)),
         kecode2 = as.integer(factor(100*kcode+ecode)),
         recode2 = as.integer(factor(100*rcode+ecode)))  # redo codes for filtered sample

x0_wdes <- x0 %>%
  select(region, wdes) %>% 
  unite("r_weldef_eqsc", c("region", "wdes")) %>% 
  pull(r_weldef_eqsc) %>% 
  unique()

rho_we0 <- rho_we %>% 
  mutate(region = countrycode(country, "swiid.name", "swiid.region", custom_dict = cc_swiid)) %>% 
  unite("r_w_e", c("region", "wdes"), remove = FALSE) %>% 
  filter(r_w_e %in% x0_wdes) %>% 
  select(-matches("code"), -r_w_e)  

rcodes <- x0 %>%
  select(region, rcode, rcode2) %>% 
  distinct()

rwecodes_x0 <- x0 %>% # region and wd_es combinations that exist in filtered data
  select(rwecode, rwecode2) %>% 
  distinct()

rwecodes_all <- ineq2 %>%
  select(rwecode, region, rcode) %>%
  right_join(rcodes, by = c("region", "rcode")) %>% 
  distinct() %>% 
  left_join(rwecodes_x0, by = "rwecode") %>% 
  arrange(rwecode2) %>% 
  mutate(rwecode2 = 1:n())

rwe2codes <- rho_we %>%
  left_join(rwecodes_all, by = c("rwecode", "rcode")) %>% 
  filter(wcode == 1) %>%        # baseline_wd is always coded 1
  transmute(wdes2 = wdes,
            rwe2code2 = rwecode2,
            rcode2 = rcode2) %>% 
  distinct() %>% 
  filter(!is.na(rcode2))

kt <- x0 %>%  
  transmute(kcode2 = kcode2,
            firstyr = firstyr,
            yrspan = (lastyr - firstyr) + 1) %>% 
  distinct(kcode2, yrspan, firstyr) %>% 
  slice(rep(1:n(), yrspan)) %>% 
  group_by(kcode2) %>% 
  mutate(year = firstyr - 1 + 1:n()) %>% 
  ungroup() %>% 
  mutate(tcode2 = year - min(year) + 1,
         ktcode2 = 1:n()) %>% 
  select(-year, -firstyr)

x <- x0 %>% 
  left_join(kt, by = c("kcode2", "tcode2")) %>% 
  mutate(wdes2 = str_replace(wdes, ".*_", "disp_")) %>% 
  left_join(rwe2codes, by = c("wdes2", "rcode2"))

kn <- x %>% 
  group_by(kcode2) %>% 
  summarize(kt1 = min(ktcode2),
            yrspan = first(yrspan)) %>% 
  ungroup()

rho_we1 <- rho_we0 %>% 
  left_join(x %>% 
              select("region", "wdes", "rcode2", "wecode2", "rwecode2") %>%
              distinct(),
            by = c("region", "wdes"))

rho_wd1 <- rho_wd %>% 
  filter(country %in% x$country) %>% 
  select(-matches("code")) %>% 
  left_join(x %>% 
              select("country", "year", "kwd", "kcode2", "rcode2", "tcode2", "wcode2", "kwcode2", "rwcode2") %>%
              distinct(),
            by = c("country", "year", "kwd")) 


# Format data for Stan
source_data <- list(  K = max(x$kcode2),
                      T = max(x$tcode2),
                      KT = nrow(kt),
                      R = max(x$rcode2),
                      WE = max(x$wecode2),
                      KWE = max(x$kwecode2), 
                      RWE = max(x$rwe2code2),
                      W = max(x$wcode2),
                      KW = max(x$kwcode2),
                      RW = max(x$rwcode2),
                      
                      N = nrow(x),
                      N_kr = nrow(x %>% filter(kw)),
                      
                      kk = x$kcode2,
                      tt = x$tcode2,
                      kk = x$kcode2,
                      tt = x$tcode2,
                      kktt = x$ktcode2,
                      ktt = kt$tcode2,
                      ktk = kt$kcode2,
                      kn = kn$yrspan,
                      kt1 = kn$kt1,
                      rr = x$rcode2,
                      wen = x$wecode2,
                      kwen = x$kwecode2,
                      rwen = x$rwecode2,
                      rwen2 = x$rwe2code2,
                      kwn = x$kwcode2,
                      ken = x$kecode2,
                      rwn = x$rwcode2,
                      ren = x$recode2,
                      
                      gini_m = x$gini_m,
                      gini_m_se = x$gini_m_se,
                      
                      M = length(rho_we1$rho),
                      rrm = rho_we1$rcode2,
                      wem = rho_we1$wecode2,
                      rwem = rho_we1$rwecode2,
                      rho_we = rho_we1$rho,
                      rho_we_se = rho_we1$rho_se,
                      
                      P = length(rho_wd1$rho_wd),
                      kkp = rho_wd1$kcode2,      
                      rrp = rho_wd1$rcode2,
                      ttp	= rho_wd1$tcode2,
                      wdp = rho_wd1$wcode2,
                      kwp = rho_wd1$kwcode2,
                      rwp = rho_wd1$rwcode2,
                      rho_wd = rho_wd1$rho_wd,
                      rho_wd_se = rho_wd1$rho_wd_se
)

# Stan
start <- proc.time()
out1 <- stan(file = "R/all_not_lis.stan",
             data = source_data,
             seed = seed,
             iter = iter,
             warmup = warmup,
             cores = cores,
             chains = chains,
             control = list(max_treedepth = 20,
                            adapt_delta = adapt_delta))
runtime <- proc.time() - start
runtime

lapply(get_sampler_params(out1, inc_warmup = FALSE),
       summary, digits = 2)

save(x, out1, file = str_c("data/all_not_lis_", iter/1000, "k_",
                           str_replace(Sys.time(), " ", "_") %>% str_replace("2017-", ""), ".rda"))

beep() # chime


# Plots
source("R/plot_tscs.R")
plot_tscs(x, out1, save_pdf = "paper/figures/ts_all_not_lis.pdf")
plot_tscs(x, out1)
