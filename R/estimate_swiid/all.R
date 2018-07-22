library(tidyverse)
library(rstan)
library(beepr)

load("data/ineq.rda")

seed <- 324
iter <- 10000
warmup <- iter - 1500
thin <- 3
chains <- 3
cores <- chains
adapt_delta <- .9

baseline_series <- "LIS disp sqrt"
baseline_wd <- str_split(baseline_series, "\\s")[[1]] %>% nth(-2)
baseline_es <- str_split(baseline_series, "\\s")[[1]] %>% last()

x0 <- ineq2 %>%  
  mutate(kcode = as.integer(factor(country, levels = unique(country))),
         rcode = as.integer(factor(region, levels = unique(region))),
         scode = as.integer(factor(series, levels = unique(series))),
         wecode = as.integer(factor(wdes, levels = unique(wdes))),
         kwecode = as.integer(factor(100*kcode+wecode)),
         rwecode = as.integer(factor(100*rcode+wecode)),
         wcode = as.integer(factor(welfare_def) %>% forcats::fct_relevel(baseline_wd)),
         ecode = as.integer(factor(equiv_scale) %>% forcats::fct_relevel(baseline_es)),
         kwcode = as.integer(factor(100*kcode+wcode))) 

kt <- x0 %>%  
  transmute(kcode = kcode,
            yrspan = (lastyr - firstyr) + 1) %>% 
  distinct(kcode, yrspan) %>% 
  slice(rep(1:n(), yrspan)) %>% 
  group_by(kcode) %>% 
  mutate(tcode = 1:n()) %>% 
  ungroup() %>% 
  mutate(ktcode = 1:n())

rwe2codes <- rho_we %>%
  filter(wcode == 1) %>%        # baseline_wd is always coded 1
  transmute(wdes2 = wdes,
            rwe2code = rwecode,
            rcode = rcode) %>% 
  distinct() 

x <- x0 %>% 
  left_join(kt, by = c("kcode", "tcode")) %>% 
  mutate(wdes2 = str_replace(wdes, ".*_", "disp_")) %>% 
  left_join(rwe2codes, by = c("wdes2", "rcode"))

kn <- x %>% 
  group_by(kcode) %>% 
  summarize(kt1 = min(ktcode),
            yrspan = first(yrspan),
            kr = first(rcode)) %>% 
  ungroup()


# Format data for Stan
source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      KT = nrow(kt),
                      R = max(x$rcode),
                      S = max(x$scode),
                      WE = max(x$wecode),
                      KWE = max(x$kwecode),
                      RWE = max(x$rwecode),
                      KW = max(x$kwcode),
                      RW = max(x$rwcode),
                      W = max(x$wcode),
                      E = max(x$ecode),

                      N = nrow(x),
                      N_ibl = nrow(x %>% filter(ibl)),
                      N_bl = nrow(x %>% filter(!is.na(gini_b))),
                      N_obl = nrow(x %>% filter(s_bl_obs>0)),
                      N_bk = nrow(x %>% filter(k_bl_obs > 0)),
                      N_kw = nrow(x %>% filter(kw)),
                      
                      kk = x$kcode,
                      tt = x$tcode,
                      kktt = x$ktcode,
                      kn = kn$yrspan,
                      kt1 = kn$kt1,
                      kr = kn$kr,
                      rr = x$rcode,
                      ss = x$scode,
                      wen = x$wecode,
                      kwen = x$kwecode,
                      kwn = x$kwcode,
                      rwen = x$rwecode,
                      rwen2 = x$rwe2code,
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
                      rho_we_se = rho_we$rho_se,

                      disp_KWE = rho_we %>% filter(wcode == 1) %>% pull(kwecode) %>% unique() %>% length(),
                      disp_idx_KWE = rho_we %>% filter(wcode == 1) %>% pull(kwecode) %>% unique(),
                      con_KWE = rho_we %>% filter(wcode == 2) %>% pull(kwecode) %>% unique() %>% length(),
                      con_idx_KWE = rho_we %>% filter(wcode == 2) %>% pull(kwecode) %>% unique(),
                      gross_KWE = rho_we %>% filter(wcode == 3) %>% pull(kwecode) %>% unique() %>% length(),
                      gross_idx_KWE = rho_we %>% filter(wcode == 3) %>% pull(kwecode) %>% unique(),
                      market_KWE = rho_we %>% filter(wcode == 4) %>% pull(kwecode) %>% unique() %>% length(),
                      market_idx_KWE = rho_we %>% filter(wcode == 4) %>% pull(kwecode) %>% unique(),
                      
                      disp_RWE = rho_we %>% filter(wcode == 1) %>% pull(rwecode) %>% unique() %>% length(),
                      disp_idx_RWE = rho_we %>% filter(wcode == 1) %>% pull(rwecode) %>% unique(),
                      con_RWE = rho_we %>% filter(wcode == 2) %>% pull(rwecode) %>% unique() %>% length(),
                      con_idx_RWE = rho_we %>% filter(wcode == 2) %>% pull(rwecode) %>% unique(),
                      gross_RWE = rho_we %>% filter(wcode == 3) %>% pull(rwecode) %>% unique() %>% length(),
                      gross_idx_RWE = rho_we %>% filter(wcode == 3) %>% pull(rwecode) %>% unique(),
                      market_RWE = rho_we %>% filter(wcode == 4) %>% pull(rwecode) %>% unique() %>% length(),
                      market_idx_RWE = rho_we %>% filter(wcode == 4) %>% pull(rwecode) %>% unique(),
                      
                      P = length(rho_wd$rho_wd),
                      kkp = rho_wd$kcode,      
                      rrp = rho_wd$rcode,
                      kwp = rho_wd$kwcode,
                      rho_w = rho_wd$rho_wd,
                      rho_w_se = rho_wd$rho_wd_se,

                      disp_KW = rho_wd %>% filter(wcode == 1) %>% pull(kwcode) %>% unique() %>% length(),
                      disp_idx_KW = rho_wd %>% filter(wcode == 1) %>% pull(kwcode) %>% unique(),
                      con_KW = rho_wd %>% filter(wcode == 2) %>% pull(kwcode) %>% unique() %>% length(),
                      con_idx_KW = rho_wd %>% filter(wcode == 2) %>% pull(kwcode) %>% unique(),
                      gross_KW = rho_wd %>% filter(wcode == 3) %>% pull(kwcode) %>% unique() %>% length(),
                      gross_idx_KW = rho_wd %>% filter(wcode == 3) %>% pull(kwcode) %>% unique(),
                      market_KW = rho_wd %>% filter(wcode == 4) %>% pull(kwcode) %>% unique() %>% length(),
                      market_idx_KW = rho_wd %>% filter(wcode == 4) %>% pull(kwcode) %>% unique()
)

# Stan
rstan_options(auto_write = TRUE)

start <- proc.time()
out1 <- stan(file = "R/estimate_swiid/all.stan",
             data = source_data,
             seed = seed,
             iter = 200,
             warmup = 100,
             thin = thin,
             cores = cores,
             chains = chains,
             pars = c("gini", "rho_kw_hat"),
             control = list(max_treedepth = 20,
                            adapt_delta = adapt_delta))
runtime <- proc.time() - start
runtime

lapply(get_sampler_params(out1, inc_warmup = FALSE),
       summary, digits = 2)

save(x, out1, file = str_c("data/all_", iter/1000, "k_", 
                           str_replace(Sys.time(), " ", "_") %>% str_replace("2018-", ""), ".rda"))

# Plots
source("R/plot_tscs.R")
plot_tscs(x, out1, save_pdf = "paper/figures/swiid.pdf")
plot_tscs(x, out1)

beep() # chime
shinystan::launch_shinystan(out1)
