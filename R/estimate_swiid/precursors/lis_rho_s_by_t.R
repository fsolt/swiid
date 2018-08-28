library(tidyverse)
library(rstan)
library(beepr)

load("data/ineq.rda")

seed <- 324
iter <- 3000
warmup <- iter - 1000
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
         wecode = as.integer(factor(wdes, levels = unique(wdes))),
         kwecode = as.integer(factor(100*kcode+wecode)),
         rwecode = as.integer(factor(100*rcode+wecode)),
         scode = as.integer(factor(series, levels = unique(series))),
         wcode = as.integer(factor(welfare_def) %>% forcats::fct_relevel(baseline_wd)),
         ecode = as.integer(factor(equiv_scale) %>% forcats::fct_relevel(baseline_es)),
         kwcode = as.integer(factor(100*kcode+wcode))) %>% 
  filter(kbl) # LIS countries only, for now

kt <- x0 %>% 
  select(country, firstyr, lastyr) %>% 
  distinct() %>% 
  slice(rep(1:n(), (lastyr - firstyr) + 1)) %>% 
  group_by(country) %>% 
  mutate(year = first(firstyr):first(lastyr)) %>% 
  ungroup() %>% 
  mutate(ktcode = 1:n()) %>% 
  select(country, year, ktcode)

skt0 <- x0 %>%  
  select(scode, year) %>% 
  arrange(scode, year) %>% 
  group_by(scode) %>% 
  mutate(firstyr =  min(year),
         lastyr = max(year)) %>% 
  ungroup() %>% 
  complete(scode, year) %>% 
  group_by(scode) %>% 
  mutate(firstyr = min(firstyr, na.rm = TRUE),
         lastyr = min(lastyr, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(year >= firstyr & year <= lastyr) %>% 
  mutate(sktcode = 1:n()) %>% 
  select(-firstyr, -lastyr)

rho_we <- rho_we %>%
  select(-ends_with("code")) %>% 
  inner_join(x0 %>% 
              select("country", "year", "wdes",
                     "kcode", "rcode", "tcode", 
                     "wcode", "ecode", "wecode", "kwecode", "rwecode") %>% 
              distinct(),
            by = c("country", "year", "wdes"))

rho_wd <- rho_wd %>%
  select(-ends_with("code")) %>% 
  inner_join(x0 %>% 
              rename(wd = "welfare_def") %>% 
              select("country", "year", "wd", "kbl",
                     "kcode", "rcode", "tcode", 
                     "wcode", "kwcode", "rwcode",
                     "kwd", "rwd") %>% 
              distinct(),
            by = c("country", "year", "wd", "kbl", "kwd", "rwd"))

rwe2codes <- rho_we %>%
  filter(wcode == 1) %>%        # baseline_wd is always coded 1
  transmute(wdes2 = wdes,
            rwe2code = rwecode,
            rcode = rcode) %>% 
  distinct() 

x <- x0 %>% 
  left_join(kt, by = c("country", "year")) %>% 
  mutate(wdes2 = str_replace(wdes, ".*_", "disp_")) %>% 
  left_join(rwe2codes, by = c("wdes2", "rcode")) %>% 
  left_join(skt0, by = c("scode", "year"))                # adds sktcode

skt <- skt0 %>% 
  left_join(x %>% select(scode, kwecode, rwecode) %>% distinct(), by = "scode")

skt_kwe <- skt %>% 
  arrange(kwecode) %>% 
  rowid_to_column() %>% 
  group_by(kwecode) %>% 
  summarize(skt_kwe_start = min(rowid),     # which skt starts each kwe?
            skt_kwe_end = max(rowid))      # which skt ends each kwe?

skt_rwe <- skt %>% 
  arrange(rwecode) %>% 
  rowid_to_column() %>% 
  group_by(rwecode) %>% 
  summarize(skt_rwe_start = min(rowid),     # which skt starts each kwe?
            skt_rwe_end = max(rowid))      # which skt ends each kwe?

kn <- x %>% 
  group_by(kcode) %>% 
  summarize(country = first(country),
            kt_k_start = min(ktcode),
            kt_k_end = max(ktcode),
            kr = first(rcode),
            bk = as.numeric(any(!is.na(gini_b)))) %>% 
  ungroup()

kt1 <- x %>%
  mutate(n = row_number()) %>%
  filter(ibl) %>%
  select(n, ktcode) %>%
  right_join(kt, by = "ktcode") %>% 
  mutate(n = if_else(!is.na(n), n, as.integer(0)))

rho_s <- x %>% 
  filter(bl) %>% 
  mutate(rho = gini_b/gini_m,
         rho_se = sqrt(gini_b_se^2 + gini_m_se^2)) %>% 
  filter(!(rho == 1 & equiv_scale == "sqrt" & str_detect(series, "^LIS")))

sj <- rho_s %>%
  mutate(sj = row_number()) %>%
  group_by(scode) %>%
  summarize(country = first(country),
            series = first(series),
            sj1 = min(sj)) %>%
  ungroup()

sn <- x %>% 
  filter(obl) %>% 
  group_by(scode) %>% 
  summarize(country = first(country),
            series = first(series),
            shnoo = as.numeric(sum(is.na(gini_b)) > 0),
            skt1 = min(sktcode),
            yrspan = max(year) - min(year) + 1,
            s_bl_obs = sum(!is.na(gini_b)),
            sr1 = any(sktcode == skt1 & !is.na(gini_b)) %>% as.numeric()) %>% 
  ungroup() %>% 
  left_join(sj, by = c("scode", "country", "series")) %>% 
  mutate(sj1 = if_else(is.na(sj1), 0, sj1))

mu_priors_by_wd <- function(x, var) {
  var <- rlang::ensym(var)
  prior_mu <-  x %>% 
    select(!!var, welfare_def) %>%
    distinct() %>% 
    arrange(!!var) %>%  
    mutate(prior_mu = case_when(welfare_def == "disp" ~ 0,
                                welfare_def == "con" ~ .1,
                                welfare_def == "gross" ~ -.1,
                                welfare_def == "market" ~ -.5)) %>% 
    pull(prior_mu)
  return(prior_mu)
}

s_priors_by_wd <- function(x, var) {
  var <- rlang::ensym(var)
  prior_s <- x %>% 
    select(!!var, welfare_def) %>%
    distinct() %>% 
    arrange(!!var) %>% 
    mutate(prior_s = case_when(welfare_def == "disp" ~ .05,
                               welfare_def == "con" ~ .15,
                               welfare_def == "gross" ~ .1,
                               welfare_def == "market" ~ .15)) %>% 
    pull(prior_s)
  return(prior_s)
}

# Format data for Stan
source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      KT = nrow(kt),
                      R = max(x$rcode),
                      S = x %>% filter(obl) %>% pull(scode) %>% max(),
                      SKT = max(x$sktcode),
                      WE = max(x$wecode),
                      KWE = max(x$kwecode),
                      RWE = max(x$rwecode),
                      KW = max(x$kwcode),
                      RW = max(x$rwcode),
                      W = max(x$wcode),
                      E = max(x$ecode),

                      N = nrow(x),
                      N_ibl = nrow(x %>% filter(ibl)),
                      N_wbl = nrow(x %>% filter(!is.na(gini_b))),
                      N_obl = nrow(x %>% filter(s_bl_obs > 2)),
                      N_bk = nrow(x %>% filter(k_bl_obs > 0)),
                      N_kw = nrow(x %>% filter(kw)),
                      
                      kk = x$kcode,
                      tt = x$tcode,
                      kktt = x$ktcode,
                      rr = x$rcode,
                      ss = x$scode,
                      skt = x$sktcode,
                      wen = x$wecode,
                      kwen = x$kwecode,
                      kwn = x$kwcode,
                      rwen = x$rwecode,
                      rwen2 = x$rwe2code,
                      gini_m = x$gini_m,
                      gini_m_se = x$gini_m_se,
                      gini_b = x$gini_b[!is.na(x$gini_b)],
                      gini_b_se = x$gini_b_se[!is.na(x$gini_b_se)],
                      
                      bk = kn$bk,
                      kt_k_start = kn$kt_k_start,
                      kt_k_end = kn$kt_k_end,
                      kr = kn$kr,
                      nbkt = kt1$n, 
 
                      J = length(rho_s$rho),
                      ssj = rho_s$scode,
                      sktj = rho_s$sktcode,
                      rho_s_m = rho_s$rho,
                      rho_s_m_se = rho_s$rho_se,
                      
                      sn = sn$yrspan,
                      shnoo = sn$shnoo,
                      s_bl_obs = sn$s_bl_obs,
                      skt1 = sn$skt1,
                      sr1 = sn$sr1,
                      sj1 = sn$sj1,
                      
                      skt_kwe_start = skt_kwe$skt_kwe_start,
                      skt_kwe_end = skt_kwe$skt_kwe_end,
                      skt_rwe_start = skt_rwe$skt_rwe_start,
                      skt_rwe_end = skt_rwe$skt_rwe_end,
                      
                      M = length(rho_we$rho),
                      kkm = rho_we$kcode,      
                      rrm = rho_we$rcode,
                      ttm	= rho_we$tcode,
                      wem = rho_we$wecode,
                      kwem = rho_we$kwecode,
                      rwem = rho_we$rwecode,
                      rho_we = rho_we$rho,
                      rho_we_se = rho_we$rho_se,
                      
                      P = length(rho_wd$rho_wd),
                      kkp = rho_wd$kcode,      
                      rrp = rho_wd$rcode,
                      kwp = rho_wd$kwcode,
                      rho_w = rho_wd$rho_wd,
                      rho_w_se = rho_wd$rho_wd_se,

                      prior_m_s = 0,
                      prior_s_s = .2,
                      prior_m_kwe = mu_priors_by_wd(x, kwecode),
                      prior_s_kwe = s_priors_by_wd(x, kwecode),
                      prior_m_rwe = mu_priors_by_wd(x, rwecode),
                      prior_s_rwe = s_priors_by_wd(x, rwecode),
                      prior_m_kw = mu_priors_by_wd(x, kwcode),
                      prior_s_kw = s_priors_by_wd(x, kwcode)
)

# Stan
rstan_options(auto_write = TRUE)

start <- proc.time()
out1 <- stan(file = "R/estimate_swiid/precursors/lis_rho_s_by_t.stan",
             data = source_data,
             seed = seed,
             iter = 100,
             warmup = 70,
             thin = 1,
             cores = cores,
             chains = chains,
             pars = c("gini", "sigma_gini", "sigma_s0", "sigma_s", "sigma_kwe"),
             control = list(max_treedepth = 20,
                            adapt_delta = adapt_delta))
runtime <- proc.time() - start
runtime
beepr::beep()

lapply(get_sampler_params(out1, inc_warmup = FALSE),
       summary, digits = 2)

save(x, out1, runtime, file = str_c("data/lis_rho_s_by_t_", iter/1000, "k_", 
                           str_replace(Sys.time(), " ", "_") %>% str_replace("2018-", ""), ".rda"))

# Plots
source("R/plot_tscs.R")
plot_tscs(x, out1, save_pdf = "paper/figures/swiid_lrsbt.pdf")

beep() # chime
shinystan::launch_shinystan(out1)
