library(tidyverse)
library(rstan)
library(beepr)

load("data/ineq.rda")

seed <- 324
iter <- 8000
warmup <- iter - 2500
thin <- 5
chains <- 3
cores <- chains
adapt_delta <- .95

baseline_series <- "LIS market sqrt"
baseline_wd <- str_split(baseline_series, "\\s")[[1]] %>% nth(-2)
baseline_es <- str_split(baseline_series, "\\s")[[1]] %>% last()

x0 <- ineq2_m %>%  
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


rho_we_m <- rho_we_m %>%
  select(-ends_with("code")) %>% 
  left_join(x0 %>% 
              select("country", "year", "wdes",
                     "kcode", "rcode", "tcode", 
                     "wcode", "ecode", "wecode", "kwecode", "rwecode") %>% 
              distinct(),
            by = c("country", "year", "wdes"))

rho_wd_m <- rho_wd_m %>%
  select(-ends_with("code")) %>% 
  left_join(x0 %>% 
              rename(wd = "welfare_def") %>% 
              select("country", "year", "wd", "kbl",
                     "kcode", "rcode", "tcode", 
                     "wcode", "kwcode", "rwcode",
                     "kwd", "rwd") %>% 
              distinct(),
            by = c("country", "year", "wd", "kbl", "kwd", "rwd"))

rwe2codes <- rho_we_m %>%
  filter(wcode == 1) %>%        # baseline_wd is always coded 1
  transmute(wdes2 = wdes,
            rwe2code = rwecode,
            rcode = rcode) %>% 
  distinct() 

x <- x0 %>% 
  left_join(kt, by = c("kcode", "tcode")) %>% 
  mutate(wdes2 = str_replace(wdes, ".*_", "market_")) %>% 
  left_join(rwe2codes, by = c("wdes2", "rcode"))

kn <- x %>% 
  group_by(kcode) %>% 
  summarize(kt1 = min(ktcode),
            yrspan = first(yrspan),
            kr = first(rcode),
            bk = as.numeric(any(!is.na(gini_b)))) %>% 
  ungroup()

kt1 <- x %>%
  mutate(n = row_number()) %>%
  filter(ibl) %>%
  select(n, ktcode) %>%
  right_join(kt, by = "ktcode") %>% 
  mutate(n = if_else(!is.na(n), n, as.integer(0)))

mu_priors_by_wd <- function(x, var) {
  var <- rlang::ensym(var)
  prior_mu <-  x %>% 
    select(!!var, welfare_def) %>%
    distinct() %>% 
    arrange(!!var) %>%  
    mutate(prior_mu = case_when(welfare_def == "disp" ~ 1.1,
                                welfare_def == "con" ~ 1.1,
                                welfare_def == "gross" ~ 1.1,
                                welfare_def == "market" ~ 0)) %>% 
    pull(prior_mu)
  return(prior_mu)
}

s_priors_by_wd <- function(x, var) {
  var <- rlang::ensym(var)
  prior_s <- x %>% 
    select(!!var, welfare_def) %>%
    distinct() %>% 
    arrange(!!var) %>% 
    mutate(prior_s = case_when(welfare_def == "disp" ~ .15,
                               welfare_def == "con" ~ .15,
                               welfare_def == "gross" ~ .1,
                               welfare_def == "market" ~ .05)) %>% 
    pull(prior_s)
  return(prior_s)
}

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
                      gini_m = x$ln_gini_m,
                      gini_m_se = x$ln_gini_m_se,
                      gini_b = log(x$gini_b[!is.na(x$gini_b)]*100),
                      gini_b_se = x$gini_b_se[!is.na(x$gini_b_se)]/x$gini_b[!is.na(x$gini_b)],
                      
                      bk = kn$bk,
                      nbkt = kt1$n, 
                      
                      M = length(rho_we_m$rho),
                      kkm = rho_we_m$kcode,      
                      rrm = rho_we_m$rcode,
                      ttm	= rho_we_m$tcode,
                      wem = rho_we_m$wecode,
                      kwem = rho_we_m$kwecode,
                      rwem = rho_we_m$rwecode,
                      rho_we = rho_we_m$rho,
                      rho_we_se = rho_we_m$rho_se,
                      
                      P = length(rho_wd_m$rho_wd),
                      kkp = rho_wd_m$kcode,      
                      rrp = rho_wd_m$rcode,
                      kwp = rho_wd_m$kwcode,
                      rho_w = rho_wd_m$rho_wd,
                      rho_w_se = rho_wd_m$rho_wd_se,

                      prior_m_s = 1.05,
                      prior_s_s = .15,
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
out1 <- stan(file = "R/estimate_swiid/all.stan",
             data = source_data,
             seed = seed,
             iter = iter,
             warmup = warmup,
             thin = thin,
             cores = cores,
             chains = chains,
             pars = c("gini"),
             control = list(max_treedepth = 20,
                            adapt_delta = adapt_delta))
runtime <- proc.time() - start
runtime

lapply(get_sampler_params(out1, inc_warmup = FALSE),
       summary, digits = 2)

save(x, out1, file = str_c("data/all_mkt_", iter/1000, "k_", 
                           str_replace(Sys.time(), " ", "_") %>% str_replace("2018-", ""), ".rda"))

# Plots
source("R/plot_tscs.R")
plot_tscs(x, out1, save_pdf = "paper/figures/swiid_mkt.pdf")

beep() # chime
shinystan::launch_shinystan(out1)
