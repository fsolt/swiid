library(cmdstanr)
library(tidyverse)
library(beepr)

load("data/ineq.rda")

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
         kwcode = as.integer(factor(100*kcode+wcode)))

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
  mutate(sj1 = if_else(is.na(sj1), as.integer(0), sj1))

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
                      S = max(x$scode),
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

# Stam
iter <- 2000

start <- proc.time()
all <- cmdstan_model(here::here("R", "estimate_swiid", "all.stan"))
out1 <- all$sample(
  data = source_data, 
  max_treedepth = 20,
  adapt_delta = 0.9,
  seed = 324, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = iter*.25,
  iter_sampling = iter*.75,
  refresh = iter/50
)
runtime <- proc.time() - start
runtime

results_path <- here::here(file.path("data", 
                                     "swiid_estimates",
                                     str_c("all_",
                                           iter/1000,
                                           "k-", 
                                           str_replace_all(Sys.time(), "[-:]", "_") %>%
                                             str_replace(" ", "-") %>% 
                                             str_replace("^\\d{4}_", "") %>% 
                                             str_replace("_\\d{2}$", ""))))
dir.create(results_path, 
           showWarnings = FALSE, 
           recursive = TRUE)
out1$save_data_file(dir = results_path,
                    random = FALSE)
out1$save_output_files(dir = results_path,
                       random = FALSE)
save(x, file = file.path(results_path,
                         "all_in.rda"))

out1$cmdstan_diagnose()

beep() # chime
