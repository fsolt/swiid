library(tidyverse)
library(rstan)

load("data/ineq0.rda")
rm(ineq2, rho_we, rho_wd)

baseline_series <- "LIS disp sqrt"
baseline_wd <- str_split(baseline_series, "\\s")[[1]] %>% nth(-2)
baseline_es <- str_split(baseline_series, "\\s")[[1]] %>% last()

baseline_kfold <- function(k) {
  set.seed(324)
  problem1 <- 1
  while (problem1 > 0) {
    baseline_rnd <- lis %>% 
      filter(series == baseline_series) %>% 
      group_by(country) %>% 
      mutate(f1 = 1:n()) %>% 
      ungroup() %>% 
      mutate(f2 = c(rep(1:max(f1), length(f1) %/% max(f1)), 1:(length(f1) %% max(f1)))) %>%
      sample_n(., size = nrow(.), replace = FALSE) %>%
      arrange(f2) %>% 
      mutate(fold_number = cut(seq(1, nrow(.)),
                               breaks = ceiling(nrow(.)/k),
                               labels = FALSE)) %>% 
      group_by(fold_number) %>% 
      mutate(countries = n_distinct(country),
             china_plus_india = (any(country == "China") & any(country == "India")),
             egypt_plus_safr = (any(country == "Egypt") & any(country == "South Africa")),
             problem = (countries < n() | china_plus_india == TRUE | egypt_plus_safr == TRUE)) %>% 
      ungroup()
    
    problem1 <- mean(baseline_rnd$problem)
  }
  return(baseline_rnd)
}

## Combine
make_inputs <- function(baseline_series, fold) {
  # first, get baseline series and order by data-richness
  baseline_wd <- str_split(baseline_series, "\\s")[[1]] %>% nth(-2)
  baseline_es <- str_split(baseline_series, "\\s")[[1]] %>% last()
  baseline_wdes <- paste0(baseline_wd, "_", baseline_es)
  baseline <- baseline_rnd %>% 
    filter(!fold_number==fold) %>%  # exclude observations by fold
    mutate(gini_b = gini,
           gini_b_se = gini_se * 2) %>%
    select(-gini, -gini_se) %>% 
    group_by(country) %>% 
    mutate(k_bl_obs = n()) %>% 
    ungroup() %>% 
    select(-f1, -f2, -fold_number, -countries, -problem) %>% 
    arrange(desc(k_bl_obs), desc(country), year)
  
  cy_fold <- baseline_rnd %>% 
    filter(fold_number == fold) %>%
    mutate(cy = paste(country, year),
           gini_b = gini,
           gini_b_se = gini_se * 2) %>% 
    select(country, year, cy, gini_b, gini_b_se)
  
  lis2 <- lis %>%
    filter(!(paste(country, year) %in% cy_fold$cy))
  
  # turn cross-country series into within-country series
  oecd1 <- oecd %>% 
    mutate(series = paste("OECD", country, str_replace(series, "OECD ", "")))
  ceq1 <- ceq %>% 
    mutate(series = paste("CEQ", country, str_replace(series, "CEQ ", "")))
  lis3 <- lis2 %>% 
    mutate(series = paste("LIS", country, str_replace(series, "LIS ", "")))
  
  # then combine with other series ordered by data-richness
  ineq0 <- bind_rows(lis3, 
                     sedlac, cepal, cepal_sdi, oecd1, eurostat,
                     transmonee, ceq1, afr_gini, wb,
                     armstat, abs, inebo, belstat, statcan, dane, ineccr, dkstat,
                     capmas, statee, statfi, insee, geostat,
                     stathk, bpsid, amar, cso_ie, istat, kazstat, kostat, nsck,
                     epumy, nbs, monstat, snz, nzmsd, ssb, dgeec, psa,
                     rosstat, singstat, ssi, ine, statslk, scb, 
                     tdgbas, nso_thailand, turkstat, ons, ifs, cbo, uscb, uine, inev, gso_vn,
                     atg, gidd,
                     added_data) %>% 
    rename(gini_m = gini,
           gini_m_se = gini_se) %>%
    mutate(country = countrycode(country, "country.name", "swiid.name", custom_dict = cc_swiid),
           region = countrycode(country, "swiid.name", "swiid.region", custom_dict = cc_swiid)) %>% 
    group_by(country) %>% 
    mutate(country_obs = n()) %>% 
    ungroup() %>% 
    group_by(country, series) %>% 
    mutate(series_obs = n()) %>%
    ungroup() %>% 
    arrange(desc(country_obs), desc(series_obs))  
  
  # obs with baseline data
  ineq_bl <- ineq0 %>% 
    right_join(baseline %>% 
                 select(country, year, gini_b, gini_b_se, k_bl_obs),
               by = c("country", "year")) %>% 
    arrange(desc(k_bl_obs)) %>% 
    group_by(country, series) 
  
  ineq_bl_series <- ineq_bl %>% pull(series) %>% unique()
  
  # obs with no baseline data from series with some baseline data ("overlap baseline")
  ineq_obl <- ineq0 %>% anti_join(ineq_bl %>% select(-gini_b, -gini_b_se), 
                                  by = c("country", "year")) %>% 
    filter(series %in% ineq_bl_series) %>% 
    group_by(country, series)
  
  # obs from series with no baseline data
  ineq_nbl <- ineq0 %>% anti_join(ineq_bl %>% select(-gini_b, -gini_b_se), 
                                  by = c("country", "year")) %>% 
    filter(!series %in% ineq_bl_series)
  
  ineq_oth_series <- bind_rows(ineq_obl, ineq_nbl) %>% pull(series) %>% unique()
  
  # combine all
  ineq <- bind_rows(ineq_bl, ineq_obl, ineq_nbl) %>% 
    group_by(series) %>% 
    mutate(s_bl_obs = sum(!is.na(gini_b))) %>% 
    ungroup() %>% 
    group_by(country) %>% 
    mutate(k_bl_obs = if_else(!is.na(mean(k_bl_obs, na.rm = TRUE)),
                              mean(k_bl_obs, na.rm = TRUE), 0),
           tcode0 = year - min(year) + 1) %>% 
    ungroup() %>% 
    arrange(desc(k_bl_obs), desc(country_obs)) %>% 
    mutate(gini_m_se = ifelse(!is.na(gini_m_se), gini_m_se * 2,
                              quantile(gini_m_se/gini_m, .99, na.rm = TRUE) * gini_m * 2),
           wdes = paste(welfare_def, equiv_scale, sep = "_"),
           ibl = (gini_m == gini_b & str_detect(series, paste("LIS .*", baseline_wd, baseline_es))),
           bl = (!is.na(gini_b)),
           obl = (s_bl_obs > 0),
           kbl = (k_bl_obs > 0),
           kcode = as.integer(factor(country, levels = unique(country))),
           tcode = tcode0,
           rcode = as.integer(factor(region, levels = unique(region))),
           scode = as.integer(factor(series, levels = unique(series))),
           wcode = as.integer(factor(welfare_def) %>% forcats::fct_relevel(baseline_wd)),
           ecode = as.integer(factor(equiv_scale) %>% forcats::fct_relevel(baseline_es)),
           wecode = as.integer(factor(paste(wcode, ecode))),
           kwecode = as.integer(factor(100*kcode+wecode)),
           rwecode = as.integer(factor(100*rcode+wecode))) %>% 
    select(-tcode0) # tcode0 is only used to facilitate getting tcode into its customary column position
  
  wecodes <- ineq %>%
    select(wdes, wecode, wcode, ecode) %>% 
    distinct() %>% 
    mutate(wd = str_replace(wdes, "_.*", ""),
           es = str_replace(wdes, ".*_", "")) %>% 
    arrange(wecode)
  
  kwecodes <- ineq %>%
    select(wecode, kcode, kwecode, rwecode) %>% 
    distinct()
  
  ineq1 <- ineq %>% 
    group_by(kcode, tcode, welfare_def, equiv_scale) %>% 
    summarize(n_obs = n(),
              gini_cat = mean(gini_m), 
              gini_cat_se = ifelse(n_obs == 1,
                                   gini_m_se,
                                   sqrt(mean(gini_m_se^2) + (1+1/n_obs)*var(gini_m)))) %>%  # per Rubin (1987)
    ungroup() %>% 
    select(-n_obs) %>% 
    unite(wdes, welfare_def, equiv_scale) %>% 
    bind_rows(ineq %>%
                group_by(kcode, tcode) %>% 
                summarize(gini_cat = first(gini_b),
                          gini_cat_se = first(gini_b_se),
                          wdes = "baseline") %>% 
                ungroup())
  
  
  ## Generate ratios
  # generate ratios of baseline to each wd_es 
  rho_we0 <- ineq1 %>% 
    select(-gini_cat_se) %>% 
    spread(key = wdes, value = gini_cat) %>% 
    mutate_at(vars(matches("_")),
              funs(baseline/.)) %>% 
    select(-baseline) %>% 
    gather(key = wdes, value = rho, -kcode, -tcode) %>% 
    filter(!is.na(rho)) %>% 
    arrange(kcode, tcode, wdes)
  
  rho_we_se <- ineq1 %>% 
    select(-gini_cat) %>% 
    spread(key = wdes, value = gini_cat_se) %>% 
    mutate_at(vars(matches("_")),
              funs(sqrt(baseline^2+.^2))) %>% 
    select(-matches("baseline")) %>% 
    gather(key = wdes, value = rho_se, -kcode, -tcode) %>% 
    filter(!is.na(rho_se)) %>% 
    arrange(kcode, tcode, wdes)
  
  rho_we00 <- rho_we0 %>% 
    left_join(rho_we_se, by = c("kcode", "tcode", "wdes")) %>% 
    mutate(rho_se = if_else(rho == 1, .1, rho_se)) %>%            # placeholder for baseline series
    left_join(ineq %>% select(country, year, kcode, tcode, rcode) %>% distinct(),
              by = c("kcode", "tcode")) %>% 
    left_join(wecodes, by = "wdes") %>% 
    left_join(kwecodes, by = c("kcode", "wecode"))
  
  rcodes_not_miss <- rho_we00 %>%                       # regions with observed ratios to baseline series
    filter(!rho == 1) %>%
    select(rcode, wdes) %>% 
    filter(wdes == baseline_wdes) %>% 
    unique() %>% 
    pull(rcode)
  
  rho_we <- rho_we00 %>% 
    filter(!(rho == 1 & (rcode %in% rcodes_not_miss)))  # use placeholder only if no observed ratios to baseline series
  
  rm(rho_we0, rho_we_se)
  
  # generate ratios of baseline_wd to each wd (for all constant es)
  rho_wd0 <- map_df(c("pc", "hh", "sqrt", "oecdm", "ae"), function(e) {
    ineq1 %>% 
      select(-gini_cat_se) %>% 
      spread(key = wdes, value = gini_cat) %>% 
      mutate(bl = get(paste0(baseline_wd, "_", e))) %>% 
      mutate_at(vars(matches(e)),
                funs(bl/.)) %>% 
      select(kcode, tcode, matches(e)) %>% 
      gather(key = wdes, value = rho_wd, -kcode, -tcode) %>% 
      filter(!is.na(rho_wd)) %>% 
      mutate(wd = str_replace(wdes, "_.*", "")) %>% 
      select(-wdes) %>% 
      arrange(kcode, tcode, wd)
  })
  
  rho_wd_se <- map_df(c("pc", "hh", "sqrt", "oecdm", "ae"), function(e) {
    ineq1 %>% 
      select(-gini_cat) %>% 
      spread(key = wdes, value = gini_cat_se) %>% 
      mutate(bl = get(paste0(baseline_wd, "_", e))) %>% 
      mutate_at(vars(matches(e)),
                funs(sqrt(bl^2+.^2))) %>% 
      select(kcode, tcode, matches(e)) %>% 
      gather(key = wdes, value = rho_wd_se, -kcode, -tcode) %>% 
      filter(!is.na(rho_wd_se)) %>% 
      mutate(wd = str_replace(wdes, "_.*", "")) %>% 
      select(-wdes) %>% 
      arrange(kcode, tcode, wd)
  })
  
  rho_wd <- rho_wd0 %>% 
    left_join(rho_wd_se, by = c("kcode", "tcode", "wd")) %>% 
    group_by(kcode, tcode, wd) %>%
    summarize(rho_wd = max(rho_wd),
              rho_wd_se = max(rho_wd_se)) %>%
    ungroup() %>%
    left_join(ineq %>% select(country, year, kcode, tcode, rcode, kbl) %>% distinct(),
              by = c("kcode", "tcode")) %>% 
    left_join(wecodes %>% select("wd", "wcode") %>% distinct(), by = "wd") %>% 
    mutate(kwcode = as.integer(factor(100*kcode+wcode)),
           rwcode = as.integer(factor(100*rcode+wcode)),
           kwd = paste(country, wd),
           rwd = paste(rcode, wd))
  
  rm(rho_wd0, rho_wd_se)
  
  rho_wd_kw <- rho_wd %>% 
    pull(kwd) %>% 
    unique()
  
  # generate ratios of baseline_es to each es (for all constant wd)
  wdes <- c("market", "gross", "disp", "con")
  rho_es0 <- map_df(c("market", "gross", "disp", "con"), function(w) {
    ineq1 %>%
      select(-gini_cat_se) %>%
      spread(key = wdes, value = gini_cat) %>%
      mutate(bl = get(paste0(w, "_", baseline_es))) %>%
      mutate_at(vars(matches(w)),
                funs(bl/.)) %>%
      select(kcode, tcode, matches(w)) %>%
      gather(key = wdes, value = rho_es, -kcode, -tcode) %>%
      filter(!is.na(rho_es)) %>%
      mutate(es = str_replace(wdes, ".*_", "")) %>%
      select(-wdes) %>%
      arrange(kcode, tcode, es)
  })
  
  rho_es_se <- map_df(c("market", "gross", "disp", "con"), function(w) {
    ineq1 %>%
      select(-gini_cat) %>%
      spread(key = wdes, value = gini_cat_se) %>%
      mutate(bl = get(paste0(w, "_", baseline_es))) %>%
      mutate_at(vars(matches(w)),
                funs(sqrt(bl^2+.^2))) %>%
      select(kcode, tcode, matches(w)) %>%
      gather(key = wdes, value = rho_es_se, -kcode, -tcode) %>%
      filter(!is.na(rho_es_se)) %>%
      mutate(es = str_replace(wdes, ".*_", "")) %>%
      select(-wdes) %>%
      arrange(kcode, tcode, es)
  })
  
  rho_es <- rho_es0 %>%
    left_join(rho_es_se, by = c("kcode", "tcode", "es")) %>%
    group_by(kcode, tcode, es) %>%
    summarize(rho_es = max(rho_es),
              rho_es_se = max(rho_es_se)) %>%
    ungroup() %>%
    left_join(ineq %>% select(country, year, kcode, tcode, rcode, kbl) %>% distinct(),
              by = c("kcode", "tcode")) %>%
    left_join(wecodes %>% select("es", "ecode") %>% distinct(), by = "es") %>%
    mutate(kecode = as.integer(factor(100*kcode+ecode)),
           recode = as.integer(factor(100*rcode+ecode)),
           kes = paste(country, es),
           res = paste(rcode, es))
  
  rm(rho_es0, rho_es_se)
  
  rho_es_ke <- rho_es %>%
    pull(kes) %>%
    unique()
  
  kyrs <- ineq %>%
    group_by(kcode) %>%
    summarize(firstyr = min(year),
              lastyr = max(year),
              n_yrs = year %>% unique() %>% length()) %>% 
    ungroup()
  
  ineq2 <- ineq %>% 
    left_join(kyrs, by = "kcode") %>% 
    mutate(kwd = paste(country, str_replace(wdes, "_.*", "")),
           kes = paste(country, str_replace(wdes, ".*_", "")),
           rwd = paste(rcode, str_replace(wdes, "_.*", "")),
           res = paste(rcode, str_replace(wdes, ".*_", "")),
           kw = (kwd %in% rho_wd_kw),
           ke = (kes %in% rho_es_ke)) %>% 
    left_join(rho_wd %>% select(kwd, kwcode) %>% unique(), by = "kwd") %>% 
    left_join(rho_wd %>% select(rwd, rwcode) %>% unique(), by = "rwd") %>% 
    left_join(rho_es %>% select(kes, kecode) %>% unique(), by = "kes") %>% 
    left_join(rho_es %>% select(res, recode) %>% unique(), by = "res") %>% 
    mutate(kwcode = if_else(is.na(kwcode), 0L, kwcode),
           kecode = if_else(is.na(kecode), 0L, kecode)) %>% 
    arrange(desc(ibl), desc(bl), desc(obl), desc(kbl), desc(kw), desc(ke), desc(k_bl_obs), desc(country_obs))
  
  return(list(ineq2, rho_we, rho_wd))
}

leave_k_out <- function(fold) {
  if (!file.exists(paste0("/Volumes/Platón-Media/Media/Projects/swiid/kfold/fold_", fold, ".rda"))) {
    disp <- make_inputs(baseline_series = baseline_series, fold = fold)
    ineq2 <- disp[[1]]
    rho_we <- disp[[2]]
    rho_wd <- disp[[3]]
    
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
    
    
    rho_we <- rho_we %>%
      select(-ends_with("code")) %>% 
      left_join(x0 %>% 
                  select("country", "year", "wdes",
                         "kcode", "rcode", "tcode", 
                         "wcode", "ecode", "wecode", "kwecode", "rwecode") %>% 
                  distinct(),
                by = c("country", "year", "wdes"))
    
    rho_wd <- rho_wd %>%
      select(-ends_with("code")) %>% 
      left_join(x0 %>% 
                  rename(wd = "welfare_def") %>% 
                  select("country", "year", "wd", "kbl",
                         "kcode", "rcode", "tcode", 
                         "wcode", "kwcode", "rwcode",
                         "kwd", "rwd") %>% 
                  distinct(),
                by = c("country", "year", "wd"))
    
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
    
    seed <- 324
    iter <- 6000
    warmup <- iter - 2000
    chains <- 3
    thin <- 4
    cores <- chains
    adapt_delta <- .9
    
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
    cat(fold)
    
    save(x, out1, file = paste0("/Volumes/Platón-Media/Media/Projects/swiid/kfold/fold_", fold, ".rda"))

    cy_fold <- baseline_rnd %>% 
      filter(fold_number == fold) %>%
      mutate(cy = paste(country, year),
             gini_b = gini,
             gini_b_se = gini_se * 2) %>% 
      select(country, year, gini_b, gini_b_se)
    
    ktcodes <- x %>%  
      transmute(kcode = kcode,
                country = country,
                firstyr = firstyr,
                yrspan = (lastyr - firstyr) + 1) %>% 
      distinct() %>% 
      slice(rep(1:n(), yrspan)) %>% 
      group_by(kcode) %>% 
      mutate(tcode = 1:n(),
             year = firstyr + tcode - 1) %>% 
      ungroup() %>% 
      mutate(ktcode = 1:n())
    
    probs <- c(.025, .975)
    fold_res <- rstan::summary(out1, pars = "gini", probs = probs) %>%
      first() %>%
      as.data.frame() %>%
      rownames_to_column("parameter") %>%
      as_tibble() %>%
      mutate(estimate = mean,
             lb = get(paste0(probs*100, "%")[1]),
             ub = get(paste0(probs*100, "%")[2]),
             ktcode = as.numeric(str_extract(parameter, "(?<=\\[)\\d+"))) %>%
      left_join(ktcodes, by="ktcode") %>% 
      arrange(kcode, tcode) %>% 
      right_join(cy_fold, by = c("country", "year")) %>% 
      mutate(point_diff = abs(mean - gini_b) %>% round(5),
             se_diff = sqrt(sd^2 + gini_b_se^2) %>% round(5),
             problem = (point_diff > 1.96*se_diff)) %>% 
      select(country, year, point_diff, se_diff, problem, mean, sd, ub, lb, gini_b, gini_b_se, everything())
    
    save(fold_res, file = paste0("/Volumes/Platón-Media/Media/Projects/swiid/kfold/res_", fold, ".rda"))
  }  
}

