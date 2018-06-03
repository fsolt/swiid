library(tidyverse)
library(rstan)
library(beepr)

load("data/ineq.rda")

seed <- 324
iter <- 2200
warmup <- iter - 200
chains <- 3
cores <- chains
adapt_delta <- .9

baseline_series <- "LIS disp sqrt"
baseline_wd <- str_split(baseline_series, "\\s")[[1]] %>% nth(-2)
baseline_es <- str_split(baseline_series, "\\s")[[1]] %>% last()

x0 <- ineq2
  
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
  left_join(kt, by = c("kcode", "tcode"))

kn <- x %>% 
  group_by(kcode) %>% 
  summarize(kt1 = min(ktcode),
            yrspan = first(yrspan),
            kr = first(rcode)) %>% 
  ungroup()

gamma_ke1 <- gamma_ke %>% 
    filter(kcode %in% x$kcode)

gamma_kw1 <- gamma_kw %>% 
  filter(kcode %in% x$kcode)

wwe <- x %>%
  group_by(wecode) %>%
  summarize(wcode = first(wcode)) %>% 
  pull(wcode)

wrw <- x %>% 
  group_by(rwcode) %>% 
  summarize(wcode = first(wcode))

ere <- x %>% 
  group_by(recode) %>% 
  summarize(ecode = first(ecode))

rwkw <- x %>% 
  group_by(kwcode) %>% 
  summarize(rwcode = first(rwcode))

reke <- x %>% 
  group_by(kecode) %>% 
  summarize(recode = first(recode))

k_kwe <- x %>% 
  group_by(kwecode) %>% 
  summarize(kwcode = first(kwcode),
            kecode = first(kecode))

kwes <- x %>% 
  group_by(scode) %>% 
  summarize(kwecode = first(kwecode)) %>% 
  pull(kwecode)

# Format data for Stan
source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      KT = nrow(kt),
                      R = max(x$rcode),
                      S = max(x$scode),
                      SO = max(x %>% filter(obl) %>% pull(scode)),
                      SN = max(x %>% filter(kw & ke) %>% pull(scode)) - max(x %>% filter(obl) %>% pull(scode)),
                      SW = max(x %>% filter(kw & !ke) %>% pull(scode)) - max(x %>% filter(kw & ke) %>% pull(scode)),
                      SE = max(x %>% filter(!kw & ke) %>% pull(scode)) - max(x %>% filter(kw & !ke) %>% pull(scode)),
                      WE = max(x$wecode),
                      KWE = max(x$kwecode),
                      KW = max(x$kwcode),
                      KE = max(x$kecode),
                      RW = max(x$rwcode),
                      RE = max(x$recode),
                      W = max(x$wcode),
                      E = max(x$ecode),
                      
                      N = nrow(x),
                      N_ibl = x %>% filter(ibl) %>% nrow(),
                      N_wbl = x %>% filter(bl) %>% nrow(),
                      N_obl = x %>% filter(obl) %>% nrow(),
                      N_nbl = nrow(x) - (x %>% filter(obl) %>% nrow()),
                      
                      kk = x$kcode,
                      tt = x$tcode,
                      kktt = x$ktcode,
                      ktt = kt$tcode,
                      ktk = kt$kcode,
                      kn = kn$yrspan,
                      kt1 = kn$kt1,
                      rr = x$rcode,
                      ss = x$scode,
                      sn = x %>% filter(!obl) %>% mutate(sn = scode - (x %>% filter(obl) %>% pull(scode) %>% max())) %>% pull(sn),
                      wen = x$wecode,
                      kwen = x$kwecode,
                      gini_n = x$gini_n,
                      gini_n_se = x$gini_n_se,
                      gini_b = x$gini_b[!is.na(x$gini_b)],
                      gini_b_se = x$gini_b_se[!is.na(x$gini_b_se)],
                      
                      rk = kn$kr,
                      
                      wrw = wrw$wcode,
                      ere = ere$ecode, 
                      rwkw = rwkw$rwcode,
                      reke = reke$recode,
                      kwkwe = k_kwe$kwcode,
                      kekwe = k_kwe$kecode,
                      kwes = kwes,
                      
                      M = nrow(gamma_ke1),
                      kkm = gamma_ke1$kcode,
                      kem = gamma_ke1$kecode,
                      blm = gamma_ke1$bl,
                      gini_m = gamma_ke1$gini_m,
                      
                      P = nrow(gamma_kw1),
                      kkp = gamma_kw1$kcode,
                      kwp = gamma_kw1$kwcode,
                      blp = gamma_kw1$bl,
                      gini_p = gamma_kw1$gini_p
)

# Stan
start <- proc.time()
out1 <- stan(file = "R/estimate_swiid/all2.stan",
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
beepr::beep()

lapply(get_sampler_params(out1, inc_warmup = FALSE),
       summary, digits = 2)

save(x, out1, file = str_c("data/all2_", iter/1000, "k_", 
                        str_replace(Sys.time(), " ", "_") %>% str_replace("2018-", ""), ".rda"))

# Plots
source("R/plot_tscs.R")
plot_tscs(x, out1, save_pdf = "paper/figures/ts_all2_.pdf")
plot_tscs(x, out1)

shinystan::launch_shinystan(out1)

beep() # chime
