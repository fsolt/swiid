library(tidyverse)
library(cmdstanr)

version <- "9_6"
ver <- str_replace(version, "_", "")

load("data/ineq.rda")

all_dir <- list.files("data/swiid_estimates/", "all_[^m]") %>% 
  last() %>% 
  file.path("data", "swiid_estimates", .)

all_mkt_dir <- list.files("data/swiid_estimates/", "all_mkt") %>% 
  last() %>% 
  file.path("data", "swiid_estimates", .)

all_in <- rio::import(here::here(all_dir,
                                 "all_in.rda"))
all_out <- as_cmdstan_fit(here::here(all_dir,
                                     list.files(all_dir, pattern = "csv$")))

all_mkt_in <- rio::import(here::here(all_mkt_dir,
                                 "all_mkt_in.rda"))
all_mkt_out <- as_cmdstan_fit(here::here(all_mkt_dir,
                                         list.files(all_mkt_dir, pattern = "csv$")))

summary_kt <- function(input, output, probs = c(.025, .975)) {
  ktcodes <- input %>%  
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

  gini_res <- output$summary("gini",
                             ~posterior::quantile2(., probs = probs),
                             c("mean", "sd")) %>%
    mutate(gini = mean,
           lb = get(paste0("q", probs[1]*100)),
           ub = get(paste0("q", probs[2]*100)),
           se = sd,
           ktcode = as.numeric(str_extract(variable, "(?<=\\[)\\d+"))) %>%
    left_join(ktcodes, by="ktcode") %>% 
    select(country, year, gini, lb, ub, se, ktcode) %>% 
    arrange(ktcode)

  return(gini_res)
}

swiid_disp_summary <- summary_kt(all_in, all_out) %>% 
  transmute(country = country,
            year = year,
            gini_disp = round(gini*100, 1),
            gini_disp_se = round(se*100, 2)) %>% 
  arrange(country, year)

swiid_mkt_summary <- summary_kt(all_mkt_in, all_mkt_out) %>% 
  transmute(country = country,
            year = year,
            gini_mkt = round(gini*100, 1),
            gini_mkt_se = round(se*100, 2)) %>% 
  arrange(country, year)

swiid_source <- read_csv("data/swiid_source.csv")
load("data/cc_swiid.rda")

k_redist <- rho_wd_m %>% 
  filter(wd == "disp" | wd == "con") %>% 
  distinct(country) %>%
  mutate(region = countrycode::countrycode(country, "swiid.name", "swiid.region", 
                                           custom_dict = {cc_swiid %>% distinct(swiid.name, .keep_all = TRUE)}),
         redist_after = recode(region, 
                               "AES" = 1975L,
                               "DAA" = 1985L,
                               "AT" = 1985L,
                               "EE" = 1993L,
                               "FSU" = 1993L,
                               "LAC" = 1985L,
                               "WE" = 1975L)) %>% 
  select(-region)

swiid_summary <- left_join(swiid_disp_summary, swiid_mkt_summary, by = c("country", "year")) %>% 
  left_join(k_redist, by = c("country")) %>% 
  mutate(redist = (year >= redist_after),
         redist = if_else(!is.na(redist), redist, FALSE),
         abs_red = ifelse(redist, (gini_mkt - gini_disp) %>% round(1), NA_real_),
         abs_red_se = ifelse(redist, sqrt(gini_mkt_se^2 + gini_disp_se^2) %>% round(1), NA_real_),
         rel_red = ifelse(redist, ((abs_red/gini_mkt)*100) %>% round(1), NA_real_),
         rel_red_se = ifelse(redist, sqrt(abs_red_se^2 + gini_mkt_se^2) %>% round(1), NA_real_))

kt_redist <- swiid_summary %>%
  select(country, year, redist)
  
write_csv(swiid_summary %>% select(-redist, -redist_after), "data/swiid_summary.csv", na = "")
write_csv(swiid_summary %>% select(-redist, -redist_after), "../SWIIDweb/swiid_summary.csv", na = "")
write_csv(swiid_summary %>% select(-redist, -redist_after), "../SWIIDweb_source/swiid_summary.csv", na = "")

swiid_kt <- function(input, output, probs = c(.025, .975)) {
  ktcodes <- input %>%  
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
    mutate(ktcode = 1:n()) %>% 
    select(ktcode, country, year)
  
  gini_type <- ifelse(str_detect(as.list(match.call())["input"], "_mkt"), "gini_mkt_", "gini_disp_")
  
  res <- output$draws(variables = "gini", 
                      format = "df") %>% 
    select(contains("gini")) %>% 
    t() %>%               # parameters in rows, iterations in columns
    as.data.frame() %>% 
    transmute(across(V1:V100,
              ~ . * 100)) %>% 
    rowid_to_column("ktcode") %>% 
    left_join(ktcodes, by = "ktcode") %>% 
    rename_with(~ str_replace(., "V", gini_type)) %>% 
    select(country, year, everything(), -ktcode)
  
  return(res)
}

res <- swiid_kt(all_in, all_out) %>% 
  pivot_longer(cols = starts_with("gini_disp_"), names_to = "iter", values_to = "gini_disp") %>% 
  mutate(iter = as.numeric(str_extract(iter, "\\d+"))) %>% 
  left_join(swiid_kt(all_mkt_in, all_mkt_out) %>% 
              pivot_longer(cols = starts_with("gini_mkt_"), names_to = "iter", values_to = "gini_mkt") %>% 
              mutate(iter = as.numeric(str_extract(iter, "\\d+")))) %>% 
  left_join(kt_redist, by = c("country", "year")) %>% 
  mutate(abs_red = ifelse(redist, (gini_mkt - gini_disp) %>% round(1), NA_real_),
         rel_red = ifelse(redist, ((abs_red/gini_mkt)*100) %>% round(1), NA_real_)) %>% 
  pivot_longer(cols = c("gini_disp", "gini_mkt", "abs_red", "rel_red")) %>% 
  unite(col = "var", c("name", "iter"), sep = "_") %>% 
  pivot_wider(names_from = var,
              values_from = value)

# Stata formatted
res_stata <- res %>% select(country, year, 
                            starts_with("gini_disp"), 
                            starts_with("gini_mkt"), 
                            redist,
                            starts_with("abs_red"),
                            starts_with("rel_red"))

haven::write_dta(res_stata, "data/for_stata.dta", version = 12)   # for format_stata.do
RStata::stata("R/format_stata.do")    # see https://github.com/lbraglia/RStata for setup instructions
file.copy(from = paste0("data/swiid", version, ".dta"), 
          to = paste0("vignette/swiid", version, ".dta"),
          overwrite = TRUE)

# R formatted
swiid <- list()
for (i in 1:100) {
  stemp <- res %>%
    transmute(country = country,
              year = year,
              gini_disp = get(paste0("gini_disp_", i)) * 100, 
              gini_mkt = get(paste0("gini_mkt_", i)) * 100,
              abs_red = gini_mkt - gini_disp,
              rel_red = (abs_red/gini_mkt)*100)
  swiid[[i]] <- stemp
}

save(swiid, swiid_summary, file = paste0("data/swiid", version, ".rda"))
save(swiid, swiid_summary, file = paste0("vignette/swiid", version, ".rda"))

# for release
dir.create(paste0("release/swiid", version, ""), recursive = TRUE)
final_files <- c("data/swiid_summary.csv", paste0("data/swiid", version, ".rda"), paste0("data/swiid", version, ".dta"))
file.copy(from = final_files,
          to = str_replace(final_files, "data/", paste0("release/swiid", version, "/")),
          overwrite = TRUE)
file.rename(paste0("release/swiid", version, "/swiid_summary.csv"), paste0("release/swiid", version, "/swiid", version, "_summary.csv"))
documentation_files <- c("vignette/R_swiid.pdf", "vignette/stata_swiid.pdf")
file.copy(from = documentation_files,
          to = str_replace(documentation_files, "vignette/", paste0("release/swiid", version, "/")),
          overwrite = TRUE)
setwd("release")
zip(paste0("swiid", version, ".zip"), paste0("swiid", version))
dir.create(paste0("s", ver))
file.copy(paste0("swiid", version, ".zip"), paste0("s", str_replace(version, "_", ""), "/swiid", version, ".zip"), overwrite = TRUE)
zip(paste0("s", ver, ".zip"), paste0("s", ver))
setwd("..")
beepr::beep()
