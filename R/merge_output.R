library(tidyverse)

version <- "9_0"
ver <- str_replace(version, "_", "")

load("data/ineq.rda")

all_file <- list.files("data/", "all_[^m]") %>% 
  last() %>% 
  file.path("data", .)

all_mkt_file <- list.files("data/", "all_mkt") %>% 
  last() %>% 
  file.path("data", .)

load(all_file)
all_in <- x
all_out <- out1

load(all_mkt_file)
all_mkt_in <- x
all_mkt_out <- out1

rm(x, out1)

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

  gini_res <- rstan::summary(output, pars="gini", probs=probs) %>%
    first() %>%
    as.data.frame() %>%
    rownames_to_column("parameter") %>%
    as_tibble() %>%
    janitor::clean_names() %>% 
    mutate(gini = mean,
           lb = get(paste0("x", str_replace(probs*100, "\\.", "_"), "_percent")[1]),
           ub = get(paste0("x", str_replace(probs*100, "\\.", "_"), "_percent")[2]),
           se = round((ub - lb)/(qnorm(.975)*2), 3),
           ktcode = as.numeric(str_extract(parameter, "(?<=\\[)\\d+"))) %>%
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
  mutate(region = countrycode::countrycode(country, "swiid.name", "swiid.region", custom_dict = cc_swiid),
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
  
  res <- rstan::extract(output, pars="gini") %>% # should be output; it's lis_out for testing
    first() %>%
    t() %>%               # parameters in rows, iterations in columns
    as.data.frame() %>% 
    select(V1:V100) %>% 
    rowid_to_column("ktcode") %>% 
    left_join(ktcodes, by = "ktcode") %>% 
    rename_all(funs(str_replace(., "V", gini_type))) %>% 
    select(country, year, everything(), -ktcode)
  
  return(res)
}

res <- swiid_kt(all_in, all_out) %>%  
  left_join(swiid_kt(all_mkt_in, all_mkt_out),
            by = c("country", "year")) %>% 
  left_join(kt_redist, by = c("country", "year")) %>% 
  arrange(country, year)

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
zip(paste0("swiid", version, ".zip"), "swiid9_0")
dir.create(paste0("s", ver))
file.copy(paste0("swiid", version, ".zip"), paste0("s", str_replace(version, "_", ""), "/swiid", version, ".zip"), overwrite = TRUE)
zip(paste0("s", ver, ".zip"), paste0("s", ver))
setwd("..")
