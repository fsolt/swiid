library(tidyverse)
library(stringr)

lis_file <- list.files("data/", "all_lis_[^m]") %>% 
  last() %>% 
  file.path("data", .)
not_lis_file <- list.files("data/", "all_not_lis_[^m]") %>% 
  last() %>% 
  file.path("data", .)

lis_mkt_file <- list.files("data/", "all_lis_mkt") %>% 
  last() %>% 
  file.path("data", .)
# not_lis_mkt_file <- list.files("data/", "all_not_lis_mkt") %>% 
#   last() %>% 
#   file.path("data", .)

load(lis_file)
lis_in <- x
lis_out <- out1

load(not_lis_file)
not_lis_in <- x
not_lis_out <- out1

load(lis_mkt_file)
lis_mkt_in <- x
lis_mkt_out <- out1

# load(not_lis_mkt_file)
# not_lis_mkt_in <- x
# not_lis_mkt_out <- out1

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
           lb = get(paste0("x", str_replace(probs*100, "\\.", "_"), "percent")[1]),
           ub = get(paste0("x", str_replace(probs*100, "\\.", "_"), "percent")[2]),
           se = round((ub - lb)/(qnorm(.975)*2), 4),
           ktcode = as.numeric(str_extract(parameter, "(?<=\\[)\\d+"))) %>%
    left_join(ktcodes, by="ktcode") %>% 
    select(country, year, gini, lb, ub, se, ktcode) %>% 
    arrange(ktcode)

  return(gini_res)
}

lis_summary <- summary_kt(lis_in, lis_out)
not_lis_summary <- summary_kt(not_lis_in, not_lis_out)
lis_mkt_summary <- summary_kt(lis_mkt_in, lis_mkt_out)
not_lis_mkt_summary <- summary_kt(not_lis_mkt_in, not_lis_mkt_out)

swiid_disp_summary <- bind_rows(lis_summary, not_lis_summary) %>% 
    transmute(country = country,
              year = year,
              gini_disp = round(gini*100, 2),
              gini_disp_se = se*100) %>% 
    arrange(country, year)

swiid_mkt_summary <- lis_mkt_summary %>% # bind_rows(lis_mkt_summary, not_lis_mkt_summary) %>% 
  transmute(country = country,
            year = year,
            gini_mkt = round(gini*100, 2),
            gini_mkt_se = se*100) %>% 
  arrange(country, year)

swiid_summary <- left_join(swiid_disp_summary, swiid_mkt_summary, by = c("country", "year")) %>% 
  mutate(abs_red = gini_mkt - gini_disp,
         abs_red_se = sqrt(gini_mkt_se^2 + gini_disp_se^2),
         rel_red = (abs_red/gini_mkt)*100,
         rel_red_se = sqrt(abs_red_se^2 + gini_mkt_se^2))


# R formatted
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

res <- bind_rows(swiid_kt(lis_in, lis_out), swiid_kt(not_lis_in, not_lis_out)) %>%  
  left_join(swiid_kt(lis_mkt_in, lis_mkt_out), by = c("country", "year")) %>%         # add not_lis_mkt data
  arrange(country, year)

haven::write_dta(res, "data/for_stata.dta", version = 12)
  
swiid <- list()
for (i in 1:100) {
  stemp <- res %>%
    transmute(country = country,
              year = year,
              gini_disp = get(paste0("gini_disp_",i)) * 100, 
              gini_mkt = get(paste0("gini_mkt_",i)) * 100,
              abs_red = gini_mkt - gini_disp,
              rel_red = (abs_red/gini_mkt)*100)
  swiid[[i]] <- stemp
}

save(swiid, file = "data/SWIIDv6_0.rda") # for release
