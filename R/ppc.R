library(tidyverse)

previous_workspace <- ls()

load("data/ineq0.rda")
rm(list=setdiff(ls(), c(previous_workspace, "lis")))
all_file <- list.files("data/", "all_[^m]") %>% 
  last() %>% 
  file.path("data", .)
load(all_file)
all_in <- x
all_out <- out1

swiid_disp_summary <- summary_kt(all_in, all_out) %>% 
  transmute(country = country,
                          year = year,
                          gini_disp = round(gini*100, 1),
                          gini_disp_se = round(se*100, 2)) %>% 
  arrange(country, year)

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

lis_vs_swiid <- left_join(lis %>% filter(series == "LIS disp sqrt"),
          swiid_disp_summary %>% select(country, year, gini_disp, gini_disp_se), 
          by = c("country", "year")) %>%
  mutate(diff = gini*100 - gini_disp, abs_diff = abs(diff),
         diff_se = sqrt((gini_se*100)^2 + gini_disp_se^2),
         diff_t = abs_diff/diff_se) %>%
  select(country, year, gini, gini_disp, diff, abs_diff, diff_se, diff_t, everything())
View(lis_vs_swiid)
