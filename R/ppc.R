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
lis_vs_swiid <- left_join(lis %>% filter(series == "LIS disp sqrt"),
          swiid_disp_summary %>% select(country, year, gini_disp, gini_disp_se), 
          by = c("country", "year")) %>%
  mutate(diff = gini*100 - gini_disp, abs_diff = abs(diff),
         diff_se = sqrt((gini_se*100)^2 + gini_disp_se^2),
         diff_t = abs_diff/diff_se) %>%
  select(country, year, gini, gini_disp, diff, abs_diff, diff_se, diff_t, everything())
View(lis_vs_swiid)
