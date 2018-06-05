library(tidyverse)

load("data/ineq0.rda")

baseline_series <- "LIS disp sqrt"

set.seed(324)
problem1 <- 1
while (problem1 > 0) {
  baseline_rnd <- lis %>% 
    filter(series == baseline_series) %>% 
    mutate(gini_b = gini,
           gini_b_se = gini_se * 2) %>%
    select(-gini, -gini_se) %>% 
    group_by(country) %>% 
    mutate(f1 = 1:n()) %>% 
    ungroup() %>% 
    mutate(f2 = c(rep(1:max(f1), length(f1) %/% max(f1)), 1:(length(f1) %% max(f1)))) %>%
    sample_n(., size = nrow(.), replace = FALSE) %>%
    arrange(f2) %>% 
    mutate(fold_number = cut(seq(1, nrow(.)),
                             breaks = ceiling(nrow(.)/3),
                             labels = FALSE)) %>% 
    group_by(fold_number) %>% 
    mutate(countries = n_distinct(country),
           problem = countries < n()) %>% 
    ungroup()
  
  problem1 <- mean(baseline_rnd$problem)
}

rm(list = setdiff(ls(), "baseline_rnd"))

fold_path <- "/Volumes/Platón-Media/Media/Projects/swiid/kfold"
files <- list.files(fold_path, pattern = "fold_\\d+.rda")
folds <- baseline_rnd %>%
  pull(fold_number) %>% 
  unique()

cross_validation <- map_df(folds, function(fold) {
  obs <- baseline_rnd %>% 
    filter(fold_number == fold) %>% 
    select(country, year, gini_b, gini_b_se, fold_number)
  
  if (file.exists(file.path(fold_path, files[fold]))) {
    load(file.path(fold_path, files[fold]))
    
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
    gini_res <- rstan::summary(out1, pars = "gini", probs = probs) %>%
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
      right_join(obs, by = c("country", "year"))
    
    return(gini_res)}
})

cross_validation1 <- cross_validation %>% 
  mutate(point_diff = abs(mean - gini_b) %>% round(5),
         se_diff = sqrt(sd^2 + gini_b_se^2) %>% round(5),
         problem = (point_diff > 1.96*se_diff)) %>% 
  select(country, year, point_diff, se_diff, problem, mean, sd, ub, lb, gini_b, gini_b_se, everything()) %>% 
  arrange(desc(problem), desc(point_diff))

beepr::beep()

save(list = "cross_validation1", file = "data/cross_validation1.rda")

