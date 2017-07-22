library(tidyverse)
library(stringr)

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
        mutate(ktcode = 1:n())
    
    gini_res <- rstan::extract(lis_out, pars="gini") %>%
        first() %>%
        as.data.frame() %>%
        rowid_to_column("iter") %>%
        as_tibble() %>%
        group_by(iter) %>% 
        do(data = (.)) %>% 
        select(data) %>%
        map(identity()) %>% 
        first()
        
        left_join(ktcodes, by="ktcode")
        
    swiid <- list()
    for (i in 1:100) {
        stemp <- data_frame(country, year, gini_net=get(paste0("gini_net_",i)), gini_market=get(paste0("gini_market_",i)), rel_red=get(paste0("rel_red_",i)), abs_red=get(paste0("abs_red_",i)))
        swiid[[i]] <- stemp
    }
    
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


not_lis_file <- list.files("data/", "all_not_lis") %>% 
  last() %>% 
  file.path("data", .)
lis_file <- list.files("data/", "all_lis") %>% 
  last() %>% 
  file.path("data", .)

load(not_lis_file)
not_lis_in <- x
not_lis_out <- out1

load(lis_file)
lis_in <- x
lis_out <- out1

rm(x, out1)

lis_summary <- summary_kt(lis_in, lis_out)
not_lis_summary <- summary_kt(not_lis_in, not_lis_out)

swiid_summary <- bind_rows(lis_summary, not_lis_summary) %>% 
    transmute(country = country,
              year = year,
              gini_disp = round(gini*100, 2),
              gini_disp_se = se*100) %>% 
    arrange(country, year)

# need to add market (and calculate rel_red and abs_red for countries with both market and disp data in source)



