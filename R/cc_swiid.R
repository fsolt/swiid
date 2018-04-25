regions <- read_csv("data/reg.csv", col_types = "cc") %>%
  mutate(swiid.name = if_else(swiid.name=="C̫te D'Ivoire", "Côte D'Ivoire", swiid.name))
write_csv(regions, "data/reg.csv")

cc_swiid <- countrycode::codelist %>% 
  mutate(swiid.name = country.name.en %>% 
           str_replace(" \\(.*", "") %>% 
           str_replace(", [^U]*", "") %>% 
           str_replace("^(United )?Republic of ", "") %>% 
           str_replace("^The former Yugoslav Republic of ", "") %>% 
           str_replace(" of [GA].*", "") %>% 
           str_replace("Democratic Republic of the Congo", "Congo, D.R.") %>% 
           str_replace("Lao People's Democratic Republic", "Lao") %>% 
           str_replace("Russian Federation", "Russia") %>% 
           str_replace("Syrian Arab Republic", "Syria"),
         country.name.en.regex = case_when(swiid.name == "Russia" ~ "\\brussia",
                                           swiid.name == "Central African Republic" ~ "\\bcentral.african.rep",
                                           swiid.name == "Micronesia" ~ "micronesia",
                                           swiid.name == "Bolivia" ~ "bolivia\\b",
                                           swiid.name == "Dominica" ~ "dominica\\b",
                                           swiid.name == "Dominican Republic" ~ "dominican",
                                           TRUE ~ country.name.en.regex)) %>% 
  filter(!country.name.en == "Korea") %>% 
  full_join(tibble(swiid.name = "Soviet Union", 
                   country.name.en.regex = "soviet.?union|u\\.?s\\.?s\\.?r|socialist.?republics"),
            by = c("country.name.en.regex", "swiid.name")) %>% 
  left_join(read_csv("data/reg.csv", col_types = "cc"), by = "swiid.name") 

save(cc_swiid, file = "data/cc_swiid.rda")
