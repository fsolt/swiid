regions <- read_csv("data/reg.csv", col_types = "cc") %>%
  mutate(swiid.name = if_else(str_detect(swiid.name, "Ivoire"), "Côte d'Ivoire", swiid.name),
         swiid.name = if_else(str_detect(swiid.name, "ncipe"), "São Tomé and Príncipe", swiid.name))
write_csv(regions, "data/reg.csv")

cc_swiid <- countrycode::codelist %>% 
  select(country.name.en, country.name.en.regex, iso2c, iso3c, wb_api3c) %>% 
  mutate(swiid.name = country.name.en %>% 
           str_replace(" & ", " and ") %>%
           str_replace(" - ", "-") %>%
           str_replace(" \\(.*", "") %>% 
           str_replace(", [^U]*", "") %>% 
           str_replace("^(United )?Republic of ", "") %>% 
           str_replace("^The former Yugoslav Republic of ", "") %>% 
           str_replace(" of [GA].*", "") %>% 
           str_replace("Czechia", "Czech Republic") %>% 
           str_replace("Russian Federation", "Russia") %>% 
           str_replace("Syrian Arab Republic", "Syria") %>% 
           str_replace("Côte d’Ivoire", "Côte d'Ivoire") %>% 
           str_replace("Hong Kong SAR China", "Hong Kong") %>% 
           str_replace("South Korea", "Korea"),
         country.name.en.regex = case_when(swiid.name == "Russia" ~ "\\brussia",
                                           swiid.name == "Central African Republic" ~ "\\bcentral.african.rep",
                                           swiid.name == "Micronesia" ~ "micronesia",
                                           swiid.name == "Bolivia" ~ "bolivia\\b",
                                           swiid.name == "Dominica" ~ "dominica\\b",
                                           swiid.name == "Dominican Republic" ~ "dominican",
                                           TRUE ~ country.name.en.regex)) %>% 
  full_join(tibble(swiid.name = c("Soviet Union", "Greenland"),
                   country.name.en.regex = c("soviet.?union|u\\.?s\\.?s\\.?r|socialist.?republics",
                                             "greenland")),
            by = c("country.name.en.regex", "swiid.name")) %>% 
  left_join(read_csv("data/reg.csv", col_types = "cc"), by = "swiid.name") 

save(cc_swiid, file = "data/cc_swiid.rda")
