cc_swiid <- countrycode::countrycode_data %>% 
  mutate(swiid.name = country.name.en %>% 
           str_replace(" \\(.*", "") %>% 
           str_replace(",.*", "") %>% 
           str_replace("^(United )?Republic of ", "") %>% 
           str_replace("^The former Yugoslav Republic of ", "") %>% 
           str_replace(" of [GA].*", "") %>% 
           str_replace("Democratic Republic of the Congo", "Congo, D.R.") %>% 
           str_replace("Lao People's Democratic Republic", "Lao"),
         country.name.en.regex = if_else(swiid.name == "Russian Federation",
                                          "\\brussia",
                                          if_else(swiid.name == "Central African Republic",
                                                  "\\bcentral.african.rep",
                                                  country.name.en.regex))) %>% 
  filter(!country.name.en == "Korea") %>% 
  full_join(tibble(swiid.name = "Soviet Union", 
                   country.name.en.regex = "soviet.?union|u\\.?s\\.?s\\.?r|socialist.?republics"),
            by = c("country.name.en.regex", "swiid.name"))

save(cc_swiid, file = "data/cc_swiid.rda")
