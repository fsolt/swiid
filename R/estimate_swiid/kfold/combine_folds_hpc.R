library(tidyverse)

output_path <- "/Volumes/fsolt/swiid_kfold/output"
(output_file_ids <- list.files(output_path) %>%
  str_extract("\\d+(?=\\.)") %>%
  unique())
output_files <- list.files(output_path) %>% 
  str_subset(output_file_ids %>% nth(length(output_file_ids)))

kfold_output <- map_dfr(output_files, function(output_file) {
  if(readLines(file.path(output_path, output_file)) %>% 
     paste(collapse = "") %>% 
     str_detect("structure")) {
    (readLines(file.path(output_path, output_file)) %>% 
       paste(collapse = "") %>% 
       str_extract("structure.*L\\)\\)") %>% 
       parse(text = .) %>% 
       eval() %>% 
       mutate(fold = str_extract(output_file, "\\d{1,2}$")))
  }
}) %>% 
  group_by(country) %>% 
  mutate(prob_perc = mean(problem, na.rm = TRUE),
         t_diff = point_diff/se_diff) %>% 
  ungroup()

mean(kfold_output$problem, na.rm = TRUE)
nrow(kfold_output)

kfold_output %>% 
  group_by(country) %>%
  select(country, year, fold, problem, prob_perc, 
         gini_b, gini_b_se, mean, sd,
         point_diff, se_diff, t_diff) %>%
  arrange(-problem, -prob_perc, country, -t_diff) %>%
  View()
