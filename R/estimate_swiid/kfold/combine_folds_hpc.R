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
       mutate(fold = str_extract(output_file, "\\d{1,2}$"),
              cy = paste(country, year),
              cy_color = if_else(problem == 1, "#354995", "#5E5E5E"),
              point_diff = mean - gini_b %>% round(5)))
  }
}) %>% 
  group_by(country) %>% 
  mutate(prob_perc = mean(problem, na.rm = TRUE),
         t_diff = point_diff/se_diff) %>% 
  ungroup() %>% 
  arrange(point_diff)
mean(kfold_output$problem, na.rm = TRUE)
nrow(kfold_output)

kfold_output %>% 
  group_by(country) %>%
  select(country, year, fold, problem, prob_perc, 
         gini_b, gini_b_se, mean, sd,
         point_diff, se_diff, t_diff) %>%
  arrange(-problem, -prob_perc, country, -t_diff) %>%
  View()


ggplot(kfold_output) + 
  # geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-2, ymax=2), fill='gray90', alpha=.2) +
  geom_hline(yintercept=0, linetype=2, colour="gray60") +
  geom_pointrange(fatten = 1,
                  aes(x = forcats::fct_reorder(cy, point_diff), 
                      y=point_diff*100, 
                      ymin=point_diff*100 - 1.96*100*se_diff,
                      ymax = point_diff*100 + 1.96*100*se_diff,
                      colour = cy_color)) +
  theme_bw() + 
  theme(legend.position="none") +
  scale_colour_manual(values=c("#354995", "#5E5E5E")) +
  labs(x = "", y = "SWIID Prediction minus LIS") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank()) +
  # scale_y_continuous(breaks=c(-15, -10, -5, 0, 5)) + 
  scale_x_discrete(limits=levels(kfold_output$cy)) 

ggsave(file="../dcpo_redistribution/paper/warsaw_talk/kfold.pdf", width=8.5, height=5.25)
