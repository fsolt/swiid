library(tidyverse)

swiid_summary_latest <- read_csv("data/swiid_summary.csv", col_types = "cddddddddd") %>% 
  mutate(decade = floor(year/10) * 10)
swiid_summary81 <- "https://github.com/fsolt/swiid/raw/0c7ea915a5d56440d894a2ec8e4f569650643ba0/data/swiid_summary.csv" %>%
  read_csv(col_types = "cddddddddd") %>% 
  mutate(decade = floor(year/10) * 10)
swiid_summary80 <- "https://github.com/fsolt/swiid/raw/8c5d972f19a68a4c432d75491f65dc5172901222/data/swiid_summary.csv" %>%
  read_csv(col_types = "cddddddddd") %>% 
  mutate(decade = floor(year/10) * 10)
swiid_summary71 <- "https://github.com/fsolt/swiid/raw/master/data/swiid7_1_summary.csv" %>%
  read_csv(col_types = "cddddddddd") %>% 
  mutate(decade = floor(year/10) * 10)
swiid_summary70 <- "https://github.com/fsolt/swiid/raw/master/data/swiid7_0_summary.csv" %>%
  read_csv(col_types = "cddddddddd") %>% 
  mutate(decade = floor(year/10) * 10)

# Merge data, calculate within country-year differences across versions
swiid_latest_81_71_70 <- swiid_summary_latest %>% 
  select(country, year, decade, gini_disp) %>% 
  rename(gini_latest = gini_disp) %>% 
  left_join(swiid_summary81 %>%
              select(country, year, decade, gini_disp) %>% 
              rename(gini81 = gini_disp),
            by = c("country", "year", "decade")) %>% 
  left_join(swiid_summary71 %>%
              select(country, year, decade, gini_disp) %>% 
              rename(gini71 = gini_disp),
            by = c("country", "year", "decade")) %>% 
  left_join(swiid_summary70 %>% 
              select(country, year, decade, gini_disp) %>% 
              rename(gini70 = gini_disp),
            by = c("country", "year", "decade")) %>% 
  mutate(vlatest_minus81 = gini_latest - gini81,
         v81minus71 = gini81-gini71,
         v71minus70 = gini71-gini70,
         v81minus70 = gini81-gini70)

# Mean within country-year differences by decade, all countries
swiid_latest_81_71_70 %>% 
  group_by(decade) %>%
  summarize_at(vars(starts_with("v")), mean, na.rm = TRUE)

# Mark's list of advanced countries only
swiid_latest_81_71_70 %>%
  filter(country %in% c("Australia", "Austria", "Belgium", "Canada", "Czech Republic",
                        "Denmark", "Finland", "France", "Germany", "Greece", "Hong Kong",
                        "Ireland", "Israel", "Italy", "Japan", "Netherlands", "New Zealand",
                        "Norway", "Portugal", "Singapore", "South Korea", "Spain", "Sweden",
                        "Switzerland", "United Kingdom", "United States" )) %>% 
  group_by(decade) %>%
  summarize_at(vars(starts_with("v")), mean, na.rm = TRUE) 

# Merge data, calculate within country-year differences across versions
swiid_latest_81_71_70_mkt <- swiid_summary_latest %>% 
  mutate(decade = floor(year/10) * 10) %>% 
  select(country, year, decade, gini_mkt) %>% 
  rename(gini_latest = gini_mkt) %>% 
  left_join(swiid_summary81 %>%
              select(country, year, decade, gini_mkt) %>% 
              rename(gini81 = gini_mkt),
            by = c("country", "year", "decade")) %>% 
  left_join(swiid_summary71 %>%
              select(country, year, decade, gini_mkt) %>% 
              rename(gini71 = gini_mkt),
            by = c("country", "year", "decade")) %>% 
  left_join(swiid_summary70 %>% 
              select(country, year, decade, gini_mkt) %>% 
              rename(gini70 = gini_mkt),
            by = c("country", "year", "decade")) %>% 
  mutate(vlatest_minus81 = gini_latest-gini81,
         v81minus71 = gini81-gini71,
         v71minus70 = gini71-gini70,
         v81minus70 = gini81-gini70)

# Mean within country-year differences by decade, Mark's advanced countries
swiid_latest_81_71_70_mkt %>%
  group_by(decade) %>%
  summarize_at(vars(starts_with("v")), mean, na.rm = TRUE) 

# Mean within country-year differences by decade, Mark's advanced countries
swiid_latest_81_71_70_mkt %>%
  filter(country %in% c("Australia", "Austria", "Belgium", "Canada", "Czech Republic",
                        "Denmark", "Finland", "France", "Germany", "Greece", "Hong Kong",
                        "Ireland", "Israel", "Italy", "Japan", "Netherlands", "New Zealand",
                        "Norway", "Portugal", "Singapore", "South Korea", "Spain", "Sweden",
                        "Switzerland", "United Kingdom", "United States" )) %>% 
  group_by(decade) %>%
  summarize_at(vars(starts_with("v")), mean, na.rm = TRUE) 

swiid_source_latest <- read_csv("data/swiid_source.csv", col_types = "cdddcclcccc")
swiid_source81 <- read_csv("https://github.com/fsolt/swiid/raw/b85cf28f34897781098fe3b992da8434bf758993/data/swiid_source.csv", col_types = "cdddcclcccc")
swiid_source80 <- read_csv("https://github.com/fsolt/swiid/raw/19349a99255f749b2c4634f748def3950e3e8ac4/data/swiid_source.csv", col_types = "cdddcclcccc")
lis_mkt <- swiid_source_latest %>% 
  filter(str_detect(source1, "LIS") & welfare_def == "market" & equiv_scale == "sqrt")
lis_disp <- swiid_source_latest %>% 
  filter(str_detect(source1, "LIS") & welfare_def == "disp" & equiv_scale == "sqrt")


swiid_latest_81_71_70_mkt %>%
  filter(country %in% c("Australia", "Austria", "Belgium", "Canada", "Czech Republic",
                        "Denmark", "Finland", "France", "Germany", "Greece", "Hong Kong",
                        "Ireland", "Israel", "Italy", "Japan", "Netherlands", "New Zealand",
                        "Norway", "Portugal", "Singapore", "South Korea", "Spain", "Sweden",
                        "Switzerland", "United Kingdom", "United States" )) %>% 
  left_join(lis_mkt %>% select(country, year, gini), by = c("country", "year")) %>% 
  group_by(country, decade) %>% 
  summarise(mean_diff = mean(vlatest_minus81, na.rm = TRUE),
            lis_obs = as_factor(sum(!is.na(gini)))) %>% 
  mutate(country_decade = paste(country, decade, sep = "_")) %>% 
  filter(abs(mean_diff) > .5 & !country_decade == "France_1960") %>% # no observations in v7.1
  ggplot(aes(x = forcats::fct_reorder(country_decade, mean_diff),
             y = mean_diff, 
             fill = lis_obs %>% 
               fct_relevel(as.character(c(0:6, 10))))) +
  geom_col(color = "#E6E6FF") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 8),
        legend.position=c(.99,.01), legend.justification=c(1, 0)) +
  ylab(NULL) + 
  xlab(NULL) +
  scale_fill_brewer(name = "LIS Observations",
                    palette = "Blues",
                    guide = guide_legend(ncol = 2)) +
  ggtitle("Latest Version Minus Version 8.1, By Country-Decade")


swiid_latest_81_71_70 %>%
  left_join(lis_disp %>% 
              select(country, year, gini), by = c("country", "year")) %>% 
  group_by(country, decade) %>% 
  summarise(mean_diff = mean(vlatest_minus81, na.rm = TRUE),
            lis_obs = as_factor(sum(!is.na(gini)))) %>% 
  mutate(country_decade = paste(country, decade, sep = "_")) %>% 
  filter(abs(mean_diff) > 4 & !country_decade == "France_1960") %>% # no observations in v7.1
  ggplot(aes(x = forcats::fct_reorder(country_decade, mean_diff),
             y = mean_diff, 
             fill = lis_obs %>% 
               fct_relevel(as.character(c(0:6, 10))))) +
  geom_col(color = "#E6E6FF") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 8),
        legend.position=c(.99,.01), legend.justification=c(1, 0)) +
  ylab(NULL) + 
  xlab(NULL) +
  scale_fill_brewer(name = "LIS Observations",
                    palette = "Blues",
                    guide = guide_legend(ncol = 2)) +
  ggtitle("Latest Version Minus Version 8.1, By Country-Decade")


swiid_latest_81_71_70_mkt %>%
  filter(country %in% c("Australia", "Austria", "Belgium", "Canada", "Czech Republic",
                        "Denmark", "Finland", "France", "Germany", "Greece", "Hong Kong",
                        "Ireland", "Israel", "Italy", "Japan", "Netherlands", "New Zealand",
                        "Norway", "Portugal", "Singapore", "South Korea", "Spain", "Sweden",
                        "Switzerland", "United Kingdom", "United States" )) %>% 
  left_join(lis_mkt %>% select(country, year, gini), by = c("country", "year")) %>% 
  group_by(country, decade) %>% 
  summarise(mean_diff = mean(vlatest_minus81, na.rm = TRUE),
            lis_obs = as_factor(sum(!is.na(gini)))) %>% 
  mutate(country_decade = paste(country, decade, sep = "_")) %>% 
  filter(abs(mean_diff) > .5 & !country_decade == "France_1960") %>% # no observations in v7.1
  ggplot(aes(x = forcats::fct_reorder(country_decade, mean_diff),
             y = mean_diff, 
             fill = lis_obs %>% 
               fct_relevel(as.character(c(0:6, 10))))) +
  geom_col(color = "#E6E6FF") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 8),
        legend.position=c(.99,.01), legend.justification=c(1, 0)) +
  ylab(NULL) + 
  xlab(NULL) +
  scale_fill_brewer(name = "LIS Observations",
                    palette = "Blues",
                    guide = guide_legend(ncol = 2)) +
  ggtitle("Latest Version Minus Version 8.1, By Country-Decade")


swiid_latest_81_71_70 %>%
  left_join(lis_disp %>% 
              select(country, year, gini), by = c("country", "year")) %>% 
  group_by(country, decade) %>% 
  summarise(mean_diff = mean(vlatest_minus81, na.rm = TRUE),
            lis_obs = as_factor(sum(!is.na(gini)))) %>% 
  mutate(country_decade = paste(country, decade, sep = "_")) %>% 
  filter(abs(mean_diff) %>% between(.5, 4) & !country_decade == "France_1960") %>% # no observations in v7.1
  ggplot(aes(x = forcats::fct_reorder(country_decade, mean_diff),
             y = mean_diff, 
             fill = lis_obs %>% 
               fct_relevel(as.character(c(0:6, 10))))) +
  geom_col(color = "#E6E6FF") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 8),
        legend.position=c(.99,.01), legend.justification=c(1, 0)) +
  ylab(NULL) + 
  xlab(NULL) +
  scale_fill_brewer(name = "LIS Observations",
                    palette = "Blues",
                    guide = guide_legend(ncol = 2)) +
  ggtitle("Latest Version Minus Version 8.1, By Country-Decade")


swiid_source71 <- "https://github.com/fsolt/swiid/raw/e86defc56d2e870c5d091a16e3a5036906b34177/data/swiid_source.csv" %>% 
  read_csv(col_types = "cdddcclcccc")

check_source <- function(cc) {
  country_source_latest <- swiid_source_latest %>% 
    filter(country == cc)
  country_source81 <- swiid_source81 %>% 
    filter(country == cc)
  
  anti_join(country_source_latest, country_source81)
}

check_source("Austria") %>% View()
