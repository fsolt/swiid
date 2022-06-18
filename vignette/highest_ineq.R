library(tidyverse)

load("../data/swiid_summary.rda")

indonesia_gini <- swiid_summary %>% 
    group_by(country) %>% 
    filter(country == "Indonesia" & year == max(year)) %>% 
    pull(gini_disp)

indonesia_gini_se <- swiid_summary %>% 
    group_by(country) %>% 
    filter(country == "Indonesia" & year == max(year)) %>% 
    pull(gini_disp_se)

swiid_last <- swiid_summary %>%
    group_by(country) %>%
    filter(year == max(year)) %>%
    filter(year>=2010) %>%
    arrange(desc(gini_disp)) %>% 
    mutate(indonesia = (country == "Indonesia"),
           diff_indonesia = gini_disp - indonesia_gini %>% abs(),
           diff_indonesia_se = (gini_disp_se^2 + indonesia_gini_se^2)^.5,
           indistinguishable = abs(diff_indonesia) < 2*diff_indonesia_se,
           clr = if_else(indonesia, "red", if_else(indistinguishable, "gray50", "black")),
           country = paste0(country, " (", year, ")"),
           country = forcats::fct_reorder(country, -gini_disp))



ggplot(swiid_last) +
    geom_pointrange(aes(x = forcats::fct_reorder(country, -gini_disp), 
                        y = gini_disp, 
                        ymin = gini_disp - 2*gini_disp_se, 
                        ymax = gini_disp + 2*gini_disp_se, 
                        colour = clr)) +
    theme_bw() + theme(legend.position="none") + 
    scale_colour_manual(values=c("black", "gray50", "red")) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size=7, colour = swiid_last$clr)) + 
    labs(x = NULL, y = "Gini Index of Disposable Income Inequality, SWIID v9.0, Most Recent Available Year") +
    ggtitle("Indonesia is Very, Very Unequal, But It Is Not Among the World's Most Unequal Countries") +
    annotate("text", x = 27, y = 30, size=4, label="Indonesia's inequality is indistinguishable\n from that of the countries \n in light gray due to uncertainty")
ggsave("vignette/highest_ineq.pdf", width=14, height=8)
