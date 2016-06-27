#Postprocessing
#save(out1, file = "data/output.rda")
out <- out1

x1 <- summary(out)
#write.table(as.data.frame(x1$summary), file="x1.csv", sep = ",")
x1_sum <- as.data.frame(x1$summary)
x1_sum$parameter <- rownames(x1_sum)
# x1_sum <- read_csv("x1.csv")
# names(x1_sum)[2] <- "mean"
x1_sum$parameter_type <- gsub("([^[]*).*", "\\1", x1_sum$parameter)
# View(x1_sum)
# View(x1_sum[x1_sum$Rhat>1.1,])
# View(x1_sum[x1_sum$Rhat>1.2,])

scodes <- x %>% group_by(series) %>%
  summarize(scode = first(scode),
            r_n = n()) %>%
  arrange(scode)

rho_res <- x1_sum %>% filter(parameter_type=="rho") %>% select(parameter, mean, `2.5%`, `97.5%`, Rhat)  %>% mutate(scode = as.numeric(str_extract(parameter, "\\d+"))) %>% left_join(scodes, by="scode")

sigma_series <- x1_sum %>% filter(parameter_type=="sigma_series") %>% select(parameter, mean, `2.5%`, `97.5%`, Rhat)  %>% mutate(scode = as.numeric(str_extract(parameter, "\\d+"))) %>% left_join(scodes, by="scode")


# b_res <- x1_sum %>% filter(parameter_type=="beta") %>% select(parameter, mean, `2.5%`, `97.5%`)  %>% mutate(mcode = as.numeric(str_extract(parameter, "\\d+"))) %>% left_join(mcodes, by="mcode")
# g_res <- x1_sum %>% filter(parameter_type=="gamma") %>% select(parameter, mean, `2.5%`, `97.5%`)  %>% mutate(mcode = as.numeric(str_extract(parameter, "\\d+"))) %>% left_join(mcodes, by="mcode")
# s_dk_res <- x1_sum %>% filter(parameter_type=="sd_k") %>% select(parameter, mean, `2.5%`, `97.5%`)  %>% mutate(rcode = as.numeric(str_extract(parameter, "\\d+"))) %>% left_join(rcodes)

gini_res <- summary(out, pars="gini", probs=c(.025, .975))
gini_res <- as.data.frame(gini_res$summary)

gini_res$ccode <- as.numeric(gsub("gini\\[([0-9]*),[0-9]*\\]", "\\1", row.names(gini_res)))
gini_res$tcode <- as.numeric(gsub("gini\\[[0-9]*,([0-9]*)\\]", "\\1", row.names(gini_res)))

c <- x %>% group_by(country) %>% summarize(
  ccode = first(ccode),
  firstyr = min(year),
  lastyr = max(year)) %>%
  ungroup()


gini_res2 <- merge(gini_res, c, all=T) %>% 
  mutate(year = min(firstyr) + tcode - 1) %>% 
  filter(year <= lastyr & year >= firstyr) %>%
  transmute(country = country,
            term = country,
            cc = ccode,
            year = year,
            estimate = mean*100,
            lb = `2.5%`*100,
            ub = `97.5%`*100,
            Rhat = Rhat) %>%
  arrange(cc, year)

ggplot(data=gini_res2, aes(x=year, y=estimate)) +
  geom_line() + theme_bw() +
  theme(legend.position="none") +
  coord_cartesian(xlim=c(1960, 2016)) +
  labs(x = NULL, y = "SWIID Gini") +
  geom_ribbon(aes(ymin = lb, ymax = ub, linetype=NA), alpha = .25) +
  facet_wrap(~country, ncol = 5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "white", colour = "white"))


# count_divergences <- function(fit) {
#   sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
#   sum(sapply(sampler_params, function(x) c(x[,'n_divergent__']))[,1])
# }

# Plots:
#   1. tolerance by country, most recent available year: cs_plot
#   2. tolerance trends, estimate plus raw data, eight countries
#   3. trends in all countries: ts_plot
#   4. probability of tolerant answer by tolerance (beta and gamma), selected items (modelled on McGann2014, fig 1)
#   5. bar chart of beta and gamma for all items?
