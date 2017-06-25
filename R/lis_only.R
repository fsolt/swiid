library(rstan)
library(beepr)

seed <- 324
iter <- 1000
chains <- 4
cores <- chains

x <- ineq %>% 
  filter(series == "LIS disp sqrt") %>% 
  mutate(kcode = as.integer(factor(country, levels = unique(country))), # redo codes for filtered sample
         tcode = as.integer(year - min(year) + 1)) 

source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      N = length(x$gini_m),
                      N_b = length(x$gini_b[!is.na(x$gini_b)]),
                      kk = x$kcode,
                      tt = x$tcode,
                      gini_m = x$gini_m,
                      gini_m_se = x$gini_m_se,
                      gini_b = x$gini_b[!is.na(x$gini_b)],
                      gini_b_se = x$gini_b_se[!is.na(x$gini_b_se)]
)

start <- proc.time()
out1 <- stan(file = "R/lis_only.stan",
             data = source_data,
             seed = seed,
             iter = iter,
             cores = cores,
             chains = chains,
             control = list(max_treedepth = 20,
                            adapt_delta = .8))
runtime <- proc.time() - start
runtime

lapply(get_sampler_params(out1, inc_warmup = FALSE),
       summary, digits = 2)

# Chime
beep()


# Post-processing
kcodes <- x %>%
  group_by(country) %>%
  summarize(kcode = first(kcode),
            firstyr = min(year),
            lastyr = max(year),
            first_gini_b = first(gini_b)) %>%
  ungroup()

ktcodes <- tibble(kcode = rep(1:max(x$kcode), each = max(x$tcode)),
                  tcode = rep(1:max(x$tcode), times = max(x$kcode))) %>%
  left_join(kcodes, by = "kcode") %>%
  mutate(year = min(firstyr) + tcode - 1)

gini_res <- rstan::summary(out1, pars="gini", probs=c(.025, .975)) %>%
  first() %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  as_tibble() %>%
  rename(estimate = mean, lb = `2.5%`, ub = `97.5%`) %>% 
  mutate(kcode = as.numeric(str_extract(parameter, "(?<=\\[)\\d+")),
         tcode = as.numeric(str_extract(parameter, "(?<=,)\\d+"))) %>%
  left_join(ktcodes, by=c("kcode", "tcode")) %>%
  arrange(kcode, tcode)

plot_results <- function(stan_m){
  if(class(stan_m) != "stanfit"){
    stop("stan_m must be an object of class stanfit, with parameters mu representing latent vote at a point in time")
  }
  ex <- as.data.frame(rstan::extract(stan_m, "mu"))
  names(ex) <- 1:d1$n_days
  
  p <- ex %>%
    gather(day, value) %>%
    mutate(day = as.numeric(day),
           day = as.Date(day, origin = "2004-10-08")) %>%
    group_by(day) %>%
    summarise(mean = mean(value),
              upper = quantile(value, 0.975),
              lower = quantile(value, 0.025)) %>%
    ggplot(aes(x = day)) +
    labs(x = "Shaded region shows a pointwise 95% credible interval.", 
         y = "Voting intention for the ALP (%)",
         caption = "Source: Jackman's pscl R package; analysis at https://ellisp.github.io") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    geom_line(aes(y = mean)) +
    scale_y_continuous(breaks = 31:54, sec.axis = dup_axis(name = "")) +
    theme(panel.grid.minor = element_blank())
  
  return(p)
}

#ts_plot <- function(df, rows = 7, cols = 5, npages) {
rows <- 7
cols <- 5
npages <- max(x$kcode)/35
#a_res <- df

pages <- c("1:35", "36:70")

for (i in 1:npages) {
  cpage <- unique(gini_res$country)[((i-1)*rows*cols)+1:i*rows*cols]
  cpage <- unique(gini_res$country)[c(eval(parse(text=pages[i])))]
  cp <- gini_res[gini_res$country %in% cpage, ]
  cp$country <- factor(cp$country, levels = cpage)
  
  plotx <- ggplot(data=cp, aes(x=year, y=estimate)) +
    geom_line() + theme_bw() +
    theme(legend.position="none") +
    coord_cartesian(xlim=c(1980,2016)) +
    labs(x = NULL, y = "Gini Coefficient") +
    geom_ribbon(aes(ymin = lb, ymax = ub, linetype=NA), alpha = .25) +
    facet_wrap(~country, ncol = 5) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.background = element_rect(fill = "white", colour = "white"))
  
  pdf(file=paste0("paper/figures/ts",i,".pdf"), width=6, height = 9)
  plot(plotx)
  graphics.off()
}
