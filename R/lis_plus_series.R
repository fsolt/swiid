library(tidyverse)
library(stringr)
library(rstan)
library(beepr)

seed <- 324
iter <- 100
chains <- 4
cores <- chains

# Get data for all series that overlap country-years with LIS data
swiid_source <- read_csv("data/swiid_source.csv")   # get all data

baseline_series <- "LIS disp sqrt"
baseline <- swiid_source %>%                        # get the LIS data for selected baseline
  filter(series == baseline_series) %>% 
  rename(gini_b = gini,
         gini_b_se = gini_se) %>% 
  group_by(country) %>% 
  mutate(bl_count = n()) %>% 
  ungroup() %>% 
  arrange(desc(bl_count))

ineq_bl_series <- swiid_source %>%                  # identify series with some baseline data
  right_join(baseline %>% 
               select(country, year), 
             by = c("country", "year")) %>% 
  pull(series) %>%
  unique() 

x <- swiid_source %>%                               # get data in series with some baseline data
  left_join(baseline %>% 
              select(country, year, gini_b, gini_b_se, bl_count),
            by = c("country", "year")) %>% 
  filter(series %in% ineq_bl_series) %>% 
  group_by(series) %>% 
  mutate(n_all = n(),
         n_bl = sum(!is.na(gini_b))) %>% 
  ungroup() %>%
  filter(!n_all == n_bl) %>%   # exclude series with *only* country-years with baseline data
  rename(gini_m = gini,
         gini_m_se = gini_se) %>% 
  mutate(gini_m_se = ifelse(!is.na(gini_m_se), gini_m_se,
                            quantile(gini_m_se/gini_m, .99, na.rm = TRUE)*gini_m),
         kcode = as.integer(factor(country, levels = unique(country))), # redo codes for filtered sample
         tcode = as.integer(year - min(year) + 1),
         scode = as.integer(factor(series, levels = unique(series)))) %>% 
  group_by(country) %>% 
  mutate(bl_count = mean(bl_count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(-bl_count)


# Format data for Stan
source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      S = max(x$scode),
                      N = length(x$gini_m),
                      N_b = length(x$gini_b[!is.na(x$gini_b)]),
                      kk = x$kcode,
                      tt = x$tcode,
                      kktt = (x$kcode-1)*max(x$tcode)+x$tcode,
                      ss = x$scode,
                      ktt = rep(1:max(x$tcode), times = max(x$kcode)),
                      ktk = rep(1:max(x$kcode), each = max(x$tcode)),
                      gini_m = x$gini_m,
                      gini_m_se = x$gini_m_se,
                      gini_b = x$gini_b[!is.na(x$gini_b)],
                      gini_b_se = x$gini_b_se[!is.na(x$gini_b_se)]
)


# Stan
start <- proc.time()
out1 <- stan(file = "R/lis_plus_series.stan",
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

save(out1, file = str_c("data/lis_plus_series_", str_replace(Sys.time(), " ", "_"), ".rda"))

beep() # chime


# Post-processing
plot_tscs <- function(input, output, pars="gini", probs=c(.025, .975),
                      dims, year_bounds, y_label, save_pdf = NA) {
  
  kcodes <- input %>%
    group_by(country) %>%
    summarize(kcode = first(kcode),
              firstyr = min(year),
              lastyr = max(year)) %>%
    ungroup()
  
  ktcodes <- tibble(kcode = rep(1:max(x$kcode), each = max(x$tcode)),
                    tcode = rep(1:max(x$tcode), times = max(x$kcode)),
                    ktcode = (kcode-1)*max(tcode)+tcode) %>%
    left_join(kcodes, by = "kcode") %>%
    mutate(year = min(firstyr) + tcode - 1)  
  
  lb <- paste0("x", str_replace(probs*100, "\\.", "_"), "percent")[1]
  ub <- paste0("x", str_replace(probs*100, "\\.", "_"), "percent")[2]
  
  gini_res <- rstan::summary(output, pars=pars, probs=probs) %>%
    first() %>%
    as.data.frame() %>%
    rownames_to_column("parameter") %>%
    as_tibble() %>%
    janitor::clean_names() %>% 
    mutate(estimate = mean,
           lb = get(paste0("x", str_replace(probs*100, "\\.", "_"), "percent")[1]),
           ub = get(paste0("x", str_replace(probs*100, "\\.", "_"), "percent")[2]),
           ktcode = as.numeric(str_extract(parameter, "(?<=\\[)\\d+"))) %>%
    left_join(ktcodes, by=c("ktcode")) %>%
    arrange(kcode, tcode)
  
  if (missing(dims)) {
    if (!missing(save_pdf)) {
      dims <- c(7, 5)
    } else {
      dims <- c(ceiling(max(input$kcode)/5), 5)
    }
  }
  
  rows <- dims[1]
  cols <- dims[2]
  
  if (missing(year_bounds)) {
    year_bounds <- c(1960, ceiling(as.integer(format(Sys.Date(), "%Y"))/10)*10)
  }
  
  if (missing(y_label)) y_label <- "Gini Coefficient"
  
  npages <- ceiling(max(gini_res$kcode)/(rows*cols))
  pages <- paste(((0:(npages - 1) * rows * cols) + 1), (1:npages * rows * cols), sep = ":")
  
  for (i in 1:npages) {
    cpage <- unique(gini_res$country)[((i-1)*rows*cols)+1:i*rows*cols]
    cpage <- unique(gini_res$country)[c(eval(parse(text=pages[i])))]
    cp <- gini_res[gini_res$country %in% cpage, ]
    cp$country <- factor(cp$country, levels = cpage)
    
    plotx <- ggplot(data=cp, aes(x=year, y=estimate)) +
      geom_line() + theme_bw() +
      theme(legend.position="none") +
      coord_cartesian(xlim=year_bounds) +
      labs(x = NULL, y = y_label) +
      geom_ribbon(aes(ymin = lb, ymax = ub, linetype=NA), alpha = .25) +
      facet_wrap(~country, ncol = 5) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(fill = "white", colour = "white"))
    
    if (i==1) plotx1 <- plotx
    
    if (!is.na(save_pdf)) {
      pdf(file=str_replace(save_pdf, "\\.", paste0(i, ".")), width=6, height = 9)
      plot(plotx)
      graphics.off()
    }
  }
  
  return(plotx1)
}

plot_tscs(x, out1, save_pdf = "paper/figures/ts_lps.pdf")
plot_tscs(x, out1)
