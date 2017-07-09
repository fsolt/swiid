plot_tscs <- function(input, output, kt = FALSE, pars="gini", probs=c(.025, .975),
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
  
  if (!kt) {
    gini_res <- rstan::summary(output, pars=pars, probs=probs) %>%
      first() %>%
      as.data.frame() %>%
      rownames_to_column("parameter") %>%
      as_tibble() %>%
      janitor::clean_names() %>% 
      mutate(estimate = mean,
             lb = get(paste0("x", str_replace(probs*100, "\\.", "_"), "percent")[1]),
             ub = get(paste0("x", str_replace(probs*100, "\\.", "_"), "percent")[2]),
             kcode = as.numeric(str_extract(parameter, "(?<=\\[)\\d+")),
             tcode = as.numeric(str_extract(parameter, "(?<=,)\\d+"))) %>%
      left_join(ktcodes, by=c("kcode", "tcode")) %>%
      arrange(kcode, tcode)
  } else {
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
      left_join(ktcodes, by="ktcode")
    arrange(kcode, tcode)
  }
  
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
      facet_wrap(~country, ncol = cols) +
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
