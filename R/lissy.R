ccn <- 47

cc <- tolower(c("AU", "AT", "BE", "BR", "CA", "CL", "CN", "CO",
                "CZ", "DK", "DO", "EG", "EE", "FI", "FR", "DE",
                "GE", "GR", "GT", "HU", "IS", "IN", "IE", "IL",
                "IT", "JP", "LU", "MX", "NL", "NO", "PA", "PY",
                "PE", "PL", "RO", "RU", "RS", "SK", "SI", "ZA",
                "KR", "ES", "SE", "CH", "TW", "UK", "US", "UY"))
yy <- as.character(c(c(67, 69, 71, 73:75, 78:99), paste0("0", 1:9), c(11:14)))

datasets <- paste0(cc[ccn], yy, "h")

## Define functions
gini <- function(x, weight) {
  ox <- order(x)
  x <- as.vector(x)[ox]
  weight <- as.vector(weight)[ox] / sum(weight) 
  p <- cumsum(weight)
  nu <- cumsum(weight * x)
  n <- length(nu)
  nu <- nu / nu[n]
  res <- round((sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1]))*100, digits = 2)
  return(res)
}

wNtile <- function(var, wgt, split) {
  x <- var[order(var)]
  y <- wgt[order(var)]
  z <- cumsum(y) / sum(y)
  cop <- rep(NA, length(split)) 
  for (i in 1:length(cop)) {
    cop[i] <- x[Find(function(h) z[h] > split[i], seq_along(z))]
  }
  return(cop)
}

topBottom <- function(var, botline, topline) {
  tb               <- ifelse(var < botline, botline, var)
  tb[tb > topline] <- topline
  return(tb)
}

setups <- function(df) {
  botline <- 0
  topline <- 10 * wNtile(df$dhi, df$hpopwgt, 0.5)
  df$dhi <- topBottom(df$dhi, botline, topline)
  df$edhi <- df$dhi / (df$nhhmem ^ 0.5)
  df$cdhi <- df$dhi / df$nhhmem
  df$mi <- topBottom((df$factor + df$hitp), botline, topline)
  df$emi <- df$mi / (df$nhhmem ^ 0.5)
  df$cmi <- df$mi / df$nhhmem
  return(df)
}

boot_gini_se <- function(data, reps=1000) {
  resamples <- lapply(1:reps, function(i) dplyr::sample_n(data, size = nrow(data), replace=TRUE))
  r_stat <- lapply(resamples, function(x) gini(x[[var]], x$hpopwgt * x$nhhmem))
  std_err <- round(sqrt(var(unlist(r_stat))), digits = 2)
  return(std_err)   
}

# For testing at home:
# read.LIS <- function(data_file, labels, vars, subset) {
#   require(dplyr)
#   data_file <- str_replace(data_file, "h", "ih.dta")
#   df <- haven::read_dta(data_file)[, vars] %>% 
#     filter(eval(parse(text = subset), .)) 
#   if (!labels) {
#     df <- df %>% mutate_each(funs(as.numeric))
#   }
#   return(df)
# }


vars <- c("dhi", "factor", "hitp", "hpopwgt", "nhhmem", "grossnet")
subset <- "complete.cases(dhi, factor, hitp)"
for (ccyy in datasets) {
  df <- try(read.LIS(ccyy, labels = FALSE, vars = vars, subset = subset), silent = TRUE)
  if (class(df)[1] != "try-error") {
    if (!is.nan(mean(df$dhi)) & mean(df$dhi)!= 0) {
      df <- setups(df)
      var <- c("emi", "edhi", "cmi", "cdhi", "mi", "dhi")
      for (var in c("emi", "edhi", "cmi", "cdhi", "mi", "dhi")) {
        if (var == "mi" | var == "dhi") {
          wt <- df$hpopwgt
        } else {
          wt <- df$hpopwgt * df$nhhmem
        }
        cat(paste(ccyy, 
                    var, 
                    gini(df[[var]], wt),
                    boot_gini_se(df),
                    df$grossnet[1],
                  
                    sep = ","), sep = "\n")
    
      }
    }
  }
}

