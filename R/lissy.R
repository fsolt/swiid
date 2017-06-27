* user = lis_user
* password = lis_password
* package = R
* project = LIS

## Define functions
gini <- function(df, x) {
  df1 <- df[!is.na(df[[x]]), ]
  x <- as.vector(df1[[x]])
  weight <- df1$wt
  
  ox <- order(x)
  x <- as.vector(x)[ox]
  weight <- as.vector(weight)[ox] / sum(weight) 
  p <- cumsum(weight)
  nu <- cumsum(weight * x)
  n <- length(nu)
  nu <- nu / nu[n]
  res <- round((sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])), digits = 4)
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
  tb <- ifelse(var < botline, botline, var)
  tb <- ifelse(var > topline, topline, var)
  return(tb)
}

setups <- function(df) {
  botline <- 0
  topline <- 10 * wNtile(df$dhi, df$hpopwgt, 0.5)
  df$disp_hh <- topBottom(df$dhi, botline, topline)
  df$disp_sqrt <- df$disp_hh / (df$nhhmem ^ 0.5)
  df$disp_pc <- df$disp_hh / df$nhhmem
  df$market_hh <- topBottom(ifelse(!is.na(df$hitp), (df$factor + df$hitp), df$factor), botline, topline)
  df$market_sqrt <- df$market_hh / (df$nhhmem ^ 0.5)
  df$market_pc <- df$market_hh / df$nhhmem
  df$con_hh <- topBottom(df$hc, botline, topline)
  df$con_sqrt <- df$con_hh / (df$nhhmem ^ 0.5)
  df$con_pc <- df$con_hh / df$nhhmem  
  return(df)
}

boot_gini_se <- function(data, var, reps=100) {
  data <- data[!is.na(df[[var]]), ]
  resamples <- lapply(1:reps, function(i) dplyr::sample_n(data, size = nrow(data), replace=TRUE))
  r_stat <- lapply(resamples, function(x) gini(x, var))
  std_err <- round(sqrt(var(unlist(r_stat))), digits = 4)
  return(std_err)   
}

# For testing at home:
# read.LIS <- function(data_file, labels, vars, subset) {
#   require(dplyr)
#   data_file <- stringr::str_replace(data_file, "h", "ih.dta")
#   df <- haven::read_dta(data_file)[, vars] %>%
#     filter(eval(parse(text = subset), .))
#   if (!labels) {
#     df <- df %>% dplyr::mutate_all(funs(as.numeric))
#   }
#   return(df)
# }

get_ginis <- function(cc, reps = 100) {
  ccs <- c("au", "at", "be", "br", "ca", "cl", "cn", "co", "cz", "dk", 
           "do", "eg", "ee", "fi", "fr", "de", "ge", "gr", "gt", "hu", "is", 
           "in", "ie", "il", "it", "jp", "lu", "mx", "nl", "no", "pa", "py", 
           "pe", "pl", "ro", "ru", "rs", "sk", "si", "za", "kr", "es", "se", 
           "ch", "tw", "uk", "us", "uy")
  cc <- tolower(cc)
  if (!cc %in% ccs) {
    stop("Please specify a LIS country in iso2c format")
  }
  yy <- as.character(c(c(67, 69, 71, 73:75, 78:99), paste0("0", 1:9), c(11:17)))
  
  datasets <- paste0(rep(cc, each = length(yy)), rep(yy, times = length(cc)), "h")
  vars <- c("dhi", "factor", "hitp", "hc", "hpopwgt", "nhhmem", "grossnet")
  subset <- "complete.cases(dhi)"
  
  v <- c("market_hh", "market_sqrt", "market_pc",
        "disp_hh", "disp_sqrt", "disp_pc",
        "con_hh", "con_sqrt", "con_pc")
  
  for (ccyy in datasets) {
    cat("")
    df <- try(read.LIS(ccyy, labels = FALSE, vars = vars, subset = subset), silent = TRUE)
    if (!class(df)[1] == "try-error") {
      if (!is.nan(mean(df$dhi)) & !mean(df$dhi) == 0) {
        df <- setups(df)
        for (var in v) {
          if (grepl("hh", var)) {
            df$wt <- df$hpopwgt
          } else {
            df$wt <- df$hpopwgt * df$nhhmem
          }
          if (!is.na(mean(df[[var]]))) {
            cat(paste(ccyy, 
                      var, 
                      gini(df, var),
                      boot_gini_se(df, var, reps = reps),
                      df$grossnet[1],
                      sep = ","), sep = "\n")
          }
        }
      }
    }
  }
}

# Call
get_ginis("CCODE")