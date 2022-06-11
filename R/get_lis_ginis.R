library(tidyverse)
library(mailR)

ccs <- c("au", "at", "be", "br", "ca", "ci", "cl", "cn", "co", "cz", "dk", 
         "do", "eg", "ee", "fi", "fr", "de", "ge", "gr", "gt", "hu", "is", 
         "in", "ie", "il", "it", "jp", "lt", "lu", "mx", "nl", "no", "pa", "py", 
         "pe", "pl", "ps", "ro", "ru", "rs", "sk", "si", "za", "kr", "es", "se", 
         "ch", "tw", "uk", "us", "uy", "vn") # la

get_lis_ginis <- function(ccs, wait = 240) {
  yahoo_user <- getOption("yahoo_user")        # set all of these in .Rprofile, e.g.:
  yahoo_password <-getOption("yahoo_password") # options("yahoo_user" = "juanita-herrera@yahoo.com", 
  lis_user <- getOption("lis_user")            #         "yahoo_password" = "app_password")
  lis_password <- getOption("lis_password")    # note you'll have to set up an app password for R in
  purrr::walk(ccs, function(cc) {              # your Yahoo account and use *that*, not your account password
    lissy <- readLines("R/lissy.R") %>% 
      paste(collapse = "\n") %>% 
      str_replace("lis_user", lis_user) %>% 
      str_replace("lis_password", lis_password) %>% 
      str_replace_all("CCODE", cc)
    
    send.mail(from = yahoo_user,
              to = "postbox@lisdatacenter.org",
              subject = cc,
              body = lissy, 
              authenticate = TRUE,
              smtp = list(host.name = "smtp.mail.yahoo.com",
                          port = 587,
                          user.name = yahoo_user,
                          passwd = yahoo_password,
                          tls = TRUE))
    
    if (length(ccs) > 1) Sys.sleep(wait)
  })
} 

get_lis_ginis(ccs)
