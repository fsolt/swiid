library(tidyverse)
library(mailR)

ccs <- c("us", "co", "uy", "uk", # bigger/slower than avg
        "au", "at", "be", "br", "ca", "ci", "cl", "cn", "cz", "dk", 
         "do", "ee", "fi", "fr", "de", "ge", "gr", "gt", "hu", "is", "in",
         "ie", "il", "it", "jp", "lt", "lu", "ml", "mx", "nl", "no", "pa", "py", 
         "pe", "pl", "ps", "ro", "ru", "rs", "sk", "si", "za", "kr", "es", "se", 
         "ch", "tw", "vn") # la lv here and lissy.R when released

get_lis_ginis <- function(ccs, wait = 120) {
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

ccs_erflis <- c("eg", "iq", "jo", "ps", "so", "sd", "tn")

get_erflis_ginis <- function(ccs, wait = 120) {
    yahoo_user <- getOption("yahoo_user")        # set all of these in .Rprofile, e.g.:
    yahoo_password <-getOption("yahoo_password") # options("yahoo_user" = "juanita-herrera@yahoo.com", 
    lis_user <- getOption("lis_user")            #         "yahoo_password" = "app_password")
    lis_password <- getOption("lis_password")    # note you'll have to set up an app password for R in
    purrr::walk(ccs, function(cc) {              # your Yahoo account and use *that*, not your account password
        lissy <- readLines("R/lissy_erflis.R") %>% 
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

get_erflis_ginis(ccs_erflis)
