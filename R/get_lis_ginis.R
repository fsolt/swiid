library(tidyverse)
library(stringr)
library(mailR)

ccs <- c("au", "at", "be", "br", "ca", "cl", "cn", "co", "cz", "dk", 
         "do", "eg", "ee", "fi", "fr", "de", "ge", "gr", "gt", "hu", "is", 
         "in", "ie", "il", "it", "jp", "lu", "mx", "nl", "no", "pa", "py", 
         "pe", "pl", "ro", "ru", "rs", "sk", "si", "za", "kr", "es", "se", 
         "ch", "tw", "uk", "us", "uy")

get_lis_ginis <- function(ccs, wait = 900) {
  gmail_user <- getOption("gmail_user")         # set all of these in .Rprofile, e.g.: 
  gmail_password <- getOption("gmail_password") # options("gmail_user" = "juanita_herrara@gmail.com", 
  lis_user <- getOption("lis_user")             #         "gmail_password" = "my_password")
  lis_password <- getOption("lis_password")
  
  purrr::walk(ccs, function(cc) {
    lissy <- readLines("R/lissy.R") %>% 
      paste(collapse = "\n") %>% 
      str_replace("lis_user", lis_user) %>% 
      str_replace("lis_password", lis_password) %>% 
      str_replace_all("CCODE", cc)

    mailR::send.mail(from = gmail_user,
                     to = "postbox@lisdatacenter.org",
                     subject = cc,
                     body = lissy,
                     smtp = list(host.name = "smtp.gmail.com",
                                 port = 465, 
                                 user.name = gmail_user, 
                                 passwd = gmail_password,
                                 ssl = TRUE),
                     authenticate = TRUE,
                     send = TRUE)
    
    if (length(ccs) > 1) Sys.sleep(wait)
  })
} 

get_lis_ginis(ccs)
