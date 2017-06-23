library(tidyverse)
library(stringr)
library(mailR)

gmail_user <- getOption("gmail_user")         # set all of these in .Rprofile, e.g.: 
gmail_password <- getOption("gmail_password") # options("gmail_user" = "juanita_herrara@gmail.com", 
lis_user <- getOption("lis_user")             #         "gmail_password" = "my_password")
lis_password <- getOption("lis_password")

weldef_eqsc <- c("market_hh", "market_sqrt", "market_pc",
                 "disp_hh", "disp_sqrt", "disp_pc",
                 "con_hh", "con_sqrt", "con_pc")

get_lis_ginis <- function(wd_es) {
  purrr::walk(wd_es, function(weldef_eqsc) {
    wt <- ifelse(str_detect(weldef_eqsc, "hh"), "hpopwgt", "hpopwgt*nhhmem")
    
    lissy <- readLines("R/lissy_stata.txt") %>% 
      paste(collapse = "\n") %>% 
      str_replace("lis_user", lis_user) %>% 
      str_replace("lis_password", lis_password) %>% 
      str_replace_all("WD_ES", weldef_eqsc) %>% 
      str_replace("WT", wt)
    
    mailR::send.mail(from = gmail_user,
                     to = "postbox@lisdatacenter.org",
                     subject = weldef_eqsc,
                     body = lissy,
                     smtp = list(host.name = "smtp.gmail.com",
                                 port = 465, 
                                 user.name = gmail_user, 
                                 passwd = gmail_password,
                                 ssl=TRUE),
                     authenticate = TRUE,
                     send = TRUE)
  })
} 









