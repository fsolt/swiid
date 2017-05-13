library(tidyverse)
library(stringr)
library(mailR)

gmail_user <- getOption("gmail_user")         # set all of these in .Rprofile, e.g.: 
gmail_password <- getOption("gmail_password") # options("gmail_user" = "juanita_herrara@gmail.com", 
lis_user <- getOption("lis_user")             #         "gmail_password" = "my_password")
lis_password <- getOption("lis_password")

weldef_eqsc <- "market_hh"
  c("market_sqrt", "market_pc",
                 "disp_hh", "disp_sqrt", "disp_pc",
                 "con_hh", "con_sqrt", "con_pc")
  c("market_hh", "market_sqrt", "market_pc",
                 "disp_hh", "disp_sqrt", "disp_pc",
                 "con_hh", "con_sqrt", "con_pc")

for (wd_es in weldef_eqsc) {
  lissy <- readLines("R/lissy.R") %>% 
    paste(collapse = "\n") %>% 
    str_replace("lis_user", lis_user) %>% 
    str_replace("lis_password", lis_password) %>% 
    str_replace("WD_ES", wd_es) 
  
  mailR::send.mail(from = sender,
                   to = "postbox@lisdatacenter.org",
                   subject = wd_es,
                   body = lissy,
                   smtp = list(host.name = "smtp.gmail.com",
                               port = 465, 
                               user.name = sender, 
                               passwd = password,
                               ssl=TRUE),
                   authenticate = TRUE,
                   send = TRUE)
}










