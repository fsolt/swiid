library(dplyr)
library(readr)
library(readxl)
library(eurostat)
library(OECD)
library(tidyr)
library(stringr)
library(magrittr)

# LIS

# Socio-Economic Database for Latin America and the Caribbean (SEDLAC)
format_sedlac <- function(df, sheet, link, es) {
  x <- df
  if(ncol(x)==2) {
    x$se <- NA
  }
  names(x) <- c("heading", "gini", "se")
  x %<>% filter(!is.na(heading))
  countries_sedlac <- "Argentina|Bolivia|Brazil|Chile|Colombia|Costa|Dominican|Ecuador|El Salvador|Guatemala|Honduras|Mexico|Nicaragua|Panama|Paraguay|Peru|Uruguay|Venezuela|Caribbean|Belice|Guyana|Haiti|Jamaica|Suriname"
  x$h_co <- str_detect(x$heading, countries_sedlac)
  x$country <- ifelse(x$h_co, x$heading, NA)
  x$country <- c(NA, zoo::na.locf(x$country))
  
  x$year <- ifelse(str_detect(x$heading, ".*(\\d{4}).*"),
                           str_replace(x$heading, "(\\d{4}).*", "\\1"), NA)
  
  x$series <- ifelse(!x$h_co & is.na(x$year), x$heading, NA)
  s <- do.call(rbind, by(x, x$country, zoo::na.locf))
  x$series <- c(NA, s$series)
  
  x %<>% filter(!is.na(gini)) %>% transmute(country = country,
                                         year = year,
                                         gini = gini * 100,
                                         se = se * 100,
                                         equiv_scale = es,
                                         welfare_def = "Monetary Income, Disposable",
                                         series = series,
                                         source1 = "SEDLAC",
                                         page = sheet, 
                                         link = link)
  x
}

sedlac_link <- "http://sedlac.econo.unlp.edu.ar/download.php?file=archivos_estadistica/inequality_LAC_2015-06.xls"
download.file(sedlac_link, "data-raw/sedlac.xls")

sedlac_pc <- read_excel(path = "data-raw/sedlac.xls", 
                        sheet = "intervals pci",
                        skip = 8)[1:3] %>%
  format_sedlac(sheet = "intervals pci",
                link = sedlac_link,
                es = "hhpc") 

sedlac_ei <- read_excel(path = "data-raw/sedlac.xls",
                        sheet = "intervals ei",
                        skip = 8)[1:3] %>%
  format_sedlac(sheet = "intervals ei",
                link = sedlac_link,
                es = "hh eq, ad eq")

sedlac_hh <- read_excel(path = "data-raw/sedlac.xls",
                        sheet = "gini1",
                        skip = 7)[c(1,8)] %>%
  format_sedlac(sheet = "gini1",
                link = sedlac_link,
                es = "hh")

sedlac <- rbind(sedlac_ei, sedlac_hh, sedlac_pc)
sedlac$country <- car::recode(sedlac$country, 
                              "'Dominican Rep. ' = 'Dominican Republic';
                              'Belice' = 'Belize'")

# CEPALStat
# http://interwp.cepal.org/sisgen/ConsultaIntegrada.asp?idIndicador=250&idioma=e
# Customization: countries only, all years, national only; area and países in rows, years in columns; resave as .xlsx


# OECD Income Distribution Database
# http://stats.oecd.org > Data by Theme: search "income distribution"; Customize: all countries, ginis only, total pop only, 1974 to latest
oecd <- get_dataset("PATS_REGION",
                    filter = "PCT_A.INVENTORS.BEL+BE10.TOTAL+BIOTECH",
                    pre_formatted = TRUE)

# Eurostat (break years in break_yr, but individual series not identified yet)
eurostat <- get_eurostat("ilc_di12", time_format = "num", update_cache = TRUE) %>% 
  label_eurostat(code = "geo") %>% 
  left_join(get_eurostat("ilc_di12", time_format = "num", keepFlags = TRUE) %>%
              rename(geo_code = geo), by = c("geo_code", "time", "values")) %>% 
  transmute(country = as.character(geo),
         year = time,
         gini = values,
         break_yr = (flags=="b"))
