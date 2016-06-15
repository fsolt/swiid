if (!require(pacman)) install.packages("pacman")
p_load(readr, readxl, 
       eurostat, rsdmx, xml2, 
       tidyr, stringr, magrittr, dplyr, purrr,
       countrycode)
p_load_gh("leeper/tabulizerjars", "leeper/tabulizer") # read PDF tables
devtools::source_gist(4676064) # as.data.frame.list for CEPALStat


# check if WB gini info is now available and library(wbstats) or library(WDI)

# LIS
kf_link <- "http://www.lisdatacenter.org/wp-content/uploads/data-key-inequality-workbook.xlsx"
download.file(kf_link, "data-raw/data-key-inequality-workbook.xlsx")

old_kf_link <- "https://web.archive.org/web/20100804001129/http://www.lisproject.org/key-figures/kf-workbook.xls"
download.file(old_kf_link, "data-raw/kf-workbook_2010-08-04.xls") 
old_kf <- read_excel("data-raw/kf-workbook_2010-08-04.xls") # get old key figures (for Russia)

kf <- suppressWarnings(read_excel("data-raw/data-key-inequality-workbook.xlsx")) %>%
  select(cy = `LIS Dataset\r\r\n`, gini = `Gini Coefficient`) %>% 
  filter(!str_detect(cy, "Russia")) %>% # Recent data on Russia increasingly implausible; use old version instead
  rbind(old_kf %>% 
          filter(str_detect(`Dataset(s)`, "Russia")) %>% 
          select(cy = `Dataset(s)`, gini = `Gini Coefficient`)) %>% 
  filter(!is.na(gini)) %>% 
  transmute(country = str_extract(cy, "(?<=- )\\D*") %>% str_trim() %>% str_to_title(),
         year = str_extract(cy, "\\d{4}") %>% as.numeric(),
         gini = gini*100,
         equiv_scale = "sqrt",
         welfare_def = "net",
         monetary = FALSE,
         series = "LIS Key Figures",
         source1 = "LIS",
         page = "Key Figures", 
         link = ifelse(!country=="Russia", kf_link, old_kf_link)) %>% 
  arrange(country, year)
kf$country <- str_replace(kf$country, "Russia", "Russian Federation")
kf$country <- str_replace(kf$country, "South Korea", "Korea, Republic of")

lis_net_sqrt_raw <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/LISSY/job_396844_dhi-sqrt.txt",
                       col_names = FALSE, skip = 80)
lis_nc_raw <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/LISSY/job_396995_dhi-pc.txt",
                       col_names = FALSE, skip = 80)
# lis_nh_raw <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/LISSY/job_.txt",
#                        col_names = FALSE, skip = )
lis_me_raw <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/LISSY/job_396882_mi-sqrt.txt",
                       col_names = FALSE, skip = 71)
lis_mc_raw <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/LISSY/job_396886_mi-pc.txt",
                       col_names = FALSE, skip = 71)
lis_mh_raw <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/LISSY/job_396952_mi-hh.txt",
                       col_names = FALSE, skip = 69)

lis_ne <-  lis_ne_raw %>%  
  filter(!is.na(X2)) %>% 
  transmute(country = str_extract(X1, "\\D{2}") %>%
              toupper() %>% 
              countrycode("iso2c", "country.name"),
            year = ifelse(str_extract(X1, "\\d{2}") %>% as.numeric() > 66,
                          str_extract(X1, "\\d{2}") %>% as.numeric() + 1900,
                          str_extract(X1, "\\d{2}") %>% as.numeric() + 2000),
            gini = (str_trim(X2) %>% as.numeric())*100,
            gini_se = (str_trim(X3) %>% as.numeric())*100,
            equiv_scale = "sqrt",
            welfare_def = "net",
            monetary = FALSE,
            series = "LIS",
            source1 = "LISSY",
            page = "", 
            link = "https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/LISSY/job_396844_dhi-sqrt.txt") %>% 
  arrange(country, year)


format_lis <- function(x) {
  paste0("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/LISSY/", 
         x, ".txt") %>%
    readLines() %>% 
    str_subset("^\\D{2}\\d{2},.*") %>%
    paste(collapse = "\n") %>% 
    read_csv(col_names = FALSE) %>%
    transmute(country = str_extract(X1, "\\D{2}") %>%
                toupper() %>% 
                str_replace("UK", "GB") %>% 
                countrycode("iso2c", "country.name") %>% 
                str_replace(", Province of China", ""),
              year = ifelse(str_extract(X1, "\\d{2}") %>% as.numeric() > 66,
                            str_extract(X1, "\\d{2}") %>% as.numeric() + 1900,
                            str_extract(X1, "\\d{2}") %>% as.numeric() + 2000),
              gini = (str_trim(X2) %>% as.numeric())*100,
              gini_se = (str_trim(X3) %>% as.numeric())*100,
              equiv_scale = str_extract(x, "[^_]*"),
              welfare_def = str_extract(x, "(?<=_).*"),
              monetary = FALSE,
              series = "LIS",
              source1 = "LISSY",
              page = "",
              link = paste0("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/LISSY/",
                            x, ".txt")) %>% 
    arrange(country, year)
}

lis_files <- c("net_sqrt", "net_pc", "net_hh", "market_sqrt", "market_pc", "market_hh")

lis <- lis_files %>% map_df(format_lis)

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
  s <- x %>% split(.$country) %>% map_df(zoo::na.locf)
  x$series <- c(NA, s$series)
  
  x %<>% filter(!is.na(gini)) %>%
    transmute(country = country,
              year = as.numeric(year),
              gini = gini * 100,
              gini_se = se * 100,
              equiv_scale = es,
              welfare_def = "net",
              monetary = TRUE,
              series = series,
              source1 = "SEDLAC",
              page = sheet, 
              link = link)
  return(x)
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
                es = "adeq")

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

rm(sedlac_ei, sedlac_hh, sedlac_pc)


# CEPALStat
# http://interwp.cepal.org/sisgen/ConsultaIntegrada.asp?idIndicador=250&idioma=e
# consider informative series var
cepal_link <- "http://interwp.cepal.org/sisgen/ws/cepalstat/getDataMeta.asp?IdIndicator=250"
cepal0 <- cepal_link %>% read_xml() 
cepal_raw <- cepal0 %>% xml_find_all("//dato") %>% xml_attrs() %>% as.data.frame()

cepal_labels <- cepal0 %>% xml_find_all("//des") %>% 
  xml_attrs() %>% 
  as.data.frame() %>% 
  select(one_of(c("name", "id")))

cepal_notes <- cepal0 %>% xml_find_all("//nota") %>% 
  xml_attrs() %>% 
  as.data.frame()

cepal <- left_join(cepal_raw, cepal_labels, by = c("dim_208" = "id")) %>%
  mutate(country = countrycode(as.character(name), "country.name", "country.name") %>% 
           str_replace(",.*", "")) %>% 
  filter(!is.na(country)) %>%
  select(-name) %>% 
  left_join(cepal_labels, by = c("dim_29117" = "id")) %>%
  mutate(year = as.numeric(levels(name))[name]) %>% 
  select(-name) %>% 
  left_join(cepal_labels, by = c("dim_326" = "id")) %>%
  mutate(area = as.character(name)) %>%
  filter(area == "National" | (area == "Urban" & (country == "Argentina" | country == "Uruguay"))) %>% 
  select(-name) %>% 
  left_join(cepal_notes, by = c("ids_notas" = "id")) %>%
  group_by(country, area) %>% 
  transmute(year = year,
            gini = as.numeric(as.character(valor)) * 100,
            equiv_scale = "hpc",
            welfare_def = "net",
            monetary = TRUE,
            notes = ifelse(is.na(descripcion), "", as.character(descripcion)),
            series = paste("CEPAL", country, "series", as.numeric(factor(notes, levels = unique(notes)))),
            source1 = "CEPALStat",
            link = cepal_link) %>% 
  ungroup()

rm(cepal0, cepal_raw, cepal_labels, cepal_notes)


# OECD Income Distribution Database
# http://stats.oecd.org > Data by Theme: search "income distribution"; Customize: all countries, ginis only, total pop only, 1974 to latest
oecd_link <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/IDD/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+RUS.GINI+STDG+GINIB+GINIG.TOT.CURRENT+PREVIOUS+INCOMPARABLE.METH2012+METH2011/all"
oecd0 <- oecd_link %>% readSDMX() %>% as.data.frame() %>% 
  mutate(country = countrycode(LOCATION, "iso3c", "country.name"),
         year = as.numeric(obsTime),
         gini = obsValue * 100,
         equiv_scale = "sqrt",   
         welfare_def = MEASURE,
         monetary = FALSE,
         series = paste("OECD", tolower(DEFINITION), "definition,", 
                        tolower(str_replace(METHODO, "METH", "")), "method"),
         source1 = "OECD",
         page = NA, 
         link = oecd_link)

oecd_se <- oecd0 %>% filter(MEASURE=="STDG") %>% 
  select(country:series) %>% 
  mutate(gini_se = gini,
         welfare_def = "GINI") %>% 
  select(-gini)

oecd <- oecd0 %>% filter(MEASURE!="STDG") %>% 
  select(country:series) %>% 
  left_join(oecd_se, by = c("country", "year", "equiv_scale", "welfare_def", "monetary", "series"))
oecd$welfare_def <- car::recode(oecd$welfare_def, 
              "'GINI' = 'net';
              'GINIB' = 'market';
              'GINIG' = 'gross'")

rm(oecd0, oecd_se)         

       
# Eurostat
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=ilc_di12&lang=en; Customization: all years
eurostat <- get_eurostat("ilc_di12", time_format = "num", update_cache = FALSE) %>% 
  label_eurostat(code = "geo")  %>% 
  left_join(get_eurostat("ilc_di12", time_format = "num", keepFlags = TRUE) %>%
              rename(geo_code = geo), by = c("geo_code", "time", "values")) %>% 
  transmute(country = countrycode(as.character(geo), "country.name", "country.name"),
         year = time,
         gini = values,
         equiv_scale = "OECDmod",   
         welfare_def = "net",
         monetary = TRUE,
         break_yr = ifelse(is.na(flags) | flags!="b", 0, 1),
         source1 = "Eurostat",
         page = NA,
         link = "http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=ilc_di12&lang=en",
         geo = geo) %>% 
  mutate(country = ifelse(is.na(country), as.character(geo), country)) %>% 
  select(-geo) %>% 
  filter(!is.na(gini)) %>% 
  group_by(country) %>% 
  arrange(country, year) %>% 
  mutate(series = paste("Eurostat", country, "series", cumsum(break_yr) + 1)) %>%  # No word from Eurostat which obs cross-nationally comparable
  ungroup()

#Commitment to Equity
ceq <- read_csv("data-raw/ceq.csv")


