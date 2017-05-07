if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, readxl, 
       eurostat, rsdmx, xml2, CANSIM2R, pxweb,
       stringr, magrittr, countrycode)
p_load_gh("ropensci/tabulizerjars", "ropensci/tabulizer") # read PDF tables

# LIS
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
              year = ifelse(str_extract(X1, "\\d{2}") %>% as.numeric() > 50,
                            str_extract(X1, "\\d{2}") %>% as.numeric() + 1900,
                            str_extract(X1, "\\d{2}") %>% as.numeric() + 2000),
              gini = (str_trim(X2) %>% as.numeric()),
              gini_se = (str_trim(X3) %>% as.numeric()),
              welfare_def = str_extract(x, "[^_]*"),
              equiv_scale = str_extract(x, "(?<=_).*"),
              monetary = FALSE,
              series = paste("LIS", welfare_def, equiv_scale),
              source1 = "LISSY",
              page = "",
              link = paste0("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/LISSY/",
                            x, ".txt")) %>% 
    arrange(country, year)
}

format_lis_xtra <- function(x) {
  paste0("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/LISSY/", 
         x, ".txt") %>%
    readLines() %>% 
    str_subset("^\\D{2}\\d{2},.*") %>%
    paste(collapse = "\n") %>% 
    read_csv(col_names = FALSE) %>%
    transmute(country = str_extract(X1, "\\D{2}") %>% 
                toupper() %>% 
                countrycode("iso2c", "country.name"),
              year = ifelse(str_extract(X1, "\\d{2}") %>% as.numeric() > 66,
                            str_extract(X1, "\\d{2}") %>% as.numeric() + 1900,
                            str_extract(X1, "\\d{2}") %>% as.numeric() + 2000),
              gini = (str_trim(X2) %>% as.numeric()),
              gini_se = (str_trim(X3) %>% as.numeric()),
              equiv_scale = "sqrt",
              welfare_def = "net",
              monetary = FALSE,
              series = "LIS net sqrt",
              source1 = ifelse(country=="New Zealand", "Statistics New Zealand 1999", "LISSY"),
              page = ifelse(country=="New Zealand", "73", ""),
              link = ifelse(country=="New Zealand", 
                            "http://www2.stats.govt.nz/domino/external/PASFull/pasfull.nsf/173371ce38d7627b4c25680900046f25/4c2567ef00247c6acc256b03000bdbe0/$FILE/Incomes.pdf", 
                            "https://web.archive.org/web/20100804001129/http://www.lisproject.org/key-figures/kf-workbook.xls")) %>% 
    arrange(country, year)
}

lis_files <- c("net_sqrt", "net_pc", "net_hh", 
               "market_sqrt", "market_pc", "market_hh",
               "con_sqrt", "con_pc", "con_hh")

lis <- lis_files %>% 
  map_df(format_lis) %>% 
  rbind(format_lis_xtra("net_sqrt_nz"), format_lis_xtra("net_sqrt_ru")) %>% 
  arrange(country, year, welfare_def, equiv_scale)



# Socio-Economic Database for Latin America and the Caribbean (SEDLAC)
format_sedlac <- function(df, sheet, link, es) {
  x <- df
  if(ncol(x)==2) {
    x$se <- NA
  }
  names(x) <- c("heading", "gini", "se")
  x %<>% filter(!is.na(heading) & !str_detect(heading, "-II"))
  countries_sedlac <- "Argentina|Bolivia|Brazil|Chile|Colombia|Costa|Dominican|Ecuador|El Salvador|Guatemala|Honduras|Mexico|Nicaragua|Panama|Paraguay|Peru|Uruguay|Venezuela|Caribbean|Belice|Guyana|Haiti|Jamaica|Suriname"
  x$h_co <- str_detect(x$heading, countries_sedlac)
  x$country <- ifelse(x$h_co, str_trim(x$heading), NA)
  x$country <- c(NA, zoo::na.locf(x$country)) 
  x$country <- x$country %>% 
    str_replace("Dominican Rep.", "Dominican Republic") %>% 
    str_replace("Belice", "Belize")
    
  x$year <- ifelse(str_detect(x$heading, ".*(\\d{4}).*"),
                   str_replace(x$heading, "(\\d{4}).*", "\\1"), NA)
  
  x$h_ser <- ((!x$h_co) && is.na(x$year))
  x$series <- ifelse(!x$h_co & is.na(x$year), x$heading, NA)
  x %<>% group_by(country) %>%
    mutate(series0 = zoo::na.locf(series, na.rm = FALSE),
           series = paste("SEDLAC", country, "net", es,
                   as.numeric(factor(series0, levels = unique(series0)))) %>% 
             str_replace("NA", "1")) %>% 
    ungroup() %>% 
    filter(!is.na(gini)) %>%
    transmute(country = country,
              year = as.numeric(year),
              gini = gini,
              gini_se = se,
              welfare_def = "net",
              equiv_scale = es,
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
                es = "pc") 

sedlac_ei <- read_excel(path = "data-raw/sedlac.xls",
                        sheet = "intervals ei",
                        skip = 8)[1:3] %>%
  format_sedlac(sheet = "intervals ei",
                link = sedlac_link,
                es = "ae")

sedlac_hh <- read_excel(path = "data-raw/sedlac.xls",
                        sheet = "gini1",
                        skip = 7)[c(1,8)] %>%
  format_sedlac(sheet = "gini1",
                link = sedlac_link,
                es = "hh")

sedlac <- rbind(sedlac_ei, sedlac_hh, sedlac_pc)

rm(sedlac_ei, sedlac_hh, sedlac_pc)


# CEPALStat
# http://interwp.cepal.org/sisgen/ConsultaIntegrada.asp?idIndicador=250&idioma=e
# consider informative series var
cepal_link <- "http://interwp.cepal.org/sisgen/ws/cepalstat/getDataMeta.asp?IdIndicator=250"
cepal0 <- cepal_link %>% read_xml() 
cepal_extract <- function(x) {
  cepal0 %>% xml_find_all(x) %>% 
    xml_attrs() %>% 
    map_df(function(y) data.frame(as.list(y), stringsAsFactors = FALSE)) %>% 
    return()
}

cepal_raw <- cepal_extract("//dato")
cepal_labels <- cepal_extract("//des") %>% 
  select(-`in.`)
cepal_notes <- cepal_extract("//nota")

cepal <- left_join(cepal_raw, cepal_labels, by = c("dim_208" = "id")) %>%
  mutate(country = countrycode(as.character(name), "country.name", "country.name") %>% 
           str_replace(",.*", "")) %>% 
  filter(!is.na(country)) %>%
  select(-name) %>% 
  left_join(cepal_labels, by = c("dim_29117" = "id")) %>%
  mutate(year = as.numeric(name)) %>% 
  select(-name) %>% 
  left_join(cepal_labels, by = c("dim_326" = "id")) %>%
  mutate(area = as.character(name)) %>%
  filter(area == "National" | (area == "Urban" & (country == "Argentina" | country == "Uruguay"))) %>% 
  select(-name) %>% 
  left_join(cepal_notes, by = c("ids_notas" = "id")) %>%
  group_by(country) %>% 
  transmute(year = year,
            gini = as.numeric(as.character(valor)),
            gini_se = NA,
            welfare_def = "net",
            equiv_scale = "pc",
            monetary = TRUE,
            notes = paste(area, ifelse(is.na(descripcion), "", as.character(descripcion))),
            series = paste("CEPAL", country, "net pc", as.numeric(factor(notes, levels = unique(notes)))),
            source1 = "CEPALStat",
            page = area,
            link = cepal_link) %>% 
  ungroup() %>% 
  select(-notes)

rm(cepal0, cepal_raw, cepal_labels, cepal_notes)

# CEPAL Serie Distribución del Ingreso
cepal_sdi <- read_tsv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/repositorio_cepal.tsv") %>% 
  transmute(country = country,
            year = year,
            gini = gini/100,
            gini_se = NA,
            monetary = str_detect(welfare_def, "Monetary"),
            welfare_def = ifelse(str_detect(welfare_def, "Disposable"), "net",
                                 ifelse(str_detect(welfare_def, "Gross"), "gross",
                                        "market")),
            equiv_scale = equiv_scale,
            series = paste("CEPAL SDI", country, welfare_def, equiv_scale, survey),
            source1 = source,
            page = NA,
            link = link)


# OECD Income Distribution Database
# http://stats.oecd.org > Data by Theme: search "income distribution"; Customize: all countries, ginis only, total pop only, 1974 to latest
oecd_link <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/IDD/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+RUS.GINI+STDG+GINIB+GINIG.TOT.CURRENT+PREVIOUS+INCOMPARABLE.METH2012+METH2011/all"
oecd0 <- oecd_link %>% 
  readSDMX() %>% 
  as.data.frame() %>% 
  transmute(country = countrycode(LOCATION, "iso3c", "country.name"),
         year = as.numeric(obsTime),
         gini = obsValue,
         welfare_def = ifelse((MEASURE=="GINI" | MEASURE=="STDG"), "net", 
                              ifelse(MEASURE=="GINIB", "market", "gross")),
         equiv_scale = "sqrt",   
         monetary = FALSE,
         series = paste("OECD", welfare_def, "sqrt,", tolower(DEFINITION), "def,", 
                        tolower(str_replace(METHODO, "METH", "")), "method"),
         source1 = "OECD",
         page = NA, 
         link = oecd_link,
         measure = MEASURE)

oecd_se <- oecd0 %>% filter(measure=="STDG" & gini!=0) %>% 
  mutate(gini_se = gini) %>% 
  select(-gini, -measure)

oecd <- oecd0 %>% filter(measure!="STDG") %>% 
  left_join(oecd_se, by = c("country", "year", "equiv_scale", "welfare_def", "monetary", "series", "source1", "page", "link")) %>% 
  select(-measure)

rm(oecd0, oecd_se)         

       
# Eurostat
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=ilc_di12&lang=en; Customization: all years
eurostat <- get_eurostat("ilc_di12", 
                         time_format = "num", 
                         update_cache = TRUE,
                         keepFlags = TRUE) %>%
  mutate(geo = as.character(geo) %>% recode("UK" = "GB", "EL" = "GR")) %>% 
  transmute(country = countrycode(as.character(geo), "iso2c", "country.name"),
         year = time - (!(country=="United Kingdom" | country=="Ireland")), #eurostat reports survey year not ref year except in UK and IE <http://ec.europa.eu/eurostat/cache/metadata/en/ilc_esms.htm#ref_period>
         gini = values/100,
         gini_se = NA,
         welfare_def = "net",
         equiv_scale = "oecdm",   
         monetary = TRUE,
         break_yr = ifelse(is.na(flags) | flags!="b", 0, 1),
         series = "",
         source1 = "Eurostat",
         page = "",
         link = "http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=ilc_di12&lang=en") %>% 
  filter(!(is.na(country) | is.na(gini))) %>% 
  group_by(country) %>% 
  arrange(country, year) %>% 
  mutate(series = paste("Eurostat", country, welfare_def, equiv_scale, cumsum(break_yr) + 1)) %>%  # No word from Eurostat which obs cross-nationally comparable
  ungroup() %>% 
  select(-break_yr)


# Transmonee 2012 (2012 is the last database that includes inequality data)
transmonee_link <- "https://web.archive.org/web/20130401075747/http://www.transmonee.org/Downloads/EN/2012/TransMonEE_2012.xls"
download.file(transmonee_link, "data-raw/transmonee.xls")

transmonee <- suppressWarnings(read_excel("data-raw/transmonee.xls", 
                                          sheet = "10. Economy",
                                          skip = 398,
                                          na = "-")[1:34, ]) %>% 
  mutate(country = c("Czech Republic", "Hungary", "Poland",
                     "Slovakia", "Slovenia", "", "Estonia", "Latvia",
                     "Lithuania", "", "Bulgaria", "Romania", "",
                     "Albania", "Bosnia and Herzegovina", "Croatia",
                     "Montenegro", "Serbia", "Macedonia, the former Yugoslav Republic of", "",
                     "Belarus", "Moldova", "Russian Federation", 
                     "Ukraine", "", "Armenia", "Azerbaijan", "Georgia",
                     "", "Kazakhstan", "Kyrgyzstan", "Tajikistan",
                     "Turkmenistan", "Uzbekistan")) %>% 
  filter(country!="") %>% 
  gather(key = year, value = gini, -country) %>% 
  filter(!is.na(gini)) %>% 
  transmute(country = country,
            year = as.numeric(year),
            gini = gini,
            gini_se = NA,
            welfare_def = "net",
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("Transmonee", country, welfare_def, equiv_scale),
            source1 = "Transmonee 2012",
            page = "",
            link = transmonee_link)

# Commitment to Equity
ceq <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/ceq.csv", col_types = "cnnncclcccc") %>% 
  mutate(series = paste("CEQ", welfare_def, equiv_scale))

# World Bank Africa Poverty Database (carefully vetted subset of WDI)
afr_wb3c <- countrycode::countrycode_data %>% 
  filter(continent=="Africa" & !is.na(wb_api3c)) %>% 
  `[[`("wb_api3c") %>% 
  setdiff(c("DZA", "DJI", "EGY", "LBY", "MAR", "TUN"))

afr_gini_pc <- wbstats::wb(indicator = "SI.POV.GINI",
               startdate = 1960, 
               enddate = 2016, 
               country = afr_wb3c) %>% 
  transmute(country = country,
            year = as.numeric(date),
            gini = value,
            gini_se = NA,
            welfare_def = "con",
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("World Bank Africa Poverty Database", country, welfare_def, equiv_scale),
            source1 = "Beegle et al. 2016",
            page = "121-126",
            link = "http://databank.worldbank.org/data/reports.aspx?source=2&series=SI.POV.GINI")

afr_gini_sqrt <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/AFR_gini_sqrt.csv") %>% 
  transmute(country = country,
            year = as.numeric(surveyr),
            gini = gini_sqrthhs,
            gini_se = NA,
            welfare_def = "con",
            equiv_scale = "sqrt",
            monetary = FALSE,
            series = paste("World Bank Africa Poverty Database", country, welfare_def, equiv_scale),
            source1 = "Personal communication, K. Beegle, 2016-08-01",
            page = "",
            link = "https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/AFR_gini_sqrt.csv")

afr_gini <- bind_rows(afr_gini_pc, afr_gini_sqrt)


## National Statistics Offices

# Australian Bureau of Statistics
abs_link <- "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&6523DO00001_201314.xls&6523.0&Data%20Cubes&4F00682720AFA825CA257EB5001B77B9&0&2013-14&16.12.2015&Latest"
download.file(abs_link, "data-raw/abs.xls")

abs_format <- function(sheet, wd, es) {
  x <- read_excel("data-raw/abs.xls",
                  sheet = sheet,
                  skip = 4) %>% `[`(c(38, 90), 3:15) %>%
    mutate(var = c("gini", "gini_se")) %>% 
    gather(year, value, -var) %>% 
    spread(var, value) %>% 
    separate(year, into = c("year", "series"), sep = "\\(", fill = "right") %>% 
    transmute(country = "Australia",
              year = ifelse(str_extract(year, "\\d{2}$") %>% as.numeric() > 50,
                            str_extract(year, "\\d{2}$") %>% as.numeric() + 1900,
                            str_extract(year, "\\d{2}$") %>% as.numeric() + 2000),
              gini = as.numeric(gini),
              gini_se = as.numeric(gini_se)/100 * gini,
              welfare_def = wd,
              equiv_scale = es,
              monetary = FALSE,
              series = paste("ABS", welfare_def, equiv_scale,
                             as.numeric(factor(str_replace_na(series), levels = unique(str_replace_na(series))))),
              source1 = "Australian Bureau of Statistics",
              page = sheet,
              link = abs_link)
  return(x)
}
       
abs_ne <- abs_format("Table 1.1", "net", "oecdm")

abs_gh <- abs_format("Table 1.2", "gross", "hh")

abs <- bind_rows(abs_ne, abs_gh)
  
# Statistics Canada
statcan <- CANSIM2R:::downloadCANSIM(2060033) %>% 
  filter(GEO=="Canada") %>% 
  mutate(equiv_scale = "sqrt",
         link = "http://www5.statcan.gc.ca/cansim/a26?id=2060033") %>% 
  bind_rows(CANSIM2R:::downloadCANSIM(2020709) %>% 
              filter(GEO=="Canada" & FAMILYTYPE=="All family units") %>% 
              mutate(equiv_scale = "pc",
                     link = "http://www5.statcan.gc.ca/cansim/a26?id=2020709")) %>% 
  bind_rows(CANSIM2R:::downloadCANSIM(2020705) %>% 
              filter(GEO=="Canada" & FAMILYTYPE=="All family units") %>% 
              mutate(equiv_scale = "hh",
                     link = "http://www5.statcan.gc.ca/cansim/a26?id=2020705")) %>% 
  transmute(country = "Canada",
            year = Ref_Date,
            gini = Value,
            gini_se = gini*.02, # StatCan indicates CV < .02
            welfare_def = tolower(INCOMECONCEPT) %>% 
              str_extract("market|after-tax|total") %>% 
              str_replace("after-tax", "net") %>% 
              str_replace("total", "gross"),
            equiv_scale = equiv_scale,
            monetary = TRUE,
            series = paste("Statistics Canada", welfare_def, equiv_scale),
            source1 = "Statistics Canada",
            page = "",
            link = link)

# Statistics Estonia
statee <- read_tsv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/statistics_estonia.tsv", col_names = FALSE) %>% 
  rename(year = X1, pc = X2, oecdm = X3) %>% 
  gather(key = equiv_scale, value = gini, pc:oecdm) %>% 
  transmute(country = "Estonia",
          year = year,
          gini = gini,
          gini_se = NA,
          welfare_def = "con",
          equiv_scale = equiv_scale,
          monetary = FALSE,
          series = paste("Statistics Estonia", welfare_def, equiv_scale),
          source1 = "Statistics Estonia",
          page = "",
          link = "http://pub.stat.ee/px-web.2001/dialog/varval.asp?ma=HH30")

# Statistics Finland
statfi <- get_pxweb_data(url = "http://pxwebapi2.stat.fi/PXWeb/api/v1/en/StatFin/tul/tjt/270_tjt_tau_117.px",
                         dims = list(Tulokäsite = c("SL2", "4L2", "6L2"),
                                     Vuosi = c("*"),
                                     Tiedot = c("Gini")),
                         clean = TRUE) %>% 
  transmute(country = "Finland",
            year = as.numeric(as.character(Year)),
            gini = values/100,
            gini_se = NA,
            welfare_def = ifelse(str_detect(`Income concept`, "Disposable"), "net",
                                 ifelse(str_detect(`Income concept`, "Gross"), "gross",
                                        "market")),
            equiv_scale = "oecdm",
            monetary = FALSE,
            series = paste("Statistics Finland", welfare_def, equiv_scale),
            source1 = "Statistics Finland",
            page = "",
            link = "http://pxnet2.stat.fi/PXWeb/pxweb/en/StatFin/StatFin__tul__tjt/270_tjt_tau_117.px")

# Insee (not available 2017-05-05)
# insee_link <- "http://www.bdm.insee.fr/bdm2/exporterSeries.action?idbank=001687249&periode=toutes&liste_formats=xls"
# download.file(insee_link, "data-raw/insee.xls")
# 
# insee <- read_excel("data-raw/insee.xls", skip = 3, col_names = c("year", "gini")) %>% 
#   filter(!is.na(gini)) %>% 
#   transmute(country = "France",
#             year = year,
#             gini = gini,
#             gini_se = NA,
#             welfare_def = "net",
#             equiv_scale = "oecdm",
#             monetary = FALSE,
#             series = paste("Insee", welfare_def, equiv_scale),
#             source1 = "Insee",
#             page = "",
#             link = insee_link)

# Statistics Georgia
geostat <- read_csv("data-raw/geostat.csv", skip = 3, col_names = c("year", "gross", "con")) %>% 
  filter(!is.na(gross)) %>% 
  gather(key = "welfare_def", value = "gini", gross:con) %>% 
  transmute(country = "Georgia",
            year = as.numeric(year),
            gini = gini,
            gini_se = NA,
            welfare_def = welfare_def,
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("Geostat", welfare_def, equiv_scale),
            source1 = "Geostat",
            page = "",
            link = "http://pc-axis.geostat.ge")

# Istat
istat <- read_csv("data-raw/istat.csv") %>% 
  transmute(country = "Italy",
            year = Year,
            gini = `0`,
            gini_se = NA,
            welfare_def = "con",
            equiv_scale = "hh",
            monetary = FALSE,
            series = paste("Istat", welfare_def, equiv_scale),
            source1 = "Istat",
            page = "",
            link = "http://dati.istat.it/Index.aspx?DataSetCode=DCCV_INDCONSUMI&Lang=en")
 
# Statistics Norway
ssb_link <- "http://www.ssb.no/eksport/tabell.csv?key=211301"
download.file(ssb_link, "data-raw/ssb.csv")

ssb <- read_delim("data-raw/ssb.csv", ";", col_names = FALSE, skip = 3) %>%
  select(X1, X2, X3) %>%
  transmute(country = "Norway",
            year = as.numeric(X1),
            gini = as.numeric(X2),
            gini_se = suppressWarnings(as.numeric(X3)),
            welfare_def = "net",
            equiv_scale = "oecdm",
            monetary = TRUE,
            series = paste("SSB", welfare_def, equiv_scale),
            source1 = "Statistics Norway",
            page = "",
            link = ssb_link)

# DGEEC Paraguay
dgeec_link <- "http://www.dgeec.gov.py/Publicaciones/Biblioteca/eph2015/Boletin%20de%20pobreza%202015.pdf"
download.file(dgeec_link, "data-raw/dgeec.pdf")

dgeec <- extract_tables("data-raw/dgeec.pdf", pages = 15)[[1]][-1, ] %>%
  as.data.frame(stringsAsFactors=FALSE) %>% 
  transmute(country = "Paraguay",
            year = ifelse(str_trim(V1) %>% str_extract("\\d{2}$") %>% as.numeric() > 50,
                          str_trim(V1) %>% str_extract("\\d{2}$") %>% as.numeric() + 1900,
                          str_trim(V1) %>% str_extract("\\d{2}$") %>% as.numeric() + 2000),
            gini = as.numeric(sub(",", ".", V4, fixed = TRUE)),
            gini_se = NA,
            welfare_def = "gross",
            equiv_scale = "pc",
            monetary = TRUE,
            series = paste("DGEEC", welfare_def, equiv_scale),
            source1 = "Dirección General de Estadística, Encuestas y Censos 2016",
            page = "14",
            link = dgeec_link)

# Russian Federal State Statistics Service
rosstat_link <- "http://www.gks.ru/free_doc/doc_2015/year/pril_year15-eng.xls"
download.file(rosstat_link, "data-raw/rosstat.xls")

rosstat <- read_excel("data-raw/rosstat.xls", sheet = "Sec.5", skip = 1) %>% 
  filter(str_detect(`  INDICATORS `, "Gini")) %>% 
  gather(key = year, value = gini) %>% 
  mutate_all(as.numeric) %>% 
  filter(!is.na(gini)) %>% 
  transmute(country = "Russian Federation",
            year = year,
            gini = gini,
            gini_se = ifelse(year<1993, .03, NA),
            welfare_def = "gross",
            equiv_scale = "pc",
            monetary = TRUE,
            series = paste("Rosstat", welfare_def, equiv_scale),
            source1 = "Russian Federal State Statistics Service 2015",
            page = "Sec.5",
            link = rosstat_link) 

# Statistics Sweden
scb <- get_pxweb_data(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/HE/HE0103/HE0103A/DispInk8",
                      dims = list(Hushallsdef = c('FAME'),
                                  InkomstTyp = c('*'),
                                  ContentsCode = c('HE0103AD'),
                                  Tid = c('*')),
                      clean = TRUE)
scb <- scb %>%
  transmute(country = "Sweden",
            year = as.numeric(as.character(år)),
            gini = values,
            gini_se = NA,
            welfare_def = ifelse(str_detect(inkomstslag, "disponibel"), "net",
                                 "market"),
            equiv_scale = "ae",
            monetary = FALSE,
            series = paste("Statistics Sweden", welfare_def, equiv_scale, ifelse(year<1991, 1, 2)),
            source1 = "Statistics Sweden",
            page = "",
            link = "http://www.scb.se/en_/Finding-statistics/Statistics-by-subject-area/Household-finances/Income-and-income-distribution/Households-finances/Aktuell-Pong/7296/Income-aggregate-19752011/163550")

# Taiwan Directorate General of Budget, Accounting, and Statistics
tdgbas_link <- "http://win.dgbas.gov.tw/fies/doc/result/103/a11/Year05.xls"
download.file(tdgbas_link, "data-raw/tdgbas1.xls")

tdgbas <- read_excel("data-raw/tdgbas1.xls", col_names = FALSE, skip = 9) %>% 
  transmute(year = X2,
            pc = X4,
            sqrt = X6) %>% 
  gather(key = equiv_scale, value = gini, -year) %>% 
  filter(!is.na(year)) %>% 
  mutate(link = tdgbas_link) %>% 
  bind_rows(suppressWarnings(read_csv("data-raw/tdgbas2.csv", col_names = FALSE, skip = 4)) %>% 
              filter(!is.na(X1)) %>% 
              transmute(year = X1, 
                        gini = X2,
                        equiv_scale = "hh",
                        link = "http://statdb.dgbas.gov.tw/pxweb/Dialog/varval.asp?ma=FF0004A1A&ti=Percentage%20Share%20of%20Disposable%20Income%20by%20Percentile%20Group%20of%20Households%20and%20Income%20Inequality%20Indexes-Annual&path=../PXfileE/HouseholdFinances/&lang=1&strList=L")) %>% 
  transmute(country = "Taiwan",
            year = year,
            gini = gini,
            gini_se = NA,
            welfare_def = "net",
            equiv_scale = equiv_scale,
            monetary = FALSE,
            series = paste("DGBAS", welfare_def, equiv_scale),
            source1 = "Taiwan Directorate General of Budget, Accounting, and Statistics",
            page = "",
            link = link)

# Statistics Turkey
turkstat_link <- "http://www.turkstat.gov.tr/PreIstatistikTablo.do?istab_id=1601"
download.file(turkstat_link, "data-raw/turkstat.xls")

turkstat <- read_excel("data-raw/turkstat.xls", skip = 5) 
names(turkstat)[1] <- "var" 
turkstat <- turkstat %>% 
  filter(str_detect(var, "Gini")) %>% 
  gather(key = year, value = gini) %>% 
  filter(year!="var") %>% 
  transmute(country = "Turkey",
            year = as.numeric(year),
            gini = as.numeric(gini),
            gini_se = NA,
            welfare_def = "net",
            equiv_scale = "oecdm",
            monetary = FALSE,
            series = paste("Turkstat", welfare_def, equiv_scale),
            source1 = "Turkish Statistical Institute",
            page = "",
            link = turkstat_link) 

# U.K. Office for National Statistics
ons_link <- "http://www.ons.gov.uk/generator?uri=/peoplepopulationandcommunity/personalandhouseholdfinances/incomeandwealth/bulletins/theeffectsoftaxesandbenefitsonhouseholdincome/financialyearending2015/e93b0b06&format=csv"
download.file(ons_link, "data-raw/ons.csv")

ons <- read_csv("data-raw/ons.csv", skip = 6) %>% 
  transmute(year = Year,
            market = `Equivalised original income`,
            gross = `Equivalised gross income`,
            net = `Equivalised disposable income`) %>% 
  gather(key = welfare_def, value = gini, -year) %>% 
  transmute(country = "United Kingdom",
            year = ifelse(str_extract(year, "\\d{2}$") %>% as.numeric() > 50,
                          str_extract(year, "\\d{2}$") %>% as.numeric() + 1900,
                          str_extract(year, "\\d{2}$") %>% as.numeric() + 2000),
            gini = gini/100,
            gini_se = NA,
            welfare_def = welfare_def,
            equiv_scale = "oecdm",
            monetary = FALSE,
            series = paste("ONS", welfare_def, equiv_scale),
            source1 = "UK Office for National Statistics",
            page = "",
            link = ons_link)  

# U.K. Institute for Fiscal Studies
ifs_link <- "http://www.ifs.org.uk/uploads/publications/bns/bn19figs_update2015.xlsx"
download.file(ifs_link, "data-raw/ifs.xlsx")

ifs <- read_excel("data-raw/ifs.xlsx", sheet = 5, col_names = FALSE, skip = 3) %>%
  select(X1, X2, X3) %>%
  filter(!is.na(X3)) %>% 
  transmute(country = "United Kingdom",
            year = ifelse(str_extract(X1, "\\d{2}$") %>% as.numeric() > 50,
                   str_extract(X1, "\\d{2}$") %>% as.numeric() + 1900,
                   str_extract(X1, "\\d{2}$") %>% as.numeric() + 2000),
            gini = X3,
            gini_se = NA,
            welfare_def = "net",
            equiv_scale = "oecdm",
            monetary = TRUE,
            series = paste("IFS", X2, welfare_def, equiv_scale),
            source1 = "Institute for Fiscal Studies",
            page = "",
            link = ifs_link)

# U.S. Congressional Budget Office
cbo_link <- "https://www.cbo.gov/sites/default/files/114th-congress-2015-2016/reports/51361-SupplementalData.xlsx"
download.file(cbo_link, "data-raw/cbo.xlsx")

cbo <- read_excel("data-raw/cbo.xlsx", sheet = 9, col_names = FALSE, skip = 10) %>% 
  select(X0:X3) %>% 
  filter(!is.na(X1)) %>% 
  transmute(year = as.numeric(X0),
            market = X1,
            gross = X2,
            net = X3) %>% 
  gather(key = "welfare_def", 
         value = "gini",
         market:net) %>% 
  mutate(country = "United States",
         gini_se = NA,
         equiv_scale = "sqrt",
         monetary = TRUE,
         series = paste("CBO", welfare_def, "sqrt"),
         source1 = "U.S. Congressional Budget Office",
         page = "",
         link = cbo_link)

#U.S. Census Bureau
uscb_link <- "https://www2.census.gov/programs-surveys/demo/tables/p60/252/table4.pdf"
download.file(uscb_link, "data-raw/uscb.pdf")

uscb <- extract_tables("data-raw/uscb.pdf") %>% 
  discard(function(x) dim(x)[2]==1) %>%
  map_df(function(x) {
    years <- x[2, ] %>% str_extract("\\d{4}")
    x1 <- x[str_detect(x[ , 1], "Gini"), ] %>%
      t() %>% 
      as_data_frame() %>% 
      mutate(year = years) %>% 
      filter(!is.na(year)) %>% 
      transmute(year = as.numeric(year),
                gini = str_replace_all(V1, fixed(" "), "") %>% as.numeric(),
                gini_se = str_replace_all(V2, fixed(" "), "") %>% as.numeric(),
                break_yr = (year == 1993 | (year == 2013 & gini_se > .003)))
    return(x1)}) %>%
  arrange(year, break_yr) %>% 
  transmute(country = "United States",
            year = year,
            gini = gini,
            gini_se = gini_se,
            welfare_def = "gross",
            equiv_scale = "hh",
            monetary = TRUE,
            series = paste("US Census Bureau", welfare_def, equiv_scale, cumsum(break_yr) + 1),
            source1 = "U.S. Census Bureau",
            page = "",
            link = uscb_link)

# Uruguay Instituto Nacional de Estadística
uine_link <- "http://www.ine.gub.uy/documents/10181/364159/Estimación+de+la+pobreza+por+el+Método+del+Ingreso+2015/321a0edb-d97e-4ab0-aa88-e31ce7a22307"
download.file(uine_link, "data-raw/uine.pdf")

uine <- extract_tables("data-raw/uine.pdf", pages = 44)[[2]][4:13, 1:2] %>% 
  as_data_frame() %>% 
  transmute(country = "Uruguay",
            year = V1 %>% str_trim() %>% as.numeric(),
            gini = as.numeric(sub(",", ".", V2, fixed = TRUE)),
            gini_se = NA,
            welfare_def = "net",
            equiv_scale = "pc",
            monetary = TRUE,
            series = paste("Instituto Nacional de Estadistica", welfare_def, equiv_scale),
            source1 = "Instituto Nacional de Estadistica",
            page = "42",
            link = uine_link)


## Added data
added_data <- read_csv("data-raw/article_data/fs_added_data.csv")

## Combine
# first, get baseline series and order by data-richness
baseline_series <- "LIS net sqrt"
baseline_wd <- "net"
baseline_es <- "sqrt"
baseline <- lis %>% filter(welfare_def==baseline_wd & equiv_scale==baseline_es) %>% 
  rename(gini_b = gini,
         gini_b_se = gini_se) %>%
  group_by(country) %>% 
  mutate(lis_count = n()) %>% 
  ungroup() %>% 
  arrange(desc(lis_count)) 

# turn cross-country series that do not have baseline's welfare_def into within-country series
oecd1 <- oecd %>% 
  mutate(series = ifelse(welfare_def!=str_extract(baseline_series, "market|net"),
                         paste("OECD", country, str_replace(series, "OECD ", "")),
                         series))
ceq1 <- ceq %>% 
  mutate(series = ifelse(welfare_def!=str_extract(baseline_series, "market|net"),
                         paste("CEQ", country, str_replace(series, "CEQ ", "")),
                         series))

# then combine with other series ordered by data-richness
ineq0 <- bind_rows(lis, 
                   sedlac, cepal, cepal_sdi, oecd1, eurostat,
                   transmonee, ceq1, afr_gini,
                   abs, statcan, statee, statfi, geostat,
                   ssb, dgeec, rosstat, scb, tdgbas, turkstat, 
                   ons, ifs, cbo, uscb, uine,
                   added_data) %>% 
  rename(gini_m = gini,
         gini_m_se = gini_se) %>%
  group_by(country) %>% 
  mutate(oth_count = n()) %>% 
  ungroup() %>% 
  group_by(country, series) %>% 
  mutate(s_count = n()) %>%
  ungroup() %>% 
  arrange(desc(oth_count), desc(s_count))  
  
# obs with baseline data
ineq_bl <- ineq0 %>% 
  right_join(baseline %>% 
               select(country, year, gini_b, gini_b_se, lis_count),
             by = c("country", "year")) %>% 
  arrange(desc(lis_count)) %>% 
  select(-lis_count)

# obs with no baseline data [from series with some baseline data?]
ineq_nbl <- ineq0 %>% anti_join(ineq_bl %>% select(-gini_b, -gini_b_se), 
             by = c("country", "year"))

# obs from series with no baseline data


# combine all
ineq <- bind_rows(ineq_bl, ineq_nbl) %>% 
  mutate(gini_m_se = ifelse(!is.na(gini_m_se), gini_m_se,
                            quantile(gini_m_se/gini_m, .99, na.rm = TRUE)*gini_m),
         ccode = as.integer(factor(country, levels = unique(country))),
         ycode = as.integer(year - min(year) + 1),
         wcode = as.integer(factor(welfare_def), levels = unique(welfare_def)),
         ecode = as.integer(factor(equiv_scale), levels = unique(equiv_scale)),
         scode = as.integer(factor(series, levels = unique(series))))

save(ineq, file = "data/ineq.Rda")

# for v5.1
ineq1 <- ineq %>%
  transmute(country = country,
            year = year,
            gini = gini_m*100,
            incdefn = welfare_def,
            equivsc = equiv_scale,
            source1 = source1,
            link = link)
write_csv(ineq1, "../Global Inequality/ SWIID v5.1/Data/ionso.csv")

g_1_se <- ineq %>% 
  filter(source1=="LISSY" & equiv_scale=="sqrt" & 
           (welfare_def=="net" | welfare_def=="market")) %>% 
  select(country, year, gini_m_se, welfare_def) %>% 
  mutate(gini_m_se = gini_m_se*100) %>% 
  spread(key = welfare_def, value = gini_m_se) %>% 
  rename(g_1se = net, g_2se = market)
write_csv(g_1_se, "../Global Inequality/ SWIID v5.1/Data/g_1_se.csv", na = "")
  

# Should flag series that *only* share obs with baseline?
# weird: Finland, Italy -- in year w missing, est goes to 50 
# not enough data: China, DR, Egypt

##
# ineq_l <- bind_rows(lis, 
#           sedlac, cepal, oecd, eurostat, ceq, abs, statcan, ifs, cbo)
# 
# ineq_w <- ineq_l %>% spread(key = series, value = gini)


# t <- ineq %>%
#   select(country, year, gini_m, scode) %>%
#   mutate(scode = paste0("s", scode)) %>%
#   split(.$country) 
# %>% 
#   map(function(x) {
#     spread(., key = scode, value = gini_m) %>% 
#       select(-country, -year) %>%
#       cor(use = "pairwise.complete.obs")})
