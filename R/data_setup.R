if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, readxl, 
       eurostat, rsdmx, xml2, CANSIM2R, pxweb, rvest,
       stringr, magrittr, countrycode, pdftools)
p_load_gh("ropensci/tabulizerjars", "ropensci/tabulizer") # read PDF tables
p_load_gh("ropengov/dkstat")

# Custom country codes (defined in R/cc_swiid.R)
load("data/cc_swiid.rda")
body(countrycode)[[7]] <- substitute(
  if (is.null(dictionary) | as.list(match.call())[["custom_dict"]] == "cc_swiid") {
    if (origin %in% default_regex_codes) {
      origin <- paste0(origin, ".regex")
      origin_regex <- TRUE
    }
    else {
      origin_regex <- FALSE
    }
  }
)


# LIS
format_lis <- function(x) {
  paste0("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/LISSY/", 
         x, ".txt") %>%
    readLines() %>% 
    str_subset("^\\D{2}\\d{2}h,.*") %>%
    paste(collapse = "\n") %>% 
    read_csv(col_names = FALSE) %>%
    transmute(country = str_extract(X1, "\\D{2}") %>%
                toupper() %>% 
                str_replace("UK", "GB") %>% 
                countrycode("iso2c", "swiid.name", custom_dict = cc_swiid),
              year = ifelse(str_extract(X1, "\\d{2}") %>% as.numeric() > 50,
                            str_extract(X1, "\\d{2}") %>% as.numeric() + 1900,
                            str_extract(X1, "\\d{2}") %>% as.numeric() + 2000),
              gini = (str_trim(X3) %>% as.numeric()),
              gini_se = (str_trim(X4) %>% as.numeric()),
              welfare_def = str_extract(X2, "[^_]*"),
              equiv_scale = str_extract(X2, "(?<=_).*"),
              monetary = FALSE,
              series = paste("LIS", welfare_def, equiv_scale),
              source1 = "LISSY",
              page = "",
              link = paste0("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/LISSY/",
                            x, ".txt")) %>% 
    filter(!gini == 0) %>% 
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
                countrycode("iso2c", "swiid.name", custom_dict = cc_swiid),
              year = ifelse(str_extract(X1, "\\d{2}") %>% as.numeric() > 66,
                            str_extract(X1, "\\d{2}") %>% as.numeric() + 1900,
                            str_extract(X1, "\\d{2}") %>% as.numeric() + 2000),
              gini = (str_trim(X2) %>% as.numeric()),
              gini_se = (str_trim(X3) %>% as.numeric()),
              equiv_scale = "sqrt",
              welfare_def = "disp",
              monetary = FALSE,
              series = "LIS disp sqrt",
              source1 = ifelse(country=="New Zealand", "Statistics New Zealand 1999", "LISSY"),
              page = ifelse(country=="New Zealand", "73", ""),
              link = ifelse(country=="New Zealand", 
                            "http://www2.stats.govt.nz/domino/external/PASFull/pasfull.nsf/173371ce38d7627b4c25680900046f25/4c2567ef00247c6acc256b03000bdbe0/$FILE/Incomes.pdf", 
                            "https://web.archive.org/web/20100804001129/http://www.lisproject.org/key-figures/kf-workbook.xls")) %>% 
    arrange(country, year)
}

lis_files <- c("au", "at", "be", "br", "ca", "cn", "co", "cz", "dk",   # add "cl" when LIS releases data
               "do", "eg", "ee", "fi", "fr", "de", "ge", "gr", "gt", "hu", "is", 
               "in", "ie", "il", "it", "jp", "lu", "mx", "nl", "no", "pa", "py", 
               "pe", "pl", "ro", "ru", "rs", "sk", "si", "za", "kr", "es", "se", 
               "ch", "tw", "uk", "us", "uy")

lis <- lis_files %>% 
  map_df(format_lis) %>% 
  filter(!country=="Russia") %>% 
  rbind(format_lis_xtra("nz"), format_lis_xtra("ru_old")) %>% 
  arrange(country, year, welfare_def, equiv_scale)


# Socio-Economic Database for Latin America and the Caribbean (SEDLAC) (automated)
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
           series = paste("SEDLAC", country, "disp", es,
                   as.numeric(factor(series0, levels = unique(series0)))) %>% 
             str_replace("NA", "1")) %>% 
    ungroup() %>% 
    filter(!is.na(gini)) %>%
    transmute(country = country,
              year = as.numeric(year),
              gini = as.numeric(gini),
              gini_se = se,
              welfare_def = "disp",
              equiv_scale = es,
              monetary = TRUE,
              series = series,
              source1 = "SEDLAC",
              page = sheet, 
              link = link)
  return(x)
}

sedlac <- "http://sedlac.econo.unlp.edu.ar/eng/statistics.php" %>% 
  html_session() %>% 
  follow_link("Inequality") %>% 
  follow_link("Inequality")
writeBin(httr::content(sedlac$response, "raw"), "data-raw/sedlac.xls")
sedlac_link <- sedlac$response$url

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


# CEPALStat (automated)
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
  filter(!name=="Latin America (simple average)") %>% 
  mutate(country = countrycode(as.character(name), "country.name", "swiid.name", custom_dict = cc_swiid)) %>% 
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
            welfare_def = "disp",
            equiv_scale = "pc",
            monetary = TRUE,
            notes = paste(area, ifelse(is.na(descripcion), "", as.character(descripcion))),
            series = paste("CEPAL", country, "disp pc", as.numeric(factor(notes, levels = unique(notes)))),
            source1 = "CEPALStat",
            page = area,
            link = cepal_link) %>% 
  ungroup() %>% 
  select(-notes)

rm(cepal0, cepal_raw, cepal_labels, cepal_notes)


# CEPAL Serie Distribución del Ingreso (archived)
cepal_sdi <- read_tsv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/repositorio_cepal.tsv",
                      col_types = "ciiccccccc") %>% 
  transmute(country = country,
            year = year,
            gini = gini/100,
            gini_se = NA,
            monetary = str_detect(welfare_def, "Monetary"),
            welfare_def = ifelse(str_detect(welfare_def, "Disposable"), "disp",
                                 ifelse(str_detect(welfare_def, "Gross"), "gross",
                                        "market")),
            equiv_scale = equiv_scale,
            series = paste("CEPAL SDI", country, welfare_def, equiv_scale, survey),
            source1 = source,
            page = NA,
            link = link)


# OECD Income Distribution Database (automated)
oecd_link <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/IDD/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+RUS.GINI+STDG+GINIB+GINIG.TOT.CURRENT+PREVIOUS+INCOMPARABLE.METH2012+METH2011/all"
oecd0 <- oecd_link %>% 
  readSDMX() %>% 
  as.data.frame() %>% 
  transmute(country = countrycode(LOCATION, "iso3c", "swiid.name", custom_dict = cc_swiid),
         year = as.numeric(obsTime),
         gini = obsValue,
         welfare_def = ifelse((MEASURE=="GINI" | MEASURE=="STDG"), "disp", 
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

       
# Eurostat (automated)
eurostat <- get_eurostat("ilc_di12", 
                         time_format = "num", 
                         update_cache = TRUE,
                         keepFlags = TRUE) %>%
  mutate(geo = as.character(geo) %>% recode("UK" = "GB", "EL" = "GR")) %>% 
  filter(!str_detect(geo, "E[AU]\\d*|NMS10")) %>% 
  transmute(country = countrycode(as.character(geo), "iso2c", "swiid.name", custom_dict = cc_swiid),
         year = time - (!(country=="United Kingdom" | country=="Ireland")), #eurostat reports survey year not ref year except in UK and IE <http://ec.europa.eu/eurostat/cache/metadata/en/ilc_esms.htm#ref_period>
         gini = values/100,
         gini_se = NA,
         welfare_def = "disp",
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


# Transmonee 2012 (2012 is the last database that includes inequality data; archived)
transmonee_link <- "https://web.archive.org/web/20130401075747/http://www.transmonee.org/Downloads/EN/2012/TransMonEE_2012.xls"
download.file(transmonee_link, "data-raw/transmonee.xls")

transmonee <- read_excel("data-raw/transmonee.xls", 
                                          sheet = "10. Economy",
                                          skip = 398,
                                          na = "-")[1:34, ] %>% 
  filter(!is.na(X__1)) %>% 
  select(-X__2) %>% 
  gather(key = year, value = gini, -X__1) %>% 
  filter(!is.na(gini)) %>% 
  transmute(country = countrycode(X__1, "country.name", "swiid.name", custom_dict = cc_swiid),
            year = as.numeric(year),
            gini = gini,
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("Transmonee", country, welfare_def, equiv_scale),
            source1 = "Transmonee 2012",
            page = "",
            link = transmonee_link)

# Commitment to Equity (update by hand; see http://www.commitmentoequity.org/publications-ceqworkingpapers/ and http://www.commitmentoequity.org/data/ )
ceq <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/ceq.csv", col_types = "cnnncclcccc") %>% 
  mutate(series = paste("CEQ", welfare_def, equiv_scale))

# World Bank Africa Poverty Database (carefully vetted subset of WDI; automated/archived)
afr_wb3c <- countrycode::countrycode_data %>% 
  filter(continent=="Africa" & !is.na(wb_api3c)) %>% 
  `[[`("wb_api3c") %>% 
  setdiff(c("DZA", "DJI", "EGY", "LBY", "MAR", "TUN"))

afr_gini_pc <- wbstats::wb(indicator = "SI.POV.GINI",
               startdate = 1960, 
               enddate = 2016, 
               country = afr_wb3c) %>% 
  transmute(country = countrycode(country, origin = "country.name", "swiid.name", custom_dict = cc_swiid),
            year = as.numeric(date),
            gini = value/100,
            gini_se = NA,
            welfare_def = "con",
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("World Bank Africa Poverty Database", country, welfare_def, equiv_scale),
            source1 = "Beegle et al. 2016",
            page = "121-126",
            link = "http://databank.worldbank.org/data/reports.aspx?source=2&series=SI.POV.GINI")

afr_gini_sqrt <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/AFR_gini_sqrt.csv", 
                          col_types = "cid") %>% 
  transmute(country = countrycode(country, origin = "wb_api3c", "swiid.name", custom_dict = cc_swiid),
            year = as.numeric(surveyr),
            gini = gini_sqrthhs/100,
            gini_se = NA,
            welfare_def = "con",
            equiv_scale = "sqrt",
            monetary = FALSE,
            series = paste("World Bank Africa Poverty Database", country, welfare_def, equiv_scale),
            source1 = "Personal communication, K. Beegle, 2016-08-01",
            page = "",
            link = "https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/AFR_gini_sqrt.csv")

afr_gini <- bind_rows(afr_gini_pc, afr_gini_sqrt)
rm(afr_gini_pc, afr_gini_sqrt)


## National Statistics Offices

first_row_to_names <- function(x) {
  names(x) <- x[1, ]
  names(x)[which(names(x) == "" | is.na(names(x)))] <- paste0("v", 1:length(which(names(x) == "" | is.na(names(x)))))
  x <- x[-1, ]
  return(x)
}

# National Statistical Service of Armenia (automated)
armstat_page <- "http://www.armstat.am/en/?nid=81&pthid=pov&year="
arm_page <- armstat_page %>% 
  read_html() %>% 
  html_nodes("h4 a")

get_access_link <- function(link) {
  access <- map(link, function(link) {
    t <- read_html(link) %>% 
      html_nodes("tr") 
    t2 <- t[str_detect(t %>% html_text(), "Accessibility|ACCESSIBILITY")] %>% 
      html_node("a") %>% 
      html_attr("href") %>% 
      str_replace("\\.\\.", "http://www.armstat.am") 
    return(t2)
  }) %>% 
    unlist()
  return(access)
}

arm_reports <- tibble(title = arm_page %>% html_text() %>% tolower(),
                      link1 = arm_page %>% html_attr("href")) %>% 
  filter(str_detect(title, "december") & !str_detect(title, "armenian")) %>% 
  mutate(link1 = str_replace(link1, "\\.", "http://www.armstat.am/en"),
         link2 = get_access_link(link1),
         year = str_extract(title, "\\d{4}"))

get_arm_ginis <- function(link) {
  file_yr <- str_extract(link, "\\d{2,4}") %>% 
    if_else(str_length(.) == 2, str_c("20", .), .)
  file_name <- paste0("data-raw/armstat", file_yr, ".pdf")
  if (!file.exists(file_name)) download.file(link, file_name)
  ginis <- extract_tables(file_name, pages = 11)[[1]] %>% 
    as_tibble()
  if (!any(str_detect(ginis$V1, "Gini"))) {
    ginis <- extract_tables(file_name, pages = 12)[[1]]
  }
  ginis1 <- ginis %>% 
    first_row_to_names() %>% 
    mutate(v1 = replace(v1, v1=="", NA_character_)) %>% 
    fill(v1, .direction = "up") %>% 
    filter(str_detect(v1, "inequality")) %>% 
    gather(key = year, value = gini, -v1) %>% 
    filter(!gini=="") %>% 
    mutate(report = as.numeric(file_yr),
           link = link)
  return(ginis1)
}

armstat <- arm_reports %>% 
  filter(year >= 2016) %>% 
  `[[`("link2") %>% 
  map_df(., ~ get_arm_ginis(.x)) %>% 
  arrange(desc(report), desc(year), v1) %>% 
  distinct(year, v1, .keep_all = TRUE) %>% 
  transmute(country = "Armenia",
            year = as.numeric(year),
            gini = as.numeric(gini),
            gini_se = NA,
            welfare_def = if_else(str_detect(v1, "consumption"), "con", "gross"),
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("NSS Armenia", welfare_def, equiv_scale),
            source1 = "National Statistical Service of Armenia",
            page = "11",
            link = link)

rm(arm_page, arm_reports)


# Australian Bureau of Statistics (update abs_link; not included in abs api as of 2017-05)
# confirm latest release at: http://www.abs.gov.au/AUSSTATS/abs@.nsf/second+level+view?ReadForm&prodno=6523.0&viewtitle=Household%20Income%20and%20Wealth,%20Australia~2013-14~Latest~04/09/2015&&tabname=Past%20Future%20Issues&prodno=6523.0&issue=2013-14&num=&view=&

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
       
abs_de <- abs_format("Table 1.1", "disp", "oecdm")
abs_gh <- abs_format("Table 1.2", "gross", "hh")
abs <- bind_rows(abs_de, abs_gh)

rm(abs_de, abs_gh)


# Instituto Naciónal de Estadística de Bolivia (update file)
# http://www.ine.gob.bo > Estadísticas Sociales > Pobreza > Linea de Pobreza > Cuadros Estadísticos >
#   Indicadores de Distribución del Ingreso > select years > Generar > Exportar a Excel

inebo <- read_excel("data-raw/inebo.xlsx", skip = 6) %>% 
  filter(DESCRIPCION == "Bolivia") %>% 
  select(-DESCRIPCION) %>% 
  gather(key = year, value = gini) %>% 
  transmute(country = "Bolivia",
            year = as.numeric(year),
            gini = gini,
            gini_se = NA,
            welfare_def = "gross",
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("INE Bolivia", welfare_def, equiv_scale),
            source1 = "Instituto Naciónal de Estadística de Bolivia",
            page = "",
            link = "http://www.ine.gob.bo/index.php/2016-08-10-15-59-03/introduccion-2")


# Belarus National Statistical Committee (automated, but will probably need to update wrangle)
belstat_page <- "http://www.belstat.gov.by/en/ofitsialnaya-statistika/social-sector/uroven-zhizni-naseleniya/publikatsii__1/"
belstat_zip <- html_session(belstat_page) %>% 
  follow_link("Social Conditions and Standard of Living") %>% 
  follow_link("Download")
belstat_link <- belstat_zip$back[1]
belstat_temp <- tempfile(fileext = ".zip")
writeBin(belstat_zip$response$content, belstat_temp)
belstat_dir <- file.path(tempdir(), "belstat")
unzip(belstat_temp, exdir = belstat_dir) 
belstat_file <- list.files(belstat_dir) %>% 
  str_subset(".pdf") %>% 
  file.path(belstat_dir, .)
file.rename(belstat_file, "data-raw/belstat.pdf")
unlink(c(belstat_temp, belstat_dir), recursive = TRUE)
rm(belstat_zip)

belstat <- extract_tables("data-raw/belstat.pdf", pages = 76)[[1]] %>%
  as_data_frame() %>% 
  filter(V1 == ""| V1 == "concentration)") %>% 
  select(-V1) %>% 
  first_row_to_names() %>% 
  gather(key = year, value = gini) %>% 
  transmute(country = "Belarus",
            year = as.numeric(year),
            gini = as.numeric(gini),
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("Belstat", welfare_def, equiv_scale),
            source1 = "Belarus National Committee of Statistics",
            page = "",
            link = belstat_link)


# Statistics Canada (automated)
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
              str_replace("after-tax", "disp") %>% 
              str_replace("total", "gross"),
            equiv_scale = equiv_scale,
            monetary = TRUE,
            series = paste("Statistics Canada", welfare_def, equiv_scale),
            source1 = "Statistics Canada",
            page = "",
            link = link)


# DANE Colombia (automated)
dane_file <- "http://www.dane.gov.co/index.php/estadisticas-por-tema/pobreza-y-condiciones-de-vida/pobreza-y-desigualdad/" %>% 
  html_session() %>% 
  follow_link("Pobreza Monetaria") %>%
  follow_link("Anexos")
dane_link <- dane_file$url
writeBin(dane_file$response$content, "data-raw/dane.xls")
rm(dane_file)

dane <- read_excel("data-raw/dane.xls", sheet = "Gini", skip = 9) %>% 
  filter(Dominio == "Nacional") %>% 
  gather(key = year, value = gini, -Dominio) %>% 
  transmute(country = "Colombia",
            year = as.numeric(year),
            gini = gini,
            gini_se = NA,
            welfare_def = "gross",
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("DANE", welfare_def, equiv_scale),
            source1 = "DANE",
            page = "",
            link = dane_link)


# Costa Rica (update file; as of 2017-05 page not sufficiently responsive to automate)
# http://www.inec.go.cr/pobreza-y-desigualdad/desigualdad
ineccr_link <- "http://www.inec.go.cr/sites/default/files/documetos-biblioteca-virtual/repobrezaenaho2010-2016.01.xlsx"

ineccr <- read_excel("data-raw/ineccr.xlsx", skip = 5) %>%
  janitor::clean_names() %>% 
  select(año, total) %>% 
  filter(!is.na(año)) %>% 
  mutate(es = if_else(cumsum(str_detect(año, "por persona")) == 1, "pc", "hh")) %>% 
  filter(str_detect(año, "^\\d{4}$")) %>% 
  transmute(country = "Costa Rica",
            year = as.numeric(año),
            gini = total,
            gini_se = NA,
            welfare_def = "gross",
            equiv_scale = es,
            monetary = NA,
            series = paste("INEC", welfare_def, equiv_scale),
            source1 = "Instituto Naciónal de Estadística y Censos de Costa Rica",
            page = "",
            link = ineccr_link)


# Statistics Denmark (automated)
dkstat <- dst_get_data(table = "IFOR41", 
                       ULLIG = "Gini coefficient", 
                       KOMMUNEDK = "All Denmark",
                       Tid = "*",
                       lang = "en") %>% 
  transmute(country = "Denmark",
            year = lubridate::year(TID),
            gini = value/100,
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = "oecdm",
            monetary = FALSE,
            series = paste("Statistics Denmark", welfare_def, equiv_scale),
            source1 = "Statistics Denmark",
            page = "",
            link = "http://www.statbank.dk/IFOR41") 


# CAPMAS Egypt (archived)
capmas_link <- "http://www.msrintranet.capmas.gov.eg/pdf/studies/inds/EG-LIV-E-I.xls" # this file isn't updated anymore
download.file(capmas_link, "data-raw/capmas.xls") 

capmas <- read_excel("data-raw/capmas.xls", skip = 7) %>% 
  filter(Indicator == "Inequality of income or expenditure distribution (Gini coefficient), Total") %>% 
  select(contains("20")) %>% 
  gather(key = year, value = gini) %>% 
  filter(!is.na(gini)) %>% 
  transmute(country = "Egypt",
            year = as.numeric(year),
            gini = gini,
            gini_se = NA,
            welfare_def = "con",
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("CAPMAS", welfare_def, equiv_scale),
            source1 = "CAPMAS",
            page = "",
            link = capmas_link) 


# Statistics Estonia (archived)
statee <- read_tsv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/statistics_estonia.tsv", 
                   col_names = FALSE,
                   col_types = "idd") %>% 
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


# Statistics Finland (automated)
statfi <- get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/tul/tjt/270_tjt_tau_117.px",
                         dims = list(Tulokäsite = c("SL2", "4L2", "6L2"),
                                     Vuosi = c("*"),
                                     Tiedot = c("Gini")),
                         clean = TRUE) %>% 
  transmute(country = "Finland",
            year = as.numeric(as.character(Year)),
            gini = values/100,
            gini_se = NA,
            welfare_def = ifelse(str_detect(`Income concept`, "Disposable"), "disp",
                                 ifelse(str_detect(`Income concept`, "Gross"), "gross",
                                        "market")),
            equiv_scale = "oecdm",
            monetary = FALSE,
            series = paste("Statistics Finland", welfare_def, equiv_scale),
            source1 = "Statistics Finland",
            page = "",
            link = "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/tul/tjt/270_tjt_tau_117.px")


# Insee France (archived)
insee_link <- "https://web-beta.archive.org/web/20151206151022/http://www.insee.fr/fr/themes/series-longues.asp?indicateur=gini-niveaux-vie"

insee <- readLines(insee_link) %>%              # kickin' it old skool . . .
  str_subset("etendue-ligne|tab-chiffre") %>% 
  str_replace(".*>([\\d,]*)<.*", "\\1") %>% 
  str_replace(",", ".") %>% 
  matrix(ncol = 2, byrow = TRUE) %>% 
  as_tibble() %>% 
  transmute(year = as.numeric(V1),
            gini = as.numeric(V2)) %>%
  filter(!is.na(gini)) %>%
  transmute(country = "France",
            year = year,
            gini = gini,
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = "oecdm",
            monetary = FALSE,
            series = paste("Insee", welfare_def, equiv_scale),
            source1 = "Insee",
            page = "",
            link = insee_link)


# Statistics Georgia (update file)
# http://91.208.144.188/Menu.aspx?rxid=c8ca81e9-2824-4c5b-a46a-c80202913531&px_db=Database&px_type=PX&px_language=en
# Social Statistics > Standard of Living, Subsistance Minimum > Gini Coefficients by Year and Indicator
# All years, "By total incomes" and "By total expenditures"

geostat <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/geostat.csv",
                    skip = 3, 
                    col_names = c("year", "gross", "con"),
                    col_types = "cdd") %>% 
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


# Statistics Hong Kong (update in 2022)
hk2016_link <- "http://www.bycensus2016.gov.hk/data/16BC_Income_Report_Key_Statistics.xlsx"
hk2011_link <- "http://www.statistics.gov.hk/pub/B11200572012XXXXB0100.pdf"
hk2006_link <- "http://www.censtatd.gov.hk/fd.jsp?file=B11200452006XXXXB0400.pdf"
download.file(hk2016_link, "data-raw/hk2016.xlsx")
download.file(hk2011_link, "data-raw/hk2011.pdf")
download.file(hk2006_link, "data-raw/hk2006.pdf")

hk2016 <- read_excel("data-raw/hk2016.xlsx", sheet = "KeyStat2", skip = 6) %>% 
  mutate(es = str_extract(X__2, ".* income")) %>% 
  fill(es) %>% 
  filter(str_detect(X__2, "\\([ac]\\)")) %>% 
  mutate(wd = if_else(X__2 == "(a)", "market", "disp"),
         `2016` = str_replace(`2016`, "\\[", "")) %>% 
  select(`2006`, `2011`, `2016`, es, wd) %>% 
    gather(key = year, value = gini, -wd, -es) %>%
    transmute(country = "Hong Kong",
              year = as.numeric(year),
              gini = as.numeric(gini),
              gini_se = NA,
              welfare_def = wd,
              equiv_scale = if_else(str_detect(es, "per capita"), "pc", "hh"),
              monetary = FALSE,
              series = paste("Statistics Hong Kong", welfare_def, equiv_scale),
              source1 = "Statistics Hong Kong 2017",
              page = "KeyStat2",
              link = hk2016_link)

hk2011 <- extract_tables("data-raw/hk2011.pdf", pages = 123)[[1]] %>% 
  as_tibble() %>% 
  mutate(wd = if_else(cumsum(str_detect(V1, "Post-tax Post-social Transfer Household Income")) == 1,
                      "disp",
                    if_else(cumsum(str_detect(V1, "Post-tax Household Income")) == 1, 
                            "posttax",
                            "market")),
         es = if_else(V1 == "合計堅尼系數", "hh", "pc")) %>% 
  filter(wd != "posttax" & str_detect(V2, "^\\d")) %>% 
  select(-V1, -V3) %>% 
  separate(V2, into = paste0("v", 1:3), sep = " ") %>% 
  first_row_to_names() %>%
  rename(wd = market, es = pc) %>% 
  gather(key = year, value = gini, -wd, -es) %>%
  filter(year == 2001) %>% 
  transmute(country = "Hong Kong",
            year = as.numeric(year),
            gini = as.numeric(gini),
            gini_se = NA,
            welfare_def = wd,
            equiv_scale = es,
            monetary = FALSE,
            series = paste("Statistics Hong Kong", welfare_def, equiv_scale),
            source1 = "Statistics Hong Kong 2012",
            page = "107",
            link = hk2011_link)

hk2006 <- extract_tables("data-raw/hk2006.pdf", pages = 105)[[1]] %>% 
  as_tibble() %>% 
  mutate(keep = cumsum(str_detect(V1, "Gini")) >= 1) %>% 
  mutate(wd = if_else(cumsum(str_detect(V1, "Post-tax Post-social Transfer Household Income")) == 1,
                      "disp",
                      if_else(cumsum(str_detect(V1, "Post-tax Household Income")) == 1, 
                              "posttax",
                              "market")),
         es = if_else(V1 == "合計堅尼系數", "hh", "pc")) %>% 
  filter(keep & wd != "posttax" & str_detect(V2, "^\\d")) %>% 
  select(-V1, - keep) %>% 
  separate(V2, into = paste0("v", 1:3), sep = " ") %>% 
  first_row_to_names() %>%
  rename(wd = market, es = pc) %>% 
  gather(key = year, value = gini, -wd, -es) %>%
  filter(year == 1996) %>% 
  transmute(country = "Hong Kong",
            year = as.numeric(year),
            gini = as.numeric(gini),
            gini_se = NA,
            welfare_def = wd,
            equiv_scale = es,
            monetary = FALSE,
            series = paste("Statistics Hong Kong", welfare_def, equiv_scale),
            source1 = "Statistics Hong Kong 2007",
            page = "107",
            link = hk2006_link)

stathk <- bind_rows(hk2006, hk2011, hk2016)

rm(hk2006, hk2011, hk2016)


# BPS Indonesia (automated)
# may need to update bpsid2_link: check https://www.bps.go.id/linkTabelStatis/view/id/946 > Download Table
# with http://wheregoes.com/retracer.php to find download link

bpsid1_link <- "https://www.bps.go.id/website/tabelExcelIndo/indo_23_6.xls"
download.file(bpsid1_link, "data-raw/bpsid1.xls")
bpsid2_link <- "https://www.bps.go.id/website/tabelExcelIndo/indo_05_21.xls"
download.file(bpsid2_link, "data-raw/bpsid2.xls")

bpsid1 <- read_excel("data-raw/bpsid1.xls", skip = 2) %>% 
  filter(Provinsi == "INDONESIA") %>% 
  select(-Provinsi) %>% 
  gather(key = year, value = gini) %>% 
  transmute(country = "Indonesia",
            year = as.numeric(year),
            gini = as.numeric(gini),
            gini_se = NA,
            welfare_def = "con",
            equiv_scale = "pc",
            monetary = NA,
            series = paste("Statistics Indonesia", welfare_def, equiv_scale),
            source1 = "Statistics Indonesia",
            page = "",
            link = bpsid1_link)

bpsid2 <- read_excel("data-raw/bpsid2.xls", skip = 2) %>% 
  fill(Daerah) %>% 
  filter(Daerah == "Kota+Desa" & !is.na(Tahun)) %>% 
  rename(year = Tahun, gini = `Indeks Gini`) %>% 
  transmute(country = "Indonesia",
            year = year,
            gini = gini,
            gini_se = NA,
            welfare_def = "con",
            equiv_scale = "pc",
            monetary = NA,
            series = paste("Statistics Indonesia", welfare_def, equiv_scale),
            source1 = "Statistics Indonesia",
            page = "",
            link = bpsid2_link) %>% 
  filter(year > 2013)   # usually prefer newer source, but bpsid1 includes 3 sig. fig.

bpsid <- bind_rows(bpsid1, bpsid2)

rm(bpsid1, bpsid2)


# Statistical Center of Iran (update link--search site with Google)
# https://www.google.com/search?hl=en&as_q=gini&as_sitesearch=https%3A%2F%2Fwww.amar.org.ir%2Fenglish

amar_link <- "https://www.amar.org.ir/english/Latest-Releases-Page/articleType/ArticleView/articleId/475"

amar <- read_html(amar_link) %>% 
  html_node(".articleEntry table") %>% 
  html_table(header = TRUE) %>% 
  filter(Description == "Gini coefficient") %>% 
  select(-Description) %>% 
  gather(key = year, value = gini) %>% 
  transmute(country = "Iran",
            year = as.numeric(year) + 621,
            gini = gini,
            gini_se = NA,
            welfare_def = "con",
            equiv_scale = "pc",
            monetary = NA,
            series = paste("Statistical Center of Iran", welfare_def, equiv_scale),
            source1 = "Statistical Center of Iran",
            page = "",
            link = amar_link)


# CSO Ireland (automated)
cso_ie_link <- "http://www.cso.ie/en/statistics/socialconditions/surveyofincomeandlivingconditionssilcmainresults/"

cso_ie0 <- read_html(cso_ie_link) %>%
  html_node("table") %>% 
  html_table(header = TRUE) 

names(cso_ie0)[1] <- "var"

cso_ie <- cso_ie0 %>% 
  filter(var == "Gini coefficient") %>% 
  select(-var) %>% 
  gather(key = year, value = gini) %>%
  transmute(country = "Ireland",
            year = as.numeric(year),
            gini = as.numeric(gini)/100,
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("CSO Ireland", welfare_def, equiv_scale),
            source1 = "CSO Ireland",
            page = "",
            link = cso_ie_link)

rm(cso_ie0)
  

# Istat (update file)
# http://dati.istat.it/Index.aspx?DataSetCode=DCCV_INDCONSUMI&Lang=en#
# Customize > Territory: Italy > Data Type: Gini

istat <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/istat.csv",
                  col_types = "ccccicdc") %>% 
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


# Kazakhstan Committee on Statistics (update link)
kazstat_page <- "http://stat.gov.kz/getImg?id=ESTAT097178"
download.file(kazstat_page, "data-raw/kazstat.xls")

kazstat <- read_excel("data-raw/kazstat.xls", skip = 3) %>% 
  filter(X__1 == "Republic of Kazakhstan") %>% 
  select(-X__1, -ends_with("_1")) %>% 
  gather(key = year, value = gini) %>% 
  transmute(country = "Kazakhstan",
            year = as.numeric(year),
            gini = gini,
            gini_se = NA,
            welfare_def = "con",
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("Kazstat", welfare_def, equiv_scale),
            source1 = "Kazakhstan Committee on Statistics",
            page = "",
            link = kazstat_page)


# Statistics Korea (update file)
# http://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1L6E001&conn_path=I2&language=en
# Item: all households; By index of distribution: Gini's; Time: all 

kostat <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/kostat.csv",
                   col_types = cols(
                     .default = col_double(),
                     `By index of distribution` = col_character(),
                     Item = col_character(),
                     `By income` = col_character()
                   )) %>%
  select(-`By index of distribution`) %>% 
  filter(!`By income`=="By income") %>% 
  gather(key = year, value = gini, -`By income`, -Item) %>% 
  filter(!is.na(gini)) %>% 
  transmute(country = "Korea",
            year = as.numeric(year),
            gini = gini,
            gini_se = NA,
            welfare_def = if_else(str_detect(`By income`, "Disposable"), "disp", "market"),
            equiv_scale = "ae",
            monetary = NA,
            series = paste("Kostat", welfare_def, equiv_scale, if_else(Item == "All households", 2, 1)),
            source1 = "Statistics Korea",
            page = "",
            link = "http://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1L6E001&conn_path=I2&language=en")


# National Bureau of Statistics Moldova (automated)
nbs_link <- "http://www.statistica.md/public/files/serii_de_timp/venituri_cheltuieli/veniturile_gospodariilor/4.2.4.xls"
download.file(nbs_link, "data-raw/nbs.xls")

nbs <- read_excel("data-raw/nbs.xls", skip = 2, sheet = "Лист1") %>%
  first_row_to_names() %>% 
  filter(str_detect(v1, "coeficientul Gini")) %>% 
  select(-v1) %>% 
  gather(key = year, value = gini) %>% 
  transmute(country = "Moldova",
            year = as.numeric(year),
            gini = gini,
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("NBS Moldova", welfare_def, equiv_scale),
            source1 = "National Bureau of Statistics of Moldova",
            page = "Лист1",
            link = nbs_link)


# National Statistical Committee of Kyrgyzstan (automated)
nsck_link <- "http://www.stat.kg/en/statistics/download/dynamic/543/"
download.file(nsck_link, "data-raw/nsck.xls")

nsck <- read_excel("data-raw/nsck.xls") %>% 
  filter(str_detect(`5.04.00.14 Additional tables- incomes`, "Items|Gini")) %>% 
  first_row_to_names() %>% 
  select(matches("\\d{4}")) %>% 
  gather(key = year, value = gini) %>% 
  filter(!is.na(gini)) %>% 
  transmute(country = "Kyrgyzstan",
            year = as.numeric(year),
            gini = gini,
            gini_se = NA,
            welfare_def = "gross",
            equiv_scale = "pc",
            monetary = TRUE,
            series = paste("NSC Kyrgyzstan", welfare_def, equiv_scale),
            source1 = "National Statistical Committee of Kyrgyzstan",
            page = "5.04.00.14",
            link = nsck_link)
  

# Statistics Office of Montenegro (automated, but update backup Internet Archive link)
# Slow server, so get from Internet Archive if it times out

get_monstat_file <- function() {
  monstat_file <- html_session("https://www.monstat.org/eng/index.php") %>% 
    follow_link("Poverty line") %>% 
    follow_link("Data")
  writeBin(httr::content(monstat_file$response, "raw"), "data-raw/monstat.xls")
  return(monstat_file$response$url)
}

monstat_link <- tryCatch(get_monstat_file(), 
  error = function(e) {
    monstat_file <- html_session("https://web.archive.org/web/20170613040141/http://www.monstat.org/eng/index.php") %>% 
      follow_link("Poverty line") %>% 
      follow_link("Data")
    writeBin(httr::content(monstat_file$response, "raw"), "data-raw/monstat.xls")
    return(monstat_file$response$url)
  })

monstat <- read_excel("data-raw/monstat.xls", skip = 1) %>%
  filter(!is.na(`Gini coefficient (%)`)) %>% 
  transmute(country = "Montenegro",
            year = as.numeric(X__1),
            gini = `Gini coefficient (%)`/100,
            gini_se = NA,
            welfare_def = "con",
            equiv_scale = "oecdm",
            monetary = NA,
            series = paste("Monstat", welfare_def, equiv_scale),
            source1 = "Statistical Office of Montenegro",
            page = "",
            link = monstat_link)


# Statistics Norway (automated)
ssb_link <- "https://www.ssb.no/en/inntekt-og-forbruk/statistikker/ifhus/aar/2016-12-16?fane=tabell&sort=nummer&tabell=288299"

ssb <- get_pxweb_data(url = "http://data.ssb.no/api/v0/en/table/if/if02/ifhus/SBMENU2486/InntUlikhet",
                       dims = list(Forbruksenhet2 = c('01'),
                                   ContentsCode = c('Ginikoeffisient', 'StandardavvikGini'),
                                   Tid = c('*')),
                       clean = TRUE) %>%
  spread(contents, values) %>% 
  transmute(country = "Norway",
            year = as.numeric(as.character(year)),
            gini = `Gini coefficient`,
            gini_se = `Standard error of the Gini coefficient`,
            welfare_def = "disp",
            equiv_scale = "oecdm",
            monetary = TRUE,
            series = paste("SSB", welfare_def, equiv_scale),
            source1 = "Statistics Norway",
            page = "",
            link = ssb_link)


# DGEEC Paraguay (update link and, probably, wrangle)
# http://www.dgeec.gov.py > Publicaciones > Pobreza

dgeec_link <- "http://www.dgeec.gov.py/Publicaciones/Biblioteca/eph2015/Boletin%20de%20pobreza%202015.pdf"
download.file(dgeec_link, "data-raw/dgeec.pdf")

dgeec <- extract_tables("data-raw/dgeec.pdf", pages = 15)[[1]][-1, ] %>%
  as_tibble() %>% 
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


# Philippines Statistical Agency (automated)
psa_link <- "https://www.psa.gov.ph/sites/default/files/Table%202.9_0.csv"
download.file(psa_link, "data-raw/psa.csv")

psa <- read_csv("data-raw/psa.csv", skip = 3, col_types = "cccdddddcc") %>% 
  first_row_to_names() %>% 
  filter(v2 == "Philippines") %>%
  select(-Region, -starts_with("v")) %>% 
  gather(key = year, value = gini) %>% 
  transmute(country = "Philippines",
            year = as.numeric(year),
            gini = as.numeric(gini),
            gini_se = NA,
            welfare_def = "gross",
            equiv_scale = "pc",
            monetary = NA,
            series = paste("PSA", welfare_def, equiv_scale),
            source1 = "Philippines Statistical Agency",
            page = "",
            link = psa_link)



# Russian Federal State Statistics Service (update link)
# http://www.gks.ru/wps/wcm/connect/rosstat_main/rosstat/en/main/
# Social and Economic Indicators of the Russian Federation (Appendix to the 'Statistical Yearbook of Russia')

rosstat_link <- "http://www.gks.ru/free_doc/doc_2016/year/pril-year_2016_eng.xls"
download.file(rosstat_link, "data-raw/rosstat.xls")

rosstat <- read_excel("data-raw/rosstat.xls", sheet = "Sec.5", skip = 1) %>% 
  filter(str_detect(INDICATORS, "Gini")) %>% 
  gather(key = year, value = gini) %>% 
  mutate_all(as.numeric) %>% 
  filter(!is.na(gini)) %>% 
  transmute(country = "Russian Federation",
            year = year,
            gini = gini,
            gini_se = NA,
            welfare_def = "gross",
            equiv_scale = "pc",
            monetary = TRUE,
            series = paste("Rosstat", welfare_def, equiv_scale),
            source1 = "Russian Federal State Statistics Service",
            page = "Sec.5",
            link = rosstat_link)


# Singapore Department of Statistics (update file)
# Note: Population covered is only resident households with at least one worker 
#  and so excludes 8-11% of resident households (which, from other data, appear
#  to be among the poorest), plus all non-resident households (surely poor).
# Note also that income definition excludes income from capital.
# These data therefore should be considered a lower bound.  Blech.
# http://www.tablebuilder.singstat.gov.sg/publicfacing/createSpecialTable.action?refId=12356
# Export > CSV

singstat <- read_csv("data-raw/singstat.csv", skip = 4, col_types = cols(
  .default = col_double(),
  `Gini Coefficient` = col_character(),
  X19 = col_character())) %>% 
  filter(!is.na(`2000`)) %>%
  select(-X19) %>% 
  gather(key = year, value = gini, -`Gini Coefficient`) %>% 
  transmute(country = "Singapore",
            year = as.numeric(year),
            gini = as.numeric(gini),
            gini_se = NA,
            welfare_def = if_else(str_detect(`Gini Coefficient`, "Taxes"), "disp", "market"),
            equiv_scale = "pc",
            monetary = TRUE,
            series = paste("Singstat", welfare_def, equiv_scale),
            source1 = "Singapore Department of Statistics",
            page = "",
            link = "http://www.tablebuilder.singstat.gov.sg/publicfacing/createSpecialTable.action?refId=12356")


# Statistics Slovenia (archived; automated)
read_px <- function(px_file_path) {
  px_vector <- readLines(px_file_path)
  px_heading <- str_subset(px_vector, "^HEADING=") %>% 
    str_replace("^HEADING=(.*);", "\\1")
  px_names <- str_subset(px_vector, paste0("^VALUES\\(",px_heading,"\\)=")) %>% 
    str_replace(paste0("^VALUES\\(",px_heading,"\\)=(.*);"), "\\1") %>% 
    str_split(",") %>% 
    first() %>% 
    str_replace_all('\\"', "")
  if (any(str_detect(px_vector, "^STUB"))) {
    px_measure <- str_subset(px_vector, "^STUB") %>% 
      str_replace('^STUB="(.*)";', "\\1")
    px_vals <- which(str_detect(px_vector, '^VALUES\\("'))
    px_vars <- px_vector[px_vals[1]:(px_vals[2]-1)] %>% 
      str_replace(paste0('^VALUES\\("', px_measure, '"\\)=(.*)[;,]'), "\\1") %>% 
      str_replace_all('\\"|,|;', "")
  } else {
    px_vars <- str_subset(px_vector, paste0('^VALUES\\("MEASURE"\\)=')) %>% 
      str_replace(paste0('^VALUES\\("', px_measure, '"\\)=(.*);'), "\\1") %>% 
      str_split(",") %>% 
      first() %>% 
      str_replace_all('\\"', "")
  }
  px_data <- px_vector[(which(str_detect(px_vector, "DATA="))+1):length(px_vector)] %>% 
    str_replace(";", "") %>%
    str_trim() %>% 
    as_tibble() %>% 
    separate(value, px_names, sep = " ") %>% 
    mutate(var_name = px_vars) %>% 
    gather(key = year, value = value, -var_name)
  return(px_data)
}

ssi1_link <- "https://www.stat.si/doc/vsebina/08/kazalniki_soc_povezanosti_Laekens_97_03.xls"
download.file(ssi1_link, "data-raw/ssi.xls", method = "curl", extra = "-k")

ssi1 <- read_excel("data-raw/ssi.xls", sheet = "Laekens kazalniki 1997-2003", skip = 4) %>% 
  janitor::clean_names() %>% 
  filter(str_detect(x_1, "Gini")) %>% 
  select(-x_1, -x_2, -x_3, -x_4) %>% 
  gather(key = year0, value = gini) %>% 
  transmute(country = "Slovenia",
            year = as.numeric(str_extract(year0, "\\d{4}")),
            gini = as.numeric(gini)/100,
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = "oecdm",
            monetary = !str_detect(year0, "_1"),
            series = paste("Statsi 2005", welfare_def, equiv_scale),
            source1 = "Slovenia Statistics Office 2005",
            page = "Laekens kazalniki 1997-2003",
            link = ssi1_link) 

ssi2_link <- "http://pxweb.stat.si/pxweb/Database/Demographics/08_level_living/08_silc_poverty_indic/15_08673_income_distribution/0867312E.px"
download.file(ssi2_link, "data-raw/ssi2.px")

ssi2 <- read_px("data-raw/ssi2.px") %>% 
  filter(str_detect(var_name, "pensions are included")) %>% 
  transmute(country = "Slovenia",
            year = as.numeric(year) - 1,
            gini = as.numeric(value)/100,
            gini_se = NA,
            welfare_def = "market",
            equiv_scale = "oecdm",
            monetary = TRUE,
            series = paste("Statsi", welfare_def, equiv_scale),
            source1 = "Slovenia Statistics Office",
            page = "",
            link = ssi2_link)

ssi <- bind_rows(ssi1, ssi2)
rm(ssi1, ssi2)


# Instituto Nacional de Estadística Spain (automated)
ine_link <- "http://www.ine.es/jaxiT3/files/t/es/csv_c/9966.csv?nocab=1"
download.file(ine_link, "data-raw/ine.csv")

ine <- read_csv("data-raw/ine.csv", skip = 4) %>% 
  filter(str_detect(X1, "con alquiler imputado")) %>% 
  gather(key = year, value = gini) %>% 
  filter(str_detect(year, "\\d{4}")) %>% 
  transmute(country = "Spain",
            year = as.numeric(year) - 1,
            gini = as.numeric(gini)/100,
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = "oecdm",
            monetary = FALSE,
            series = paste("Instituto Nacional de Estadística", welfare_def, equiv_scale),
            source1 = "Instituto Nacional de Estadística",
            page = "",
            link = ine_link)


# Statistics Sri Lanka (archived)
statslk_link <- "http://www.statistics.gov.lk/HIES/HIES2012_13FinalReport.pdf"
download.file(statslk_link, "data-raw/statslk2015.pdf")

statslk <- extract_tables("data-raw/statslk2015.pdf", pages = 22)[[1]] %>%
  as_tibble() %>% 
  filter(str_detect(V1, "Gini coefficient of household") | V1 == "") %>% 
  separate(V7, into = c("v7, v7a"), sep = " ") %>% 
  first_row_to_names() %>% 
  filter(v2 == "") %>% 
  select(-v2) %>% 
  gather(key = year, value = gini, -v1) %>% 
  filter(!gini == "-") %>% 
  transmute(country = "Sri Lanka",
            year = as.numeric(str_replace(year, "^(\\d{2}).*(\\d{2})$", "\\1\\2")),
            gini = as.numeric(gini),
            gini_se = NA,
            welfare_def = if_else(str_detect(v1, "income"), "gross", "con"),
            equiv_scale = "hh",
            monetary = NA,
            series = paste("Statistics Sri Lanka", welfare_def, equiv_scale),
            source1 = "Statistics Sri Lanka 2015",
            page = "x",
            link = statslk_link)


# Statistics Sweden (automated)
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
            welfare_def = ifelse(str_detect(inkomstslag, "disponibel"), "disp",
                                 "market"),
            equiv_scale = "ae",
            monetary = FALSE,
            series = paste("Statistics Sweden", welfare_def, equiv_scale, ifelse(year<1991, 1, 2)),
            source1 = "Statistics Sweden",
            page = "",
            link = "http://www.scb.se/en_/Finding-statistics/Statistics-by-subject-area/Household-finances/Income-and-income-distribution/Households-finances/Aktuell-Pong/7296/Income-aggregate-19752011/163550")


# Taiwan Directorate General of Budget, Accounting, and Statistics 
# update tdfbas_link [adding 1 to number after 'doc/result/' should work]
# update file from tdfbas_link2

tdgbas_link <- "http://win.dgbas.gov.tw/fies/doc/result/104/a11/Year05.xls"
download.file(tdgbas_link, "data-raw/tdgbas1.xls")

tdgbas_link2 <- "http://statdb.dgbas.gov.tw/pxweb/Dialog/varval.asp?ma=FF0004A1A&ti=Percentage%20Share%20of%20Disposable%20Income%20by%20Percentile%20Group%20of%20Households%20and%20Income%20Inequality%20Indexes-Annual&path=../PXfileE/HouseholdFinances/&lang=1&strList=L"

tdgbas <- read_excel("data-raw/tdgbas1.xls", col_names = FALSE, skip = 9) %>% 
  transmute(year = X__2,
            pc = X__4,
            sqrt = X__6) %>% 
  gather(key = equiv_scale, value = gini, -year) %>% 
  filter(!is.na(year)) %>% 
  mutate(link = tdgbas_link) %>% 
  bind_rows(read_csv("data-raw/tdgbas2.csv", 
                     col_names = c("year", "gini"), 
                     col_types = "id", 
                     skip = 4) %>% 
              filter(!is.na(year)) %>% 
              mutate(equiv_scale = "hh",
                     link = "http://statdb.dgbas.gov.tw/pxweb/Dialog/varval.asp?ma=FF0004A1A&ti=Percentage%20Share%20of%20Disposable%20Income%20by%20Percentile%20Group%20of%20Households%20and%20Income%20Inequality%20Indexes-Annual&path=../PXfileE/HouseholdFinances/&lang=1&strList=L")) %>% 
  transmute(country = "Taiwan",
            year = year,
            gini = gini,
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = equiv_scale,
            monetary = FALSE,
            series = paste("DGBAS", welfare_def, equiv_scale),
            source1 = "Taiwan Directorate General of Budget, Accounting, and Statistics",
            page = "",
            link = link)


# National Statistics Office of Thailand (archived)
nso_thailand1_link <- "https://web.archive.org/web/20101113152831/http://web.nso.go.th/eng/en/stat/socio/soctab6.htm"
nso_thailand2_link <- "https://web.archive.org/web/20100523041410/http://web.nso.go.th:80/eng/en/indicators/eco/ied-e.htm"

nso_thailand1 <- read_html(nso_thailand1_link) %>% 
  html_node(".F2 table") %>% 
  html_table(header = TRUE) %>% 
  filter(`Quintile Group` == "Gini Coefficient") %>% 
  select(-`Quintile Group`) %>% 
  gather(key = year, value = gini) %>% 
  transmute(country = "Thailand",
            year = as.numeric(str_extract(year, "\\d{4}")),
            gini = as.numeric(gini),
            gini_se = NA,
            welfare_def = "gross",
            equiv_scale = "hh",
            monetary = FALSE,
            series = paste("NSO Thailand", welfare_def, equiv_scale),
            source1 = "National Statistical Office of Thailand",
            page = "",
            link = nso_thailand1_link) %>% 
  filter(year < 1998)

nso_thailand2 <- read_html(nso_thailand2_link) %>% 
  html_node(".F2") %>% 
  html_table() %>% 
  filter(str_detect(X2, "Indicators|Gini")) %>% 
  select(-X1, -X8) %>% 
  first_row_to_names() %>% 
  gather(key = year, value = gini, -Indicators) %>% 
  transmute(country = "Thailand",
            year = as.numeric(str_extract(year, "\\d{4}")),
            gini = as.numeric(str_extract(gini, "[\\d\\.]*")),
            gini_se = NA,
            welfare_def = "gross",
            equiv_scale = if_else(str_detect(Indicators, "household"), "hh", "pc"),
            monetary = FALSE,
            series = paste("NSO Thailand", welfare_def, equiv_scale),
            source1 = "National Statistical Office of Thailand",
            page = "",
            link = nso_thailand2_link)

nso_thailand <- bind_rows(nso_thailand1, nso_thailand2)

rm(nso_thailand1, nso_thailand2)


# Statistics Turkey (automated)
turkstat_links <- paste0("http://www.turkstat.gov.tr/PreIstatistikTablo.do?istab_id=", c(1601, 2354))
download.file(turkstat_links[1], "data-raw/turkstat_oecdm.xls")
download.file(turkstat_links[2], "data-raw/turkstat_hh.xls")

turkstat_oecdm <- read_excel("data-raw/turkstat_oecdm.xls", skip = 5) 
turkstat_hh <- read_excel("data-raw/turkstat_hh.xls", skip = 5) 
turkstat_list <- list(turkstat_oecdm = turkstat_oecdm, turkstat_hh = turkstat_hh)

turkstat <- pmap_df(list(turkstat_list, names(turkstat_list), turkstat_links),
                    function(x, name_x, link_x) {
  names(x)[1] <- "var" 
  es <- str_extract(name_x, "[^_]*$")
  x %>% 
  filter(str_detect(var, "Gini")) %>% 
  gather(key = year, value = gini) %>% 
  filter(year!="var") %>% 
  transmute(country = "Turkey",
            year = as.numeric(year),
            gini = as.numeric(gini),
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = es,
            monetary = FALSE,
            series = paste("Turkstat", welfare_def, equiv_scale),
            source1 = "Turkish Statistical Institute",
            page = "",
            link = link_x) })

rm(turkstat_list, turkstat_hh, turkstat_oecdm)


# U.K. Office for National Statistics (update link)
# https://www.ons.gov.uk/atoz?query=effects+taxes+benefits (new releases in April)

ons_link <- "https://www.ons.gov.uk/generator?uri=/peoplepopulationandcommunity/personalandhouseholdfinances/incomeandwealth/bulletins/theeffectsoftaxesandbenefitsonhouseholdincome/financialyearending2016/bd6b2fe3&format=csv"
download.file(ons_link, "data-raw/ons.csv")

ons <- read_csv("data-raw/ons.csv", skip = 7, col_types = "cdddd") %>% 
  transmute(year = X1,
            market = Original,
            gross = Gross,
            disp = Disposable) %>% 
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


# U.K. Institute for Fiscal Studies (automated)
ifs <- "https://www.ifs.org.uk/tools_and_resources/incomes_in_uk" %>% 
  html_session() %>% 
  follow_link("spreadsheet")
writeBin(httr::content(ifs$response, "raw"), "data-raw/ifs.xlsx")
ifs_link <- ifs$response$url

ifs <- read_excel("data-raw/ifs.xlsx", sheet = 5, col_names = FALSE, skip = 3) %>%
  select(X__1:X__4) %>%
  filter(!is.na(X__2)) %>% 
  transmute(country = "United Kingdom",
            year = ifelse(str_extract(X__2, "\\d{2}$") %>% as.numeric() > 50,
                   str_extract(X__2, "\\d{2}$") %>% as.numeric() + 1900,
                   str_extract(X__2, "\\d{2}$") %>% as.numeric() + 2000),
            gini = as.numeric(X__4) %>% round(4),
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = "oecdm",
            monetary = TRUE,
            series = paste("IFS", X__1, welfare_def, equiv_scale),
            source1 = "Institute for Fiscal Studies",
            page = "",
            link = ifs_link)


# U.S. Congressional Budget Office (update link)
# https://www.cbo.gov/search?search=gini

cbo_link <- "https://www.cbo.gov/sites/default/files/114th-congress-2015-2016/reports/51361-SupplementalData.xlsx"
download.file(cbo_link, "data-raw/cbo.xlsx")

cbo <- read_excel("data-raw/cbo.xlsx", sheet = 9, col_names = FALSE, skip = 10) %>% 
  select(X__1:X__4) %>% 
  filter(!is.na(X__1) & !is.na(X__2)) %>% 
  transmute(year = as.numeric(X__1),
            market = as.numeric(X__2),
            gross = as.numeric(X__3),
            disp = as.numeric(X__4)) %>% 
  filter(!is.na(year)) %>% 
  gather(key = welfare_def, 
         value = gini,
         market:disp) %>% 
  mutate(country = "United States",
         gini_se = NA,
         equiv_scale = "sqrt",
         monetary = TRUE,
         series = paste("CBO", welfare_def, "sqrt"),
         source1 = "U.S. Congressional Budget Office",
         page = "",
         link = cbo_link)


# U.S. Census Bureau (update link and wrangle)
# https://www.census.gov/topics/income-poverty/income-inequality/data/data-tables.html

uscb_links <- paste0("https://www2.census.gov/programs-surveys/demo/tables/p60/256/table", c(4, "A3"), ".xls")
download.file(uscb_links[1], "data-raw/uscb_hh.xls")
download.file(uscb_links[2], "data-raw/uscb_ae.xls")

uscb_hh <- read_excel("data-raw/uscb_hh.xls", skip = 5) %>% 
  filter(str_detect(`Measures of income dispersion`, "Gini")) %>% 
  mutate(var = c("gini", "gini_se")) %>% 
  select(-`Measures of income dispersion`) %>% 
  gather(key = year, value = value, -var) %>% 
  spread(key = var, value = value) %>% 
      mutate(year = as.numeric(str_extract(year, "\\d{4}")),
                gini = as.numeric(gini),
                gini_se = as.numeric(gini_se),
                break_yr = (year == 1993 | (year == 2013 & gini_se > .003))) %>%
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
            link = uscb_links[1])

uscb_ae <- read_excel("data-raw/uscb_ae.xls", skip = 6) %>% 
  filter(str_detect(dispersion, "Gini")) %>% 
  mutate(var = c("gini", "gini_se")) %>% 
  select(-dispersion) %>% 
  gather(key = year, value = value, -var) %>% 
  spread(key = var, value = value) %>% 
  mutate(year = as.numeric(str_extract(year, "\\d{4}")),
         gini = as.numeric(gini),
         gini_se = as.numeric(gini_se),
         break_yr = (year == 1993 | (year == 2013 & gini_se > .003))) %>%
  arrange(year, break_yr) %>% 
  transmute(country = "United States",
            year = year,
            gini = gini,
            gini_se = gini_se,
            welfare_def = "gross",
            equiv_scale = "ae",
            monetary = TRUE,
            series = paste("US Census Bureau", welfare_def, equiv_scale, cumsum(break_yr) + 1),
            source1 = "U.S. Census Bureau",
            page = "",
            link = uscb_links[2])

uscb <- bind_rows(uscb_ae, uscb_hh)
rm(uscb_ae, uscb_hh)


# Uruguay Instituto Nacional de Estadística (update link and wrangle)
uine_link <- "http://www.ine.gub.uy/documents/10181/364159/Estimación+de+la+pobreza+por+el+Método+del+Ingreso+2016/4b1eabd2-ac77-48ac-95c2-fc5b92f3ade8"
download.file(uine_link, "data-raw/uine.pdf")

uine <- extract_tables("data-raw/uine.pdf", pages = 45)[[2]][5:15, 1] %>% 
  as_data_frame() %>% 
  separate(value, into = paste0("V", 1:7), sep = "\\s") %>% 
  transmute(country = "Uruguay",
            year = V1 %>% str_trim() %>% as.numeric(),
            gini = as.numeric(sub(",", ".", V2, fixed = TRUE)),
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = "pc",
            monetary = TRUE,
            series = paste("Instituto Nacional de Estadistica", welfare_def, equiv_scale),
            source1 = "Instituto Nacional de Estadistica",
            page = "43",
            link = uine_link)


# Venezuela Instituto Nacional de Estadística (update link)
inev_link <- "http://www.ine.gov.ve/documentos/Social/Pobreza/xls/Serie_%20GINI_1s1997-1s2015.xls"
download.file(inev_link, "data-raw/inev.xls")

inev <- read_excel("data-raw/inev.xls", skip = 3) %>% 
  filter(`Coeficiente Gini y Quintiles` == "Coeficiente de Gini") %>% 
  select(matches("\\d{4}")) %>% 
  gather(key = year, value = gini) %>% 
  transmute(country = "Venezuela",
            year = as.numeric(str_extract(year, "\\d{4}")),
            gini = gini,
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = "pc",
            monetary = TRUE,
            series = paste("Instituto Nacional de Estadistica", welfare_def, equiv_scale),
            source1 = "Instituto Nacional de Estadistica",
            page = "",
            link = inev_link)


# General Statistics Office of Vietnam (update link for gso_vn2)
gso_vn1_link <- "http://www.gso.gov.vn/Modules/Doc_Download.aspx?DocID=16773"
gso_vn2_link <- "http://www.gso.gov.vn/Modules/Doc_Download.aspx?DocID=19990"
download.file(gso_vn1_link, "data-raw/gso_vn2013.pdf")
download.file(gso_vn2_link, "data-raw/gso_vn.pdf") # 2016 Stats Yearbook: Health, Culture, Sport and Living Standards

gso_vn1 <- extract_tables("data-raw/gso_vn2013.pdf", pages = 84)[[1]][-1, ] %>% 
  as_tibble() %>% 
  filter(str_detect(V1, "TOTAL") | V1 == "") %>% 
  select(-V1) %>% 
  first_row_to_names() %>% 
  gather(key = year, value = gini) %>% 
  transmute(country = "Vietnam",
            year = as.numeric(str_extract(year, "\\d{4}")),
            gini = as.numeric(gini),
            gini_se = NA,
            welfare_def = "con",
            equiv_scale = "pc",
            monetary = TRUE,
            series = paste("GSO Vietnam", welfare_def, equiv_scale),
            source1 = "General Statistics Office of Vietnam 2013",
            page = "",
            link = gso_vn1_link)

gso_vn2 <- extract_tables("data-raw/gso_vn.pdf", pages = 61)[[1]] %>% 
  as_tibble() %>% 
  filter(str_detect(V1, "GENERAL") | V1 == "") %>%
  select(-V1) %>% 
  first_row_to_names() %>% 
  gather(key = year, value = gini) %>% 
  transmute(country = "Vietnam",
            year = as.numeric(str_extract(year, "\\d{4}")),
            gini = as.numeric(str_replace(gini, ",", ".")),
            gini_se = NA,
            welfare_def = "gross",
            equiv_scale = "pc",
            monetary = TRUE,
            series = paste("GSO Vietnam", welfare_def, equiv_scale),
            source1 = "General Statistics Office of Vietnam",
            page = "",
            link = gso_vn2_link)  

gso_vn <- bind_rows(gso_vn1, gso_vn2)

rm(gso_vn1, gso_vn2)

# Additional Inequality Datasets
# Chen and Ravallion 2008 (identifies welfare_def for subset of PovcalNet)
cr2008_link <- "http://siteresources.worldbank.org/JAPANINJAPANESEEXT/Resources/515497-1201490097949/080827_The_Developing_World_is_Poorer_than_we_Thought.pdf"
download.file(cr2008_link, "data-raw/ChenRavallion2008.pdf")

cr2008_codes1 <- pdftools::pdf_text("data-raw/ChenRavallion2008.pdf")[41:43] %>% 
  paste(collapse = "\\n") %>% 
  str_replace("Appendix.*\\n.*\\n.*\\n.*\\n.*\\n", "") %>%
  str_replace_all("Note:.*\\n.*\\n.*", "") %>% 
  str_replace_all(", ", ",") %>%
  str_replace("& Caribbean", "\t") %>%
  str_replace_all("(?<=\\d) *([EI])", "\t\\1") %>% 
  str_replace_all("(?<=\\n) {28,}", "\t\t") %>%
  str_replace_all(" {2,27}", "\t") %>%
  str_replace_all(" (\\d)", "\t\\1") %>%
  str_replace_all("(\\d) ([E|I])", "\\1\t\\2") %>%
  str_replace_all("(\\d),([E|I])", "\\1,\t\\2") %>%
  str_replace_all(".*\\d{2}\\.\\d\\s+", "\t") %>% 
  read_tsv(col_names = FALSE, col_types = "cccc") %>% 
  transmute(country = X2,
            years = X3,
            wd = X4) 

cr2008_codes2 <- pdftools::pdf_text("data-raw/ChenRavallion2008.pdf")[44] %>% 
  paste(collapse = "\\n") %>% 
  str_replace("Appendix.*\\n.*\\n.*\\n.*\\n.*\\n", "") %>%
  str_replace_all("Note:.*\\n.*\\n.*", "") %>% 
  str_replace_all(", ", ",") %>%
  str_replace("& Caribbean", "\t") %>%
  str_replace_all("(?<=\\d) *([EI])", "\t\\1") %>% 
  str_replace_all("(?<=\\n) {28,}", "\t\t") %>%
  str_replace_all(" {2,27}", "\t") %>%
  str_replace_all(" (\\d)", "\t\\1") %>%
  str_replace_all("(\\d) ([E|I])", "\\1\t\\2") %>%
  str_replace_all("(\\d),([E|I])", "\\1,\t\\2") %>%
  str_replace("\\t2005/2006", "\t\t2005/2006") %>% 
  str_replace_all(".*\\d{2}\\.\\d\\s+", "\t") %>% 
  read_tsv(col_names = FALSE, col_types = "ccccc") %>% 
  transmute(country = X3,
            years = X4,
            wd = X5)

expand_years <- function(years) {
  x <- NA
  for(i in 1:length(years)) {
    x[i] <- eval(parse(text = years[i])) %>% paste(collapse = ",")
  }
  return(x)
}

cr2008_codes <- bind_rows(cr2008_codes1, cr2008_codes2) %>% 
  fill(country, wd) %>% 
  filter(!str_detect(country, "^\\d{2}$")) %>% 
  filter(!str_detect(years, "^\\d{2}$")) %>%
  mutate(years = str_replace(years, ",\\s*$", ""),
         years = str_replace_all(years, "-", ":"),
         years = str_replace_all(years, "(\\d{2})\\d{2}/(\\d{2})", "\\1\\2")) %>% 
  separate(years, into = paste0("V", 1:7), sep = ",") %>% 
  gather(key = orig, value = year, V1:V7) %>% 
  filter(!is.na(year) & !year=="") %>% 
  mutate(year = str_replace(year, "\\d{2}(\\d{4})", "\\1"),
         year = str_replace(year, "^(\\d{2})$", "19\\1"),
         year = str_replace(year, "(\\d{2})(\\d{2}):(\\d{2})$", "\\1\\2:\\1\\3"),
         year = str_trim(year),
         years = expand_years(year)) %>% 
  select(-orig, -year) %>% 
  separate(years, into = paste0("V", 1:11), sep = ",") %>% 
  gather(key = orig, value = year, V1:V11) %>% 
  filter(!is.na(year) & !year=="") %>%
  mutate(country = countrycode(country, "country.name", "swiid.name", custom_dict = cc_swiid),
         year = as.numeric(year),
         wd = if_else(country == "Turkey" | country == "Bulgaria",
                      "Income", wd)) %>% 
  group_by(country, year) %>% 
  arrange(wd, .by_group = TRUE) %>% 
  slice(1) %>% 
  ungroup()

cr2008_gini <- wbstats::wb(indicator = "SI.POV.GINI",
                           startdate = 1980, 
                           enddate = 2007) %>% 
  transmute(country = countrycode(country, "country.name", "swiid.name", custom_dict = cc_swiid),
            year = as.numeric(date),
            gini = value)

cr2008 <- left_join(cr2008_codes, cr2008_gini, by = c("country", "year")) %>% 
  transmute(country = country,
            year = year,
            gini = gini/100,
            gini_se = NA,
            welfare_def = if_else(wd=="Income", "gross", "con"),
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("CR2008", country, welfare_def, equiv_scale),
            source1 = "PovcalNet; Chen and Ravallion 2008",
            page = "41-44",
            link = "http://databank.worldbank.org/data/reports.aspx?source=2&series=SI.POV.GINI") %>% 
  filter(!is.na(gini)) %>% 
  anti_join(afr_gini, by = c("country", "year")) %>% # Exclude obs already added by afr_gini
  arrange(country, year)

rm(cr2008_codes, cr2008_codes1, cr2008_codes2, cr2008_gini)


# Milanovic All the Ginis
atg <- "https://www.gc.cuny.edu/Page-Elements/Academics-Research-Centers-Initiatives/Centers-and-Institutes/Stone-Center-on-Socio-Economic-Inequality/Core-Faculty,-Team,-and-Affiliated-LIS-Scholars/Branko-Milanovic/Datasets" %>% 
  html_session() %>% 
  follow_link("Dataset")
writeBin(httr::content(atg$response, "raw"), "data-raw/atg.dta")
atg_link <- atg$response$url

atg0 <- haven::read_dta("data-raw/atg.dta") %>% 
  select(contcod, year, ends_with("_INDIE")) %>% 
  filter(!is.na(gini_INDIE)) %>% 
  mutate(country = countrycode(contcod, "wb_api3c", "swiid.name", custom_dict = cc_swiid)) %>% 
  filter(country == "Poland" | country == "United Kingdom") %>% 
  transmute(country = country,
         year = year,
         gini = gini_INDIE/100,
         gini_se = NA,
         welfare_def = if_else(Dinc_INDIE == 0, "con",
                               if_else(Dgross_INDIE == 1, "gross", "disp")),
         equiv_scale = if_else(Dhh_INDIE == 1, "hh", "pc"),
         monetary = NA,
         series = paste("AtG", country, welfare_def, equiv_scale),
         source1 = "Milanovic 2016",
         page = "",
         link = atg_link)

brandolini <- haven::read_dta("data-raw/atg.dta") %>% 
  select(contcod, year, ends_with("_INDIE")) %>% 
  filter(!is.na(gini_INDIE)) %>% 
  mutate(country = countrycode(contcod, "wb_api3c", "swiid.name", custom_dict = cc_swiid)) %>% 
  filter(country == "France" | country == "Germany" | country == "Canada" | country == "Netherlands") %>% 
  transmute(country = country,
            year = year,
            gini = gini_INDIE/100,
            gini_se = NA,
            welfare_def = if_else(Dinc_INDIE == 0, "con",
                                  if_else(Dgross_INDIE == 1, "gross", "disp")),
            equiv_scale = if_else(Dhh_INDIE == 1, "hh", "pc"),
            monetary = NA,
            series = paste("Brandolini1998", country, welfare_def, equiv_scale),
            source1 = "Milanovic 2016; Brandolini 1998",
            page = "",
            link = atg_link)

atg <- bind_rows(atg0, brandolini) %>% 
  filter(year >= 1960)

rm(atg0, brandolini)

# Global Income Distribution Database (Ackah, Bussolo, De Hoyas, and Medvedev 2008, archived)
# see http://siteresources.worldbank.org/INTPROSPECTS/Resources/334934-1225141925900/GIDDdatasetpaper.doc

gidd_link <- "https://github.com/fsolt/swiid/raw/master/data-raw/GlobalDistStata.zip"
download.file(gidd_link, "data-raw/GlobalDistStata.zip")

con <- c("Algeria", "Angola", "Bhutan", "Bosnia & Herzegovina", "Botswana", "Cape Verde",
         "Central African Rep.", "Chad", "Comoros", "Congo, Dem. Rep. Of", "Congo, Republic of",
         "Djibouti", "Egypt", "Gabon", "Guinea-Bissau", "Iran, I.R. of", "Lesotho", "Liberia",
         "Malawi", "Malaysia", "Mongolia", "Mozambique", "Namibia", "Niger", "Papua New Guinea",
         "Rwanda", "Sierra Leone", "St. Lucia", "Suriname", "Swaziland", "Togo", 
         "Trinidad and Tobago", "Tunisia", "Turkmenistan", "Zambia")

gidd_raw <- haven::read_dta(unz("data-raw/GlobalDistStata.zip", "global_dist March 12, 2009.dta")) %>% 
  filter(!countrylong == "") %>%
  filter(incsource == 1 | countrylong %in% con) %>% 
  mutate(countrylong = if_else(countrylong == "Central African Rep.",
                                  "Central African Republic",
                                  countrylong))

get_gini <- function(data, x, weight) {
  x <- data[[x]]
  weight <- data[[weight]]
  ox <- order(x)
  x <- as.vector(x)[ox]
  weight <- as.vector(weight)[ox] / sum(weight) 
  p <- cumsum(weight)
  nu <- cumsum(weight * x)
  n <- length(nu)
  nu <- nu / nu[n]
  res <- round((sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])), digits = 3)
  return(res)
}

gidd <- map_df(gidd_raw$countrylong %>% unique(), function(x) {
  gini <- get_gini(gidd_raw %>% 
                     filter(countrylong == x), "consincPPP05", "pop")
  df <- gidd_raw %>% 
    filter(countrylong == x) %>% 
    summarize(country = first(countrylong),
              year = first(year),
              gini = gini)
  
  return(df)
  }) %>% 
  transmute(country = countrycode(country, "country.name", "swiid.name", custom_dict = cc_swiid),
            year = year,
            gini = gini,
            gini_se = NA,
            welfare_def = "con",
            equiv_scale = "pc",
            monetary = NA,
            series = paste("GIDD", country, welfare_def, equiv_scale),
            source1 = "Ackah, Bussolo, De Hoyos, and Medvedev 2008",
            page = "",
            link = gidd_link)

rm(gidd_raw)


## Added data
added_data <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/article_data/fs_added_data.csv",
                       col_types = "ciddcclcccc")

## Combine
# first, get baseline series and order by data-richness
baseline_series <- "LIS disp sqrt"
baseline_wd <- "disp"
baseline_es <- "sqrt"
baseline <- lis %>% 
  filter(welfare_def==baseline_wd & equiv_scale==baseline_es) %>% 
  rename(gini_b = gini,
         gini_b_se = gini_se) %>%
  group_by(country) %>% 
  mutate(lis_count = n()) %>% 
  ungroup() %>% 
  arrange(desc(lis_count)) 

# turn cross-country series that do not have baseline's welfare_def and equiv_scale
# into within-country series
oecd1 <- oecd %>% 
  mutate(series = ifelse(welfare_def!=str_extract(baseline_series, "market|disp") |
                           equiv_scale!=str_extract(baseline_series, "\\w*$"),
                         paste("OECD", country, str_replace(series, "OECD ", "")),
                         series))
ceq1 <- ceq %>% 
  mutate(series = ifelse(welfare_def!=str_extract(baseline_series, "market|disp") |
                           equiv_scale!=str_extract(baseline_series, "\\w*$"),
                         paste("CEQ", country, str_replace(series, "CEQ ", "")),
                         series))

# then combine with other series ordered by data-richness
ineq0 <- bind_rows(lis, 
                   sedlac, cepal, cepal_sdi, oecd1, eurostat,
                   transmonee, ceq1, afr_gini,
                   abs, inebo, belstat, statcan, dane, ineccr, dkstat,
                   capmas, statee, statfi, insee, geostat,
                   stathk, bpsid, amar, cso_ie, istat, kazstat, kostat, nsck,
                   nbs, monstat, ssb, dgeec, psa,
                   rosstat, singstat, ssi, ine, statslk, scb, 
                   nso_thailand, tdgbas, turkstat, ons, ifs, cbo, uscb, uine, inev, gso_vn,
                   cr2008, atg, gidd,
                   added_data) %>% 
  rename(gini_m = gini,
         gini_m_se = gini_se) %>%
  mutate(country = countrycode(country, "country.name", "swiid.name", custom_dict = cc_swiid)) %>% 
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
         kcode = as.integer(factor(country, levels = unique(country))),
         tcode = as.integer(year - min(year) + 1),
         wcode = as.integer(factor(welfare_def), levels = unique(welfare_def)),
         ecode = as.integer(factor(equiv_scale), levels = unique(equiv_scale)),
         scode = as.integer(factor(series, levels = unique(series))))

swiid_source <- ineq0 %>% 
  select(-oth_count, -s_count) %>% 
  arrange(country, year, series)

save.image(file = "data/ineq.rda")
write_csv(swiid_source, "data/swiid_source.csv")


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
