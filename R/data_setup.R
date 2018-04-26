if (!require(pacman)) install.packages("pacman"); library(pacman)
p_load(tidyverse, readxl, 
       eurostat, rsdmx, xml2, CANSIM2R, pxweb, rvest,
       countrycode, janitor, pdftools)
p_load_gh("ropensci/tabulizerjars", "ropensci/tabulizer") # read PDF tables; see https://github.com/ropensci/tabulizer for installation help if needed
p_load_gh("ropengov/dkstat")

# Custom country codes (defined in R/cc_swiid.R)
load("data/cc_swiid.rda")
body(countrycode)[[2]] <- substitute(
  if (is.null(custom_dict) | as.list(match.call())[["custom_dict"]] == "cc_swiid") {
    if (origin == "country.name") {
      origin <- "country.name.en"
    }
    if (destination == "country.name") {
      destination <- "country.name.en"
    }
    if (origin %in% c("country.name.en", "country.name.de")) {
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
                            "https://web.archive.org/web/20170407020304/http://www2.stats.govt.nz/domino/external/pasfull/pasfull.nsf/84bf91b1a7b5d7204c256809000460a4/4c2567ef00247c6acc256b03000bdbe0/$FILE/Incomes.pdf", 
                            "https://web.archive.org/web/20100804001129/http://www.lisproject.org/key-figures/kf-workbook.xls")) %>% 
    arrange(country, year)
}

lis_files <- c("au", "at", "be", "br", "ca", "cn", "co", "cz", "dk",   # add "cl" when LIS releases data
               "do", "eg", "ee", "fi", "fr", "de", "ge", "gr", "gt", "hu", "is", 
               "in", "ie", "il", "it", "jp", "lt", "lu", "mx", "nl", "no", "pa", "py", 
               "pe", "pl", "ro", "ru", "rs", "sk", "si", "za", "kr", "es", "se", 
               "ch", "tw", "uk", "us", "uy") # add "tn" when LIS releases data

lis <- lis_files %>% 
  map_df(format_lis) %>% 
  filter(!country=="Russia") %>% 
  rbind(format_lis_xtra("nz"), format_lis_xtra("ru_old")) %>% 
  arrange(country, year, welfare_def, equiv_scale)

ru_lissy <- format_lis("ru") %>% 
  mutate(series = paste("RLMS", series))

# Socio-Economic Database for Latin America and the Caribbean (SEDLAC) (update link)
format_sedlac <- function(df, sheet, link, es) {
  x <- df
  if(ncol(x)==2) {
    x$se <- NA
  }
  names(x) <- c("heading", "gini", "se")
  x <- x %>%
    filter(!is.na(heading) & !str_detect(heading, "-II"))
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
  x <- x %>%
    group_by(country) %>%
    mutate(series0 = zoo::na.locf(series, na.rm = FALSE),
           series = paste("SEDLAC", country, "disp", es,
                          as.numeric(factor(series0, levels = unique(series0)))) %>% 
             str_replace("NA", "1")) %>% 
    ungroup() %>% 
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
              link = link) %>% 
    filter(!is.na(gini))
  
  return(x)
}

sedlac_link <- "http://www.cedlas.econo.unlp.edu.ar/wp/wp-content/uploads/inequality_LAC_2017-07.xlsx"
download.file(sedlac_link, "data-raw/sedlac.xlsx")

sedlac_pc <- read_excel(path = "data-raw/sedlac.xlsx", 
                        sheet = "intervals pci",
                        skip = 8)[1:3] %>%
  format_sedlac(sheet = "intervals pci",
                link = sedlac_link,
                es = "pc") 

sedlac_ei <- read_excel(path = "data-raw/sedlac.xlsx",
                        sheet = "intervals ei",
                        skip = 8)[1:3] %>%
  format_sedlac(sheet = "intervals ei",
                link = sedlac_link,
                es = "ae")

sedlac_hh <- read_excel(path = "data-raw/sedlac.xlsx",
                        sheet = "gini1",
                        skip = 7)[c(1,8)] %>%
  format_sedlac(sheet = "gini1",
                link = sedlac_link,
                es = "hh")

sedlac <- rbind(sedlac_ei, sedlac_hh, sedlac_pc) %>% 
  filter(!is.na(gini))

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
  mutate(country = countrycode(as.character(name), 
                               origin = "country.name.en", 
                               destination = "swiid.name",
                               custom_dict = cc_swiid)) %>% 
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
oecd_link <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/IDD/.GINI+STDG+GINIB+GINIG.TOT.CURRENT+PREVIOUS+INCOMPARABLE.METH2012+METH2011/all"
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
  mutate(gini_se = if_else(gini < 0.1, gini, gini/100)) %>%   # data for Chile and China obv on wrong scale
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

# World Bank Africa Poverty Database (bespoke analysis of subset of WB surveys; archived)
afr_gini <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/AFR_gini_sqrt.csv", 
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


# World Bank Povcalnet
wb_zip <- "http://databank.worldbank.org/data/download/WDI_csv.zip"
download.file(wb_zip, "data-raw/wb.zip")

wb_fn <- read_csv(unz("data-raw/wb.zip", "WDIFootNote.csv")) %>% 
  filter(SeriesCode == "SI.POV.GINI") %>% 
  mutate(year = str_replace(Year, "YR", "")) %>% 
  select(CountryCode, year, DESCRIPTION)

wb <- read_csv(unz("data-raw/wb.zip", "WDIData.csv")) %>% 
  filter(`Indicator Code` == "SI.POV.GINI") %>% 
  select(-`Country Name`, -contains("Indicator"), -contains("X")) %>% 
  gather(key = year, value = gini, -`Country Code`) %>% 
  rename(CountryCode = `Country Code`) %>% 
  filter(!is.na(gini)) %>% 
  left_join(wb_fn, by = c("CountryCode", "year")) %>% 
  filter(!str_detect(DESCRIPTION, "[Uu]rban|[Rr]ural")) %>% 
  mutate(CountryCode = if_else(CountryCode == "XKX", "KSV", CountryCode)) %>% 
  group_by(CountryCode) %>% 
  mutate(desc = as.integer(as.factor(DESCRIPTION))) %>% 
  ungroup() %>% 
  transmute(country = countrycode(CountryCode, origin = "wb_api3c", "swiid.name", custom_dict = cc_swiid),
            year = as.numeric(year),
            gini = as.numeric(gini)/100,
            gini_se = NA,
            welfare_def = if_else(str_detect(DESCRIPTION, "income"), "gross", "con"),
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("Povcalnet", country, welfare_def, equiv_scale, desc),
            source1 = "World Bank Povcalnet",
            page = "",
            link = wb_zip)

rm(wb_fn)
unlink("data-raw/wb.zip")   # too big to keep around


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
            page = "95",
            link = link)

rm(arm_page, arm_reports)


# Australian Bureau of Statistics (update abs_link; not included in abs api as of 2017-05)
# confirm latest release at: http://www.abs.gov.au/AUSSTATS/abs@.nsf/second+level+view?ReadForm&prodno=6523.0&viewtitle=Household%20Income%20and%20Wealth,%20Australia~2013-14~Latest~04/09/2015&&tabname=Past%20Future%20Issues&prodno=6523.0&issue=2013-14&num=&view=&
# latest release link > downloads tab > copy link to download xls for "Household Income and Income Distribution, Australia, 1994–95 to [year]"

abs_link <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&65230DO001_201516.xls&6523.0&Data%20Cubes&432A10E61C768B3FCA2581C600111BD7&0&2015-16&08.12.2017&Latest"
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

dane <- read_excel("data-raw/dane.xls", sheet = "Gini", skip = 15) %>% 
  filter(X__1 == "Nacional") %>% 
  gather(key = year, value = gini, -X__1) %>% 
  transmute(country = "Colombia",
            year = as.numeric(year),
            gini = gini,
            gini_se = NA,
            welfare_def = "gross",
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("DANE", welfare_def, equiv_scale),
            source1 = "Departamento Administrativo Nacional de Estadística Colombia",
            page = "",
            link = dane_link)


# Costa Rica (update file; as of 2017-05 page not sufficiently responsive to automate)
# http://www.inec.go.cr/pobreza-y-desigualdad/desigualdad
ineccr_link <- "http://www.inec.go.cr/sites/default/files/documetos-biblioteca-virtual/repobrezaenaho2010-2017-01.xlsx"

ineccr <- read_excel("data-raw/ineccr.xlsx",
                     skip = 4,
                     sheet = "1") %>%
  select(Año, Total) %>% 
  filter(!is.na(Año)) %>% 
  mutate(es = if_else(cumsum(str_detect(Año, "por persona")) == 1, "pc", "hh")) %>% 
  filter(str_detect(Año, "^\\d{4}$")) %>% 
  transmute(country = "Costa Rica",
            year = as.numeric(Año),
            gini = Total,
            gini_se = NA,
            welfare_def = "gross",
            equiv_scale = es,
            monetary = NA,
            series = paste("INEC", welfare_def, equiv_scale),
            source1 = "Instituto Naciónal de Estadística y Censos Costa Rica",
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
capmas_link <- "https://web.archive.org/web/20180425181619/http://www.msrintranet.capmas.gov.eg/pdf/studies/inds/EG-LIV-E-I.xls" # this file isn't updated anymore
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
            source1 = "Central Agency for Public Mobilization and Statistics Egypt",
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
statfi <- get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tul/tjt/statfin_tjt_pxt_015.px",
                         dims = list(Tulokäsite = c('SL2', '4L2', '6L2'),
                                     Tiedot = c('Gini'),
                                     Vuosi = c('*')),
                         clean = TRUE) %>% 
  transmute(country = "Finland",
            year = as.numeric(as.character(Vuosi)),
            gini = values/100,
            gini_se = NA,
            welfare_def = ifelse(str_detect(`Tulokäsite`, "Käytettävissä"), "disp",
                                 ifelse(str_detect(`Tulokäsite`, "Bruttotulot"), "gross",
                                        "market")),
            equiv_scale = "oecdm",
            monetary = FALSE,
            series = paste("Statistics Finland", welfare_def, equiv_scale),
            source1 = "Statistics Finland",
            page = "",
            link = "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/tul/tjt/270_tjt_tau_117.px")


# Insee France (archived)
insee_link <- "https://web.archive.org/web/20151206151022/http://www.insee.fr/fr/themes/series-longues.asp?indicateur=gini-niveaux-vie"

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
            source1 = "Institut National de la Statistique et des Études Économiques France",
            page = "",
            link = insee_link)


# Statistics Georgia (update file)
# http://pc-axis.geostat.ge/PXWeb/pxweb/ka/Database
# Social Statistics > Standard of Living, Subsistance Minimum > Gini Coefficients by Year and Indicator
# All years, "By total incomes" and "By total expenditures"

geostat <- read_delim("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/geostat.csv",
                      delim = ";",
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
            source1 = "Statistics Georgia",
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
            link = bpsid2_link) 

bpsid <- bind_rows(bpsid1, bpsid2) %>% 
  arrange(year, link) %>% 
  distinct(year, .keep_all = TRUE)

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
cso_ie_link <- "http://www.cso.ie/px/pxeirestat/Database/eirestat/Survey%20on%20Income%20and%20Living%20Conditions%20(SILC)/SIA47.px"
download.file(cso_ie_link_px, "data-raw/cso_ie.px")

cso_ie <- pxR::read.px("data-raw/cso_ie.px") %>% 
  pxR:::as.data.frame.px() %>% 
  filter(Statistic == "Gini Coefficient (%)") %>% 
  transmute(country = "Ireland",
            year = as.numeric(as.character(Year)),
            gini = as.numeric(value)/100,
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = "pc",
            monetary = FALSE,
            series = paste("CSO Ireland", welfare_def, equiv_scale),
            source1 = "CSO Ireland",
            page = "",
            link = cso_ie_link)


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
  filter(gini < 1) %>% # because 2017 ratio without 2017 gini on 2017-04-25 
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
# Item: all households and urban; By index of distribution: Gini's; Time: all; Pivot By income, Time Period, Item x By index of distribution 

kostat <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/kostat.csv",
                   col_types = cols(
                     .default = col_double(),
                     Item = col_character(),
                     `By income` = col_character()
                   )) %>%
  filter(!is.na(`Gini's coefficient`)) %>% 
  transmute(country = "Korea",
            year = as.numeric(Period),
            gini = `Gini's coefficient`,
            gini_se = NA,
            welfare_def = if_else(str_detect(`By income`, "Disposable"), "disp", "market"),
            equiv_scale = "ae",
            monetary = NA,
            series = paste("Kostat", welfare_def, equiv_scale, if_else(Item == "All households", "national", "urban")),
            source1 = "Statistics Korea",
            page = "",
            link = "http://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1L6E001&conn_path=I2&language=en")


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


# Economy Planning Unit of Malaysia (update link--http://www.epu.gov.my/ms/search/node/gini)
epumy_link <- "http://www.epu.gov.my/sites/default/files/Jadual%206%20-%20Pekali%20Gini%20Mengikut%20Kumpulan%20Etnik%2C%20Strata%20dan%20Negeri%2C%20Malaysia%2C%201970-2016.pdf"
download.file(epumy_link, "data-raw/epumy.pdf")

epumy <- extract_tables("data-raw/epumy.pdf") %>% 
  first() %>% 
  t() %>% 
  as_tibble() %>% 
  transmute(country = "Malaysia",
            year = as.numeric(V1),
            gini = as.numeric(V2),
            gini_se = NA,
            welfare_def = "gross",
            equiv_scale = "hh",
            monetary = FALSE,
            series = paste("EPU Malaysia", welfare_def, equiv_scale),
            source1 = "Economy Planning Unit of Malaysia",
            page = "1",
            link = epumy_link) %>% 
  filter(!is.na(gini))


# National Bureau of Statistics Moldova (update link: check Statistical Yearbook to find table number)
nbs_link <- "http://www.statistica.md/public/files/serii_de_timp/venituri_cheltuieli/veniturile_gospodariilor/4.2.5.xls"
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


# Statistics New Zealand (archived)

snz_link <- "https://web.archive.org/web/20170407020304/http://www2.stats.govt.nz/domino/external/pasfull/pasfull.nsf/84bf91b1a7b5d7204c256809000460a4/4c2567ef00247c6acc256b03000bdbe0/$FILE/Income.xls"
download.file(snz_link, "data-raw/snz.xls")

snz <- read_excel("data-raw/snz.xls",
                  sheet = "A2.2", skip = 3) %>% 
  transmute(country = "New Zealand",
            year = as.numeric(Year),
            gini = `Gini coefficient`,
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = "oecdm", # actually the Revised Jensen Scale, but basically identical to oecdm (see https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources/monitoring/household-income-report/2014/appendices.doc)
            monetary = NA,
            series = paste("Statistics New Zealand 1999", welfare_def, equiv_scale),
            source1 = "Statistics New Zealand 1999",
            page = "100",
            link = "https://web.archive.org/web/20170407020304/http://www2.stats.govt.nz/domino/external/pasfull/pasfull.nsf/84bf91b1a7b5d7204c256809000460a4/4c2567ef00247c6acc256b03000bdbe0/$FILE/Incomes.pdf")


# New Zealand Ministry of Social Development
# update link from http://www.msd.govt.nz/about-msd-and-our-work/publications-resources/monitoring/household-incomes/index.html
# and wrangle (including publication year)

nzmsd_link <- "http://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources/monitoring/household-income-report/2017/2017-incomes-report-wed-19-july-2017.pdf"
download.file(nzmsd_link, "data-raw/nzmsd.pdf")

nzmsd <- extract_tables("data-raw/nzmsd.pdf", pages = 92) %>% 
  first() %>% 
  t() %>% 
  as_tibble() %>% 
  transmute(country = "New Zealand",
            year = if_else(as.numeric(V1) < 50, as.numeric(paste0("20", V1)), as.numeric(paste0("19", V1))),
            gini = as.numeric(V3)/100,
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = "sqrt",
            monetary = NA,
            series = paste("Perry 2017", welfare_def, equiv_scale),
            source1 = "Perry 2017",
            page = "92",
            link = nzmsd_link) %>% 
  filter(!is.na(gini))


# Statistics Norway (update file and wrangle)
# Gini & std.err.; total population; all years > pivot clockwise > save as semicolon delimited

ssb_link <- "https://www.ssb.no/statistikkbanken/selectvarval/Define.asp?MainTable=InntUlikhet&PLanguage=1&nyTmpVar=true&CMSSubjectArea=inntekt-og-forbruk&KortNavnWeb=ifhus&StatVariant=&checked=true"

ssb <- read_csv2("data-raw/ssb.csv", skip = 2) %>%  # throws warnings; they are irrelevant
  transmute(country = "Norway",
            year = year,
            gini = as.numeric(`Total population Gini coefficient`),
            gini_se = as.numeric(`Total population Standard error of the Gini coefficient`),
            welfare_def = "disp",
            equiv_scale = "oecdm",
            monetary = TRUE,
            series = paste("SSB", welfare_def, equiv_scale),
            source1 = "Statistics Norway",
            page = "",
            link = ssb_link)


# DGEEC Paraguay (update link and, probably, wrangle)
# http://www.dgeec.gov.py > Publicaciones > Pobreza

dgeec_link <- "https://web.archive.org/web/20180319151156/http://www.dgeec.gov.py/Publicaciones/Biblioteca/diptico%20desigualdad%20ingreso/diptico%20DESIGUALDAD%20DE%20INGRESOS.pdf"
download.file(dgeec_link, "data-raw/dgeec.pdf")

dgeec <- extract_tables("data-raw/dgeec.pdf", pages = 2) %>% 
  first() %>% 
  as_tibble() %>% 
  transmute(country = "Paraguay",
            year = str_trim(V1) %>% 
              str_extract("\\d{4}/?\\d{0,2}") %>% 
              str_replace("(\\d{2})\\d{2}/(\\d{2})", "\\1\\2") %>% 
              as.numeric(),
            gini = str_trim(V1) %>% 
              str_extract("01?,\\d{3}") %>% 
              str_replace("1?,", ".") %>% 
              as.numeric(),
            gini_se = NA,
            welfare_def = "gross",
            equiv_scale = "pc",
            monetary = TRUE,
            series = paste("DGEEC", welfare_def, equiv_scale),
            source1 = "Dirección General de Estadística, Encuestas y Censos 2017",
            page = "2",
            link = dgeec_link) %>% 
  filter(!is.na(year))


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
# Social and Economic Indicators of the Russian Federation ('Attachment to the Yearbook')

rosstat_link <- "http://www.gks.ru/free_doc/doc_2017/year/pril_year17-eng.xls"
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


# Singapore Department of Statistics (update file--and check for inclusion in API)
# Note: Population covered is only resident households with at least one worker 
#  and so excludes 8-11% of resident households (which, from other data, appear
#  to be among the poorest), plus all non-resident households (surely poor).
# Note also that income definition excludes income from capital.
# These data therefore should be considered a lower bound.  Blech.
# http://www.tablebuilder.singstat.gov.sg/ > search "gini" > Key Indicators > Search variable "gini" > Create
# Export > CSV

singstat <- read_csv("data-raw/singstat.csv", skip = 4) %>% 
  filter(!is.na(`2000`)) %>%
  select(-starts_with("X")) %>% 
  gather(key = year, value = gini, -Variables) %>% 
  transmute(country = "Singapore",
            year = as.numeric(year),
            gini = as.numeric(gini),
            gini_se = NA,
            welfare_def = if_else(str_detect(Variables, "After"), "disp", "market"),
            equiv_scale = if_else(str_detect(Variables, "OECD"), "oecdm",
                                  if_else(str_detect(Variables, "Square"), "sqrt", "pc")),
            monetary = TRUE,
            series = paste("Singstat", welfare_def, equiv_scale),
            source1 = "Singapore Department of Statistics",
            page = "",
            link = "http://www.tablebuilder.singstat.gov.sg/")


# Statistics Slovenia (archived; automated)
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

ssi2 <- pxR::read.px("data-raw/ssi2.px") %>% 
  pxR:::as.data.frame.px() %>% 
  filter(str_detect(INCOME, "pensions are included")) %>% 
  transmute(country = "Slovenia",
            year = as.numeric(as.character(YEAR)) - 1,
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
            series = paste("Instituto Nacional de Estadistica Spain", welfare_def, equiv_scale),
            source1 = "Instituto Nacional de Estadística Spain",
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

tdgbas_link <- "http://win.dgbas.gov.tw/fies/doc/result/105/a11/Year05.xls"
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

turkstat_oecdm <- read_excel("data-raw/turkstat_oecdm.xls", skip = 3) 
turkstat_hh <- read_excel("data-raw/turkstat_hh.xls", skip = 3) 
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


# U.K. Office for National Statistics (update links; join with latest file last)
# https://www.ons.gov.uk/atoz?query=effects+taxes+benefits (new releases in April and January)

ons_link1 <- "https://www.ons.gov.uk/generator?uri=/peoplepopulationandcommunity/personalandhouseholdfinances/incomeandwealth/bulletins/theeffectsoftaxesandbenefitsonhouseholdincome/financialyearending2016/bd6b2fe3&format=csv"
download.file(ons_link1, "data-raw/ons1.csv")

ons1 <- read_csv("data-raw/ons1.csv", skip = 7, col_types = "cdddd") %>% 
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
            link = ons_link1)  


ons_link2 <- "https://www.ons.gov.uk/generator?uri=/peoplepopulationandcommunity/personalandhouseholdfinances/incomeandwealth/bulletins/householddisposableincomeandinequality/financialyearending2017/51fff87e&format=csv"
download.file(ons_link2, "data-raw/ons2.csv")

ons2 <- read_csv("data-raw/ons2.csv", skip = 6, col_types = "cddd") %>% 
  transmute(year = X1,
            market = Original,
            gross = Gross,
            disp = Disposable) %>% 
  gather(key = welfare_def, value = gini, -year) %>% 
  transmute(country = "United Kingdom",
            year = as.numeric(str_replace(year, "^(\\d{2}).*(\\d{2})$", "\\1\\2")),
            gini = gini/100,
            gini_se = NA,
            welfare_def = welfare_def,
            equiv_scale = "oecdm",
            monetary = FALSE,
            series = paste("ONS", welfare_def, equiv_scale),
            source1 = "UK Office for National Statistics",
            page = "",
            link = ons_link2) 

ons <- ons1 %>%
  anti_join(ons2, by = c("year", "welfare_def")) %>% 
  bind_rows(ons2) %>% 
  arrange(welfare_def, year)

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

uscb_links <- paste0("https://www2.census.gov/programs-surveys/demo/tables/p60/259/tableA", 2:3, ".xls")
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
uine_link <- "http://www.ine.gub.uy/documents/10181/364159/Estimación+de+la+pobreza+por+el+Método+del+Ingreso+2017/f990baaf-1c32-44c5-beda-59a20dd8325c"
download.file(uine_link, "data-raw/uine.pdf")

uine <- extract_tables("data-raw/uine.pdf", pages = 47)[[2]][6:17, 1] %>% 
  as_data_frame() %>% 
  separate(value, into = paste0("V", 1:7), sep = "\\s") %>% 
  transmute(country = "Uruguay",
            year = V1 %>% str_trim() %>% as.numeric(),
            gini = as.numeric(sub(",", ".", V2, fixed = TRUE)),
            gini_se = NA,
            welfare_def = "disp",
            equiv_scale = "pc",
            monetary = TRUE,
            series = paste("Instituto Nacional de Estadistica Uruguay", welfare_def, equiv_scale),
            source1 = "Instituto Nacional de Estadística Uruguay",
            page = "45",
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
            series = paste("Instituto Nacional de Estadistica Venezuela", welfare_def, equiv_scale),
            source1 = "Instituto Nacional de Estadística Venezuela",
            page = "",
            link = inev_link)


# General Statistics Office of Vietnam (update file for gso_vn2; check 'page')
# http://www.gso.gov.vn/default_en.aspx?tabid=783 > Index of income inequality
# WHOLE COUNTRY, all years > Comma delimited with heading

gso_vn1_link <- "http://www.gso.gov.vn/Modules/Doc_Download.aspx?DocID=16773"
gso_vn2_link <- "http://www.gso.gov.vn/default_en.aspx?tabid=783"
download.file(gso_vn1_link, "data-raw/gso_vn2013.pdf")

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

gso_vn2 <- read_csv("data-raw/gso_vn2.csv", skip = 1) %>% 
  filter(Iterms == "WHOLE COUNTRY") %>% 
  select(-Iterms) %>% 
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
            page = "28",
            link = gso_vn2_link)  

gso_vn <- bind_rows(gso_vn1, gso_vn2)

rm(gso_vn1, gso_vn2)

# Additional Inequality Datasets

# Milanovic All the Ginis (update if possible: now using 2014 from WB; 2016 link at CUNY not working currently)
atg_link <- "https://web.archive.org/web/20170907141001/http://siteresources.worldbank.org/INTRES/Resources/469232-1107449512766/allginis_Oct2014.dta"
writeBin(httr::content(html_session(atg_link)$response, "raw"), "data-raw/atg.dta")

atg0 <- haven::read_dta("data-raw/atg.dta", encoding = "latin1") %>% 
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
            source1 = "Milanovic 2014",
            page = "",
            link = atg_link)

brandolini <- haven::read_dta("data-raw/atg.dta", encoding = "latin1") %>% 
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
            source1 = "Milanovic 2014; Brandolini 1998",
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
added_data <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/fs_added_data.csv",
                       col_types = "ciddcclcccc")

## Combine
make_inputs <- function(baseline_series, nbl = FALSE) {
  # first, get baseline series and order by data-richness
  baseline_wd <- str_split(baseline_series, "\\s")[[1]] %>% nth(-2)
  baseline_es <- str_split(baseline_series, "\\s")[[1]] %>% last()
  baseline_wdes <- paste0(baseline_wd, "_", baseline_es)
  baseline <- lis %>% 
    filter(series == baseline_series) %>% 
    mutate(gini_b = gini,
           gini_b_se = gini_se * 2) %>%
    select(-gini, -gini_se) %>% 
    group_by(country) %>% 
    mutate(k_bl_obs = n()) %>% 
    ungroup() %>% 
    arrange(desc(k_bl_obs))
  
  # turn cross-country series into within-country series
  oecd1 <- oecd %>% 
    mutate(series = paste("OECD", country, str_replace(series, "OECD ", "")))
  ceq1 <- ceq %>% 
    mutate(series = paste("CEQ", country, str_replace(series, "CEQ ", "")))
  
  # then combine with other series ordered by data-richness
  ineq0 <- bind_rows(lis, 
                     sedlac, cepal, cepal_sdi, oecd1, eurostat,
                     transmonee, ceq1, afr_gini, wb,
                     armstat, abs, inebo, belstat, statcan, dane, ineccr, dkstat,
                     capmas, statee, statfi, insee, geostat,
                     stathk, bpsid, amar, cso_ie, istat, kazstat, kostat, nsck,
                     epumy, nbs, monstat, snz, nzmsd, ssb, dgeec, psa,
                     rosstat, ru_lissy, singstat, ssi, ine, statslk, scb, 
                     tdgbas, nso_thailand, turkstat, ons, ifs, cbo, uscb, uine, inev, gso_vn,
                     atg, gidd,
                     added_data) %>% 
    rename(gini_m = gini,
           gini_m_se = gini_se) %>%
    mutate(country = countrycode(country, "country.name", "swiid.name", custom_dict = cc_swiid),
           region = countrycode(country, "swiid.name", "swiid.region", custom_dict = cc_swiid)) %>% 
    group_by(country) %>% 
    mutate(country_obs = n()) %>% 
    ungroup() %>% 
    group_by(country, series) %>% 
    mutate(series_obs = n()) %>%
    ungroup() %>% 
    arrange(desc(country_obs), desc(series_obs))  
  
  if (str_detect(baseline_series, "market") & nbl == TRUE) {
    ineq0 <- ineq0 %>% 
      filter(welfare_def == "market")
  } 
  
  # obs with baseline data
  ineq_bl <- ineq0 %>% 
    right_join(baseline %>% 
                 select(country, year, gini_b, gini_b_se, k_bl_obs),
               by = c("country", "year")) %>% 
    arrange(desc(k_bl_obs)) %>% 
    group_by(country, series) 
  
  ineq_bl_series <- ineq_bl %>% pull(series) %>% unique()
  
  # obs with no baseline data from series with some baseline data ("overlap baseline")
  ineq_obl <- ineq0 %>% anti_join(ineq_bl %>% select(-gini_b, -gini_b_se), 
                                  by = c("country", "year")) %>% 
    filter(series %in% ineq_bl_series) %>% 
    group_by(country, series)
  
  # obs from series with no baseline data
  ineq_nbl <- ineq0 %>% anti_join(ineq_bl %>% select(-gini_b, -gini_b_se), 
                                  by = c("country", "year")) %>% 
    filter(!series %in% ineq_bl_series)
  
  ineq_oth_series <- bind_rows(ineq_obl, ineq_nbl) %>% pull(series) %>% unique()
  
  # combine all
  ineq <- bind_rows(ineq_bl, ineq_obl, ineq_nbl) %>% 
    group_by(series) %>% 
    mutate(s_bl_obs = sum(!is.na(gini_b))) %>% 
    ungroup() %>% 
    group_by(country) %>% 
    mutate(k_bl_obs = if_else(!is.na(mean(k_bl_obs, na.rm = TRUE)),
                              mean(k_bl_obs, na.rm = TRUE), 0),
           tcode0 = year - min(year) + 1) %>% 
    ungroup() %>% 
    arrange(desc(k_bl_obs), desc(country_obs)) %>% 
    mutate(gini_m_se = ifelse(!is.na(gini_m_se), gini_m_se * 2,
                              quantile(gini_m_se/gini_m, .99, na.rm = TRUE) * gini_m * 2),
           wdes = paste(welfare_def, equiv_scale, sep = "_"),
           ibl = (gini_m == gini_b & series == first(baseline$series)),
           bl = (!is.na(gini_b)),
           obl = (s_bl_obs>0),
           kbl = (k_bl_obs>0),
           kcode = as.integer(factor(country, levels = unique(country))),
           tcode = tcode0,
           rcode = as.integer(factor(region, levels = unique(region))),
           scode = as.integer(factor(series, levels = unique(series))),
           wcode = as.integer(factor(welfare_def) %>% forcats::fct_relevel(baseline_wd)),
           ecode = as.integer(factor(equiv_scale) %>% forcats::fct_relevel(baseline_es)),
           wecode = as.integer(factor(paste(wcode, ecode))),
           kwecode = as.integer(factor(100*kcode+wecode)),
           rwecode = as.integer(factor(100*rcode+wecode))) %>% 
    select(-tcode0) # tcode0 is only used to facilitate getting tcode into its customary column position
  
  wecodes <- ineq %>%
    select(wdes, wecode, wcode, ecode) %>% 
    distinct() %>% 
    mutate(wd = str_replace(wdes, "_.*", ""),
           es = str_replace(wdes, ".*_", "")) %>% 
    arrange(wecode)
  
  kwecodes <- ineq %>%
    select(wecode, kcode, kwecode, rwecode) %>% 
    distinct()
  
  ineq1 <- ineq %>% 
    group_by(kcode, tcode, welfare_def, equiv_scale) %>% 
    summarize(n_obs = n(),
              gini_cat = mean(gini_m), 
              gini_cat_se = ifelse(n_obs == 1,
                                   gini_m_se,
                                   sqrt(mean(gini_m_se^2) + (1+1/n_obs)*var(gini_m)))) %>%  # per Rubin (1987)
    ungroup() %>% 
    select(-n_obs) %>% 
    unite(wdes, welfare_def, equiv_scale) %>% 
    bind_rows(ineq %>%
                group_by(kcode, tcode) %>% 
                summarize(gini_cat = first(gini_b),
                          gini_cat_se = first(gini_b_se),
                          wdes = "baseline") %>% 
                ungroup())
  
  
  ## Generate ratios
  # generate ratios of baseline to each wd_es 
  rho_we0 <- ineq1 %>% 
    select(-gini_cat_se) %>% 
    spread(key = wdes, value = gini_cat) %>% 
    mutate_at(vars(matches("_")),
              funs(baseline/.)) %>% 
    select(-baseline) %>% 
    gather(key = wdes, value = rho, -kcode, -tcode) %>% 
    filter(!is.na(rho)) %>% 
    arrange(kcode, tcode, wdes)
  
  rho_we_se <- ineq1 %>% 
    select(-gini_cat) %>% 
    spread(key = wdes, value = gini_cat_se) %>% 
    mutate_at(vars(matches("_")),
              funs(sqrt(baseline^2+.^2))) %>% 
    select(-matches("baseline")) %>% 
    gather(key = wdes, value = rho_se, -kcode, -tcode) %>% 
    filter(!is.na(rho_se)) %>% 
    arrange(kcode, tcode, wdes)
  
  rho_we00 <- rho_we0 %>% 
    left_join(rho_we_se, by = c("kcode", "tcode", "wdes")) %>% 
    mutate(rho_se = if_else(rho == 1, .1, rho_se)) %>%            # placeholder for baseline series
    left_join(ineq %>% select(country, year, kcode, tcode, rcode) %>% distinct(),
              by = c("kcode", "tcode")) %>% 
    left_join(wecodes, by = "wdes") %>% 
    left_join(kwecodes, by = c("kcode", "wecode"))
  
  rcodes_not_miss <- rho_we00 %>%                       # regions with no observed ratios to baseline series
    filter(!rho == 1) %>%
    select(rcode, wdes) %>% 
    filter(wdes == baseline_wdes) %>% 
    unique() %>% 
    pull(rcode)
  
  rho_we <- rho_we00 %>% 
    filter(!(rho == 1 & (rcode %in% rcodes_not_miss)))  # use placeholder if no observed ratios to baseline series
  
  rm(rho_we0, rho_we_se)
  
  # generate ratios of baseline_wd to each wd (for all constant es)
  rho_wd0 <- map_df(c("pc", "hh", "sqrt", "oecdm", "ae"), function(e) {
    ineq1 %>% 
      select(-gini_cat_se) %>% 
      spread(key = wdes, value = gini_cat) %>% 
      mutate(bl = get(paste0(baseline_wd, "_", e))) %>% 
      mutate_at(vars(matches(e)),
                funs(bl/.)) %>% 
      select(kcode, tcode, matches(e)) %>% 
      gather(key = wdes, value = rho_wd, -kcode, -tcode) %>% 
      filter(!is.na(rho_wd)) %>% 
      mutate(wd = str_replace(wdes, "_.*", "")) %>% 
      select(-wdes) %>% 
      arrange(kcode, tcode, wd)
  })
  
  rho_wd_se <- map_df(c("pc", "hh", "sqrt", "oecdm", "ae"), function(e) {
    ineq1 %>% 
      select(-gini_cat) %>% 
      spread(key = wdes, value = gini_cat_se) %>% 
      mutate(bl = get(paste0(baseline_wd, "_", e))) %>% 
      mutate_at(vars(matches(e)),
                funs(sqrt(bl^2+.^2))) %>% 
      select(kcode, tcode, matches(e)) %>% 
      gather(key = wdes, value = rho_wd_se, -kcode, -tcode) %>% 
      filter(!is.na(rho_wd_se)) %>% 
      mutate(wd = str_replace(wdes, "_.*", "")) %>% 
      select(-wdes) %>% 
      arrange(kcode, tcode, wd)
  })
  
  rho_wd <- rho_wd0 %>% 
    left_join(rho_wd_se, by = c("kcode", "tcode", "wd")) %>% 
    group_by(kcode, tcode, wd) %>%
    summarize(rho_wd = max(rho_wd),
              rho_wd_se = max(rho_wd_se)) %>%
    ungroup() %>%
    left_join(ineq %>% select(country, year, kcode, tcode, rcode, kbl) %>% distinct(),
              by = c("kcode", "tcode")) %>% 
    left_join(wecodes %>% select("wd", "wcode") %>% distinct(), by = "wd") %>% 
    mutate(kwcode = as.integer(factor(100*kcode+wcode)),
           rwcode = as.integer(factor(100*rcode+wcode)),
           kwd = paste(country, wd),
           rwd = paste(rcode, wd))
  
  rm(rho_wd0, rho_wd_se)
  
  rho_wd_kw <- rho_wd %>% 
    pull(kwd) %>% 
    unique()
  
  # generate ratios of baseline_es to each es (for all constant wd)
  wdes <- c("market", "gross", "disp", "con")
  if (str_detect(baseline_series, "market") & nbl == TRUE) {
    wdes <- "market"
  }
  rho_es0 <- map_df(c("market", "gross", "disp", "con"), function(w) {
    ineq1 %>%
      select(-gini_cat_se) %>%
      spread(key = wdes, value = gini_cat) %>%
      mutate(bl = get(paste0(w, "_", baseline_es))) %>%
      mutate_at(vars(matches(w)),
                funs(bl/.)) %>%
      select(kcode, tcode, matches(w)) %>%
      gather(key = wdes, value = rho_es, -kcode, -tcode) %>%
      filter(!is.na(rho_es)) %>%
      mutate(es = str_replace(wdes, ".*_", "")) %>%
      select(-wdes) %>%
      arrange(kcode, tcode, es)
  })
  
  rho_es_se <- map_df(c("market", "gross", "disp", "con"), function(w) {
    ineq1 %>%
      select(-gini_cat) %>%
      spread(key = wdes, value = gini_cat_se) %>%
      mutate(bl = get(paste0(w, "_", baseline_es))) %>%
      mutate_at(vars(matches(w)),
                funs(sqrt(bl^2+.^2))) %>%
      select(kcode, tcode, matches(w)) %>%
      gather(key = wdes, value = rho_es_se, -kcode, -tcode) %>%
      filter(!is.na(rho_es_se)) %>%
      mutate(es = str_replace(wdes, ".*_", "")) %>%
      select(-wdes) %>%
      arrange(kcode, tcode, es)
  })
  
  rho_es <- rho_es0 %>%
    left_join(rho_es_se, by = c("kcode", "tcode", "es")) %>%
    group_by(kcode, tcode, es) %>%
    summarize(rho_es = max(rho_es),
              rho_es_se = max(rho_es_se)) %>%
    ungroup() %>%
    left_join(ineq %>% select(country, year, kcode, tcode, rcode, kbl) %>% distinct(),
              by = c("kcode", "tcode")) %>%
    left_join(wecodes %>% select("es", "ecode") %>% distinct(), by = "es") %>%
    mutate(kecode = as.integer(factor(100*kcode+ecode)),
           recode = as.integer(factor(100*rcode+ecode)),
           kes = paste(country, es),
           res = paste(rcode, es))
  
  rm(rho_es0, rho_es_se)
  
  rho_es_ke <- rho_es %>%
    pull(kes) %>%
    unique()
  
  kyrs <- ineq %>%
    group_by(kcode) %>%
    summarize(firstyr = min(year),
              lastyr = max(year),
              n_yrs = year %>% unique() %>% length()) %>% 
    ungroup()
  
  ineq2 <- ineq %>% 
    left_join(kyrs, by = "kcode") %>% 
    mutate(kwd = paste(country, str_replace(wdes, "_.*", "")),
           kes = paste(country, str_replace(wdes, ".*_", "")),
           rwd = paste(rcode, str_replace(wdes, "_.*", "")),
           res = paste(rcode, str_replace(wdes, ".*_", "")),
           kw = (kwd %in% rho_wd_kw),
           ke = (kes %in% rho_es_ke)) %>% 
    left_join(rho_wd %>% select(kwd, kwcode) %>% unique(), by = "kwd") %>% 
    left_join(rho_wd %>% select(rwd, rwcode) %>% unique(), by = "rwd") %>% 
    left_join(rho_es %>% select(kes, kecode) %>% unique(), by = "kes") %>% 
    left_join(rho_es %>% select(res, recode) %>% unique(), by = "res") %>% 
    mutate(kwcode = if_else(is.na(kwcode), 0L, kwcode),
           kecode = if_else(is.na(kecode), 0L, kecode)) %>% 
    arrange(desc(ibl), desc(bl), desc(obl), desc(kbl), desc(kw), desc(ke), desc(k_bl_obs), desc(country_obs))
  
  return(list(ineq2, rho_we, rho_wd, ineq0))
}

disp <- make_inputs("LIS disp sqrt")
ineq2 <- disp[[1]]
rho_we <- disp[[2]]
rho_wd <- disp[[3]]

market <- make_inputs("LIS market sqrt")
ineq2_m <- market[[1]]
rho_we_m <- market[[2]]
rho_wd_m <- market[[3]]

## Save
swiid_source <- disp[[4]] %>% 
  rename(gini = gini_m,
         gini_se = gini_m_se) %>% 
  select(-country_obs, -series_obs, -region) %>% 
  arrange(country, year, series)

rm(disp, market)
write_csv(swiid_source, "data/swiid_source.csv", na = "")
save.image(file = "data/ineq.rda")
