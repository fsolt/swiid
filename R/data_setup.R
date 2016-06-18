if (!require(pacman)) install.packages("pacman")
p_load(readr, readxl, 
       eurostat, rsdmx, xml2, CANSIM2R,
       tidyr, stringr, magrittr, dplyr, purrr, reshape2,
       countrycode)
p_load_gh("leeper/tabulizerjars", "leeper/tabulizer") # read PDF tables

# check if WB gini info is now available and library(wbstats) or library(WDI)

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
  filter(!country=="Russian Federation") %>% 
  rbind(format_lis_xtra("net_sqrt_nz"), format_lis_xtra("net_sqrt_ru")) %>% 
  arrange(country, year, welfare_def, equiv_scale)



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
                es = "adeq")

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
  group_by(country, area) %>% 
  transmute(year = year,
            gini = as.numeric(as.character(valor)),
            gini_se = NA,
            welfare_def = "net",
            equiv_scale = "pc",
            monetary = TRUE,
            notes = ifelse(is.na(descripcion), "", as.character(descripcion)),
            series = paste("CEPAL", country, "net pc", as.numeric(factor(notes, levels = unique(notes)))),
            source1 = "CEPALStat",
            page = "",
            link = cepal_link) %>% 
  ungroup() %>% 
  select(-notes, -area)

rm(cepal0, cepal_raw, cepal_labels, cepal_notes)


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
eurostat <- get_eurostat("ilc_di12", time_format = "num", update_cache = FALSE) %>% 
  label_eurostat(code = "geo")  %>% 
  left_join(get_eurostat("ilc_di12", time_format = "num", keepFlags = TRUE) %>%
              rename(geo_code = geo), by = c("geo_code", "time", "values")) %>% 
  transmute(country = countrycode(as.character(geo), "country.name", "country.name"),
         year = time,
         gini = values/100,
         gini_se = NA,
         welfare_def = "net",
         equiv_scale = "oecd",   
         monetary = TRUE,
         break_yr = ifelse(is.na(flags) | flags!="b", 0, 1),
         series = "",
         source1 = "Eurostat",
         page = "",
         link = "http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=ilc_di12&lang=en") %>% 
  filter(!(is.na(country) | is.na(gini))) %>% 
  group_by(country) %>% 
  arrange(country, year) %>% 
  mutate(series = paste("Eurostat", country, "net oecd", cumsum(break_yr) + 1)) %>%  # No word from Eurostat which obs cross-nationally comparable
  ungroup() %>% 
  select(-break_yr)


# Commitment to Equity
ceq <- read_csv("https://raw.githubusercontent.com/fsolt/swiid/master/data-raw/ceq.csv", col_types = "cnnncclcccc") %>% 
  mutate(series = paste("CEQ", welfare_def, equiv_scale))

## National Statistics Offices

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


# U.K. Institute for Fiscal Studies
ifs_link <- "http://www.ifs.org.uk/uploads/publications/bns/bn19figs_update2015.xlsx"
download.file(ifs_link, "data-raw/ifs.xlsx")

ifs <- read_excel("data-raw/ifs.xlsx", sheet = 5, col_names = FALSE, skip = 3) %>%
  select(X1, X2, X3) %>% 
  melt(id.vars = c(1,2),
       value.name = "gini",
       na.rm = TRUE) %>% 
  transmute(country = "United Kingdom",
            year = ifelse(str_extract(X1, "\\d{2}$") %>% as.numeric() > 50,
                   str_extract(X1, "\\d{2}$") %>% as.numeric() + 1900,
                   str_extract(X1, "\\d{2}$") %>% as.numeric() + 2000),
            gini = gini,
            gini_se = NA,
            welfare_def = "net",
            equiv_scale = "oecd",
            monetary = TRUE,
            series = paste("IFS", X2, "net adeq"),
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
  melt(id.vars = "year", 
       variable.name = "welfare_def", 
       value.name = "gini") %>% 
  mutate(country = "United States",
         gini_se = NA,
         equiv_scale = "sqrt",
         monetary = TRUE,
         series = paste("CBO", welfare_def, "sqrt"),
         source1 = "U.S. Congressional Budget Office",
         page = "",
         link = cbo_link)

## Combine
# first, get baseline series and order by data-richness
baseline_series <- "LIS net sqrt"
baseline <- lis %>% filter(series==baseline_series) %>% 
  rename(gini_b = gini,
         gini_b_se = gini_se) %>%
  group_by(country) %>% 
  mutate(lis_count = n()) %>% 
  ungroup() %>% 
  arrange(desc(lis_count)) 

# then combine with other series ordered by data-richness
ineq0 <- bind_rows(lis %>% filter(series!=baseline_series), 
                  sedlac, cepal, oecd, eurostat, ceq, statcan, ifs, cbo) %>% 
  rename(gini_m = gini,
         gini_m_se = gini_se) %>%
  group_by(country) %>% 
  mutate(oth_count = n()) %>% 
  ungroup() %>% 
  arrange(desc(oth_count)) %>% 
  select(-oth_count) 

# obs with baseline data
ineq_bl <- ineq0 %>% 
  right_join(baseline %>% 
               select(country, year, gini_b, gini_b_se, lis_count),
             by = c("country", "year")) %>% 
  filter(!is.na(gini_m)) %>% 
  arrange(desc(lis_count)) %>% 
  select(-lis_count)

# obs with no baseline data
ineq_nbl <- ineq0 %>% anti_join(ineq_bl %>% select(-gini_b, -gini_b_se), 
             by = c("country", "year"))

ineq <- bind_rows(ineq_bl, ineq_nbl) %>% 
  mutate(ccode = as.numeric(factor(country, levels = unique(country))),
         tcode = as.integer(year - min(year) + 1),
         mcode = as.numeric(factor(series, levels = unique(series))))
