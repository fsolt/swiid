library(dplyr)
library(readr)
library(readxl)
library(reshape2)
library(eurostat)
library(tidyr)

###NEITHER OF THESE IS DONE

# Sedlac
download.file("http://sedlac.econo.unlp.edu.ar/download.php?file=archivos_estadistica/inequality_LAC_2015-06.xls", "data-raw/sedlac.xls")
sedlac_pc <- read_excel("data-raw/sedlac.xls", sheet = "intervals pci", skip = 8)



# Eurostat (no flags for series breaks)
eurostat <- get_eurostat("ilc_di12", time_format = "num") %>% label_eurostat()
