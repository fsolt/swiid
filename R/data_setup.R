library(dplyr)
library(readr)
library(readxl)
library(reshape2)

download.file("http://sedlac.econo.unlp.edu.ar/download.php?file=archivos_estadistica/inequality_LAC_2015-06.xls", "data-raw/sedlac.xls")
sedlac_pc <- read_excel("data-raw/sedlac.xls", sheet = "intervals pci", skip = 8)
