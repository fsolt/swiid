typeset_pdf <- function(path_to_Rmd) {
  dir_with_Rmd <- stringr::str_replace(path_to_Rmd, "(.*/).*.Rmd", "\\1")
  orig_path <- getwd()
  setwd(dir_with_Rmd)
  file_name <- stringr::str_replace(path_to_Rmd, dir_with_Rmd, "")
  try(rmarkdown::render(file_name))
  system(paste0("osascript -e", " 'tell application", ' "Preview" to close (every window whose name contains "', file_name_stub, '")', "'"))
  system(paste0('open "', paste0(stringr::str_replace(file_name, ".Rmd", ".pdf")), '"'))
  setwd(orig_path)
}

# typeset_pdf("updating_swiid_paper/updating_swiid.Rmd")