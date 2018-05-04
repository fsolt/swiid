typeset_pdf <- function(path_to_Rmd) {
  folder_with_Rmd <- stringr::str_replace(path_to_Rmd, "(.*)/.*.Rmd", "\\1")
  file_name_stub <- stringr::str_replace(path_to_Rmd, ".*/(.*).Rmd", "\\1")
  path_to_tex <- paste0(tools::file_path_sans_ext(path_to_Rmd), ".tex")
  path_to_pdf <- paste0(tools::file_path_sans_ext(path_to_Rmd), ".pdf")
  rmarkdown::render(path_to_Rmd)
  tools::texi2pdf(file = path_to_tex, clean = TRUE)
  file.rename(paste0(file_name_stub, ".pdf"), path_to_pdf)
  system(paste0("osascript -e", " 'tell application", ' "Preview" to close (every window whose name contains "', file_name_stub, '")', "'"))
  system(paste0('open "', path_to_pdf, '"'))
}
