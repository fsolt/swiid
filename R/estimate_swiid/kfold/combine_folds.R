folds <- 1:10
# folds <- 11:20
# folds <- 21:31
# folds <- 32:42
# folds <- 43:53

source("R/estimate_swiid/kfold/all_fold.R")
baseline_rnd <- baseline_kfold(6)
walk(folds, function(fold) leave_k_out(fold))


fold_path <- "/Volumes/Platón-Media/Media/Projects/swiid/kfold"
files <- list.files(fold_path, pattern = "res_\\d+.rda")

map_df(files, function(file_name) {
  load(file.path(fold_path, file_name))
  return(gini_res)
})
