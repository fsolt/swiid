# folds <- 1:14
# folds <- 15:28
folds <- 29:42
# folds <- 43:57

source("R/estimate_swiid/kfold/all_fold.R")
baseline_rnd <- baseline_kfold(6)
walk(folds, function(fold) leave_k_out(fold))


fold_path <- "/Volumes/Platón-Media/Media/Projects/swiid/kfold"
files <- list.files(fold_path, pattern = "res_\\d+.rda")

res_folds <- map_df(files, function(file_name) {
  load(file.path(fold_path, file_name))
  return(fold_res)
})

save(res_folds, "data/res_folds.rda")
