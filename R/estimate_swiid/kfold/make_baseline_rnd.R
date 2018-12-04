download.file("https://github.com/fsolt/swiid/raw/master/data/ineq0.Rda", "data/ineq0.rda")
source("https://raw.githubusercontent.com/fsolt/swiid/master/R/estimate_swiid/kfold/all_fold.R")
baseline_rnd <- baseline_kfold(6)
save(baseline_rnd, "data/baseline_rnd.rda")