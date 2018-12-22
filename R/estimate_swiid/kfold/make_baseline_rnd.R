download.file("https://github.com/fsolt/swiid/raw/master/data/ineq0.rda", "data/ineq0.rda")
source("https://raw.githubusercontent.com/fsolt/swiid/master/R/estimate_swiid/kfold/all_fold.R")
baseline_rnd <- baseline_kfold(3)
save(baseline_rnd, file = "R/estimate_swiid/kfold/baseline_rnd.rda")
