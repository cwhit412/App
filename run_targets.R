# run_targets.R
# Run this manually whenever tournament_data/ changes

setwd("/Users/caseywhitman/Documents/Baseball R/OOTP/App/")

library(targets)

tar_make(script = "_targets_team_perf.R")