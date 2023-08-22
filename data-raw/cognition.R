# Start by downloading cognition dataset from https://github.com/LCBC-UiO/galamm-scripts/blob/main/data/cognition_model/dat.rds
cognition <- readRDS("~/Downloads/dat.rds")
cognition <- as.data.frame(cognition)
cognition$zeta_3 <- cognition$zeta_2 <- cognition$linpred <-
  cognition$value <- cognition$value_z_orig <- cognition$rn <- NULL

cognition$y <- cognition$value_z
cognition$value_z <- NULL
cognition <- cognition[, c(
  "id", "y", "trials", "itemgroup", "test", "age", "age_z",
  "itemgroup_retest", "retest", "timepoint"
)]
usethis::use_data(cognition, overwrite = TRUE)
