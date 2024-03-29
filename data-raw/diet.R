library(tidyverse)
diet0 <- read.delim("data-raw/diet.dat", header = TRUE, sep = "\t")
diet0$fiber2[diet0$fiber2 == -99] <- NA_real_

diet <- diet0 %>%
  pivot_longer(
    cols = c(fiber1, fiber2, chd),
    values_to = "y", names_to = "item",
    values_drop_na = TRUE
  ) %>%
  mutate(
    chd = as.integer(item == "chd"),
    fiber = 1L - chd,
    fiber2 = as.integer(item == "fiber2"),
    item = factor(item, levels = c("fiber1", "fiber2", "chd"))
  ) %>%
  as.data.frame()

usethis::use_data(diet, overwrite = TRUE)
