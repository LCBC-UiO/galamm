library(tidyverse)
diet0 <- read.delim("http://www.gllamm.org/books/diet.dat", header = TRUE, sep = "\t")
diet0$fiber2[diet0$fiber2 == -99] <- NA_real_

diet <- diet0 %>%
  pivot_longer(
    cols = c(fiber1, fiber2, chd),
    values_to = "y",
    values_drop_na = TRUE) %>%
  mutate(
    d1 = name == "fiber1",
    d2 = name == "fiber2",
    d3 = name == "chd"
  ) %>%
  mutate(across(c(d1, d2, d3), as.integer)) %>%
  select(-name)

usethis::use_data(diet, overwrite = TRUE)
