library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

set.seed(100)
mresp <- tibble(id = 1:1000) %>%
  mutate(b = rnorm(nrow(.))) %>%
  uncount(4, .id = "item") %>%
  mutate(
    x = runif(nrow(.)),
    y = pmap_dbl(
      list(b, item, x), ~ if_else(..2 %in% c(1, 2), rnorm(1, ..3 + ..1),
        as.numeric(rbinom(1, 1, plogis(..3 + ..1)))
      )
    ),
    itemgroup = factor(if_else(item %in% c(1, 2), "a", "b"))
  ) %>%
  select(-b, -item) %>%
  as.data.frame()

usethis::use_data(mresp, overwrite = TRUE)
