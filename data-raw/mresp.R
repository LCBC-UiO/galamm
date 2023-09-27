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

set.seed(33)
mresp$grp <- sample(c("a", "b"), size = nrow(mresp), replace = TRUE)
mresp$isgauss <- as.numeric(mresp$itemgroup == "a")
mresp$y <- ifelse(mresp$itemgroup == "a" & mresp$grp == "a",
                  mresp$y + rnorm(nrow(mresp), sd = 5), mresp$y)

mresp_hsced <- mresp
usethis::use_data(mresp_hsced, overwrite = TRUE)
