# Download epilepsy data, described at http://www.gllamm.org/books/readme.html#11.3
epilep <- read.delim("http://www.gllamm.org/books/epilep.dat",
  header = TRUE,
  sep = "\t"
)

epilep$cons <- NULL
epilep$id <- NULL
epilep$lbas_trt <- NULL
usethis::use_data(epilep, overwrite = TRUE)
