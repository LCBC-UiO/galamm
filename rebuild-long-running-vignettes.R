old_wd <- getwd()

setwd("vignettes/")
knitr::knit("lmm_factor.Rmd.orig", output = "lmm_factor.Rmd")
knitr::knit("glmm_factor.Rmd.orig", output = "glmm_factor.Rmd")
knitr::knit("mixed_response.Rmd.orig", output = "mixed_response.Rmd")
knitr::knit("lmm_heteroscedastic.Rmd.orig", output = "lmm_heteroscedastic.Rmd")
setwd(old_wd)
