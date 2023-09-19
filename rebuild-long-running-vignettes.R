old_wd <- getwd()

setwd("vignettes/")
knitr::knit("lmm_factor.orig.Rmd", output = "lmm_factor.Rmd")
knitr::knit("glmm_factor.orig.Rmd", output = "glmm_factor.Rmd")
knitr::knit("mixed_response.orig.Rmd", output = "mixed_response.Rmd")
knitr::knit("lmm_heteroscedastic.orig.Rmd", output = "lmm_heteroscedastic.Rmd")
knitr::knit("semiparametric.orig.Rmd", output = "semiparametric.Rmd")
setwd(old_wd)
