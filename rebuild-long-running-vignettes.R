old_wd <- getwd()

setwd("vignettes-raw/")
knitr::knit("lmm_factor.Rmd", output = "../vignettes/lmm_factor.Rmd")
knitr::knit("glmm_factor.Rmd", output = "../vignettes/glmm_factor.Rmd")
knitr::knit("mixed_response.Rmd", output = "../vignettes/mixed_response.Rmd")
knitr::knit("lmm_heteroscedastic.Rmd", output = "../vignettes/lmm_heteroscedastic.Rmd")
knitr::knit("semiparametric.Rmd", output = "../vignettes/semiparametric.Rmd")
knitr::knit("optimization.Rmd", output = "../vignettes/optimization.Rmd")

imgs <- list.files(pattern = "\\.png$")
imgs_new <- file.path("..", "vignettes", imgs)
file.rename(imgs, imgs_new)
setwd(old_wd)
