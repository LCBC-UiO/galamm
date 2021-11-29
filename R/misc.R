

linkinv <- function(x) (1 + exp(-x))^(-1)

log_response_prob <- function(mean, y, trials = 1){
  y * log(mean) + (trials - y) * log(1 - mean)
}


initialize_moments <- function(glmod){
  lapply(seq_along(glmod$reTrms$cnms), function(i){
    moments <- NULL
    nm <- names(glmod$reTrms$cnms)[[i]]
    eval(parse(
      text = paste0("moments <- data.frame(", nm,
                    " = levels(glmod$reTrms$flist[[1]]))"
      )))

    # First order moments
    mom1 <- glmod$reTrms$cnms[[i]]
    # Second order moments
    mom2 <- unique(apply(
      expand.grid(glmod$reTrms$cnms[[i]], glmod$reTrms$cnms[[i]]),
      1, function(x) paste(sort(x), collapse = ":")))

    for(x in c(mom1, mom2)){
      if(!grepl("\\:", x) ||
         length(unique(strsplit(x, split = ":", fixed = TRUE)[[1]])) == 2){
        a <- 0
      } else {
        a <- 1
      }

      eval(parse(text = paste0("moments$`", x,
                               "` <- rep(", a, ", nrow(moments))")))
    }

    moments

  })
}
